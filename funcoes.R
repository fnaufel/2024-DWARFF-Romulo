library(tidyverse)
library(RcppAlgos)



# Criar ranking a partir de vetor numérico com posições -------------------

rk <- function(v) {
  
  k <- length(v)
  p <- max(v)   # o último elemento pertence à lista
  
  # Verificar se posicoes contêm só números entre 1 e p, sem repetições
  stopifnot(
    'Valores precisam ser inteiros positivos, sem repetições.' =
    all(v == as.integer(v)) & 
      all(v > 0) &
      identical(v, unique(v))
  )

  s <- rep('-', p)
  s[v] <- 'x'
  
  paste0(s, collapse = '')
  
}



# Criar tibble representando um ranking -----------------------------------

criar_df <- function(ranking) {
  
  stopifnot(
    'Argumento deve conter apenas "x" e "-", terminando com "x".' =
    str_detect(ranking, '[-x]+') & endsWith(ranking, 'x')
  )
  
  # Separar caracteres. 
  # De agora em diante, é vetor:
  ranking <- str_split(ranking, '')[[1]]
  
  p <- length(ranking)
  lista <- ranking[ranking == 'x']
  k <- length(lista)
  pos_lista <- 1:k
  pos_ranking <- which(ranking %in% lista)
  
  # Linhas da tibble com elementos da lista
  df <- tibble(
    nome = lista,
    pos_lista = pos_lista,
    pos_ranking = pos_ranking
  )
  
  if (p > k) {
    
    # Linhas da tibble com outros elementos
    nomes <- rep('-', p - k)
    pos_lista <- rep((sum((k+1):p) / (p - k)) , p - k)
    pos_ranking <- which(!(ranking %in% lista))
    
    df <- df %>% 
      bind_rows(
        tibble(
          nome = nomes,
          pos_lista = pos_lista,
          pos_ranking = pos_ranking
        )
      ) %>% 
      arrange(pos_ranking)
      
  }
  
  df
  
}


# Converter ranking de tibble para string ---------------------------------

df_string <- function(df) {
  
  df$nome %>% paste0(collapse = '')
  
}


# Plotar um ranking -------------------------------------------------------

criar_plot <- function(ranking, fun = NULL, reta = TRUE) {
  
  if (!is_tibble(ranking)) {
    ranking <- criar_df(ranking)
  }
  
  df <- ranking
  p <- nrow(df)
  
  grafico <- df %>% 
    ggplot(aes(pos_lista, pos_ranking)) +
      geom_point() +
      scale_x_continuous(breaks = 1:p, labels = 1:p, limits = c(1, p)) +
      scale_y_continuous(breaks = 1:p, labels = 1:p, limits = c(1, p)) +
      labs(
        x = 'lista',
        y = 'ranking'
      )
  
  if (!is.null(fun)) {
    score <- do.call(fun, list(df))
    grafico <- grafico + labs(title = paste0('Score = ', score))
  }
  
  if (reta) {
    grafico <- grafico +
      geom_smooth(
        formula = y ~ x,
        method = 'lm',
        se = FALSE
      )
  }
  
  grafico

}


# Gerar todos os rankings com p e k ---------------------------------------

criar_df_rankings <- function(p, k = NULL) {
  
  if (!is.null(k)) {
    stopifnot(
      'p deve ser maior ou igual a k' = p >= k
    )
  }
  
  # Se k foi especificado, usar
  if (!is.null(k)) {
    rv <- gerar_df_para_um_k(p, k)
  } else {
    # Senão, gerar com todos os valores de k entre 1 e p
    rv <- 1:p %>% 
      map(
        ~ gerar_df_para_um_k(p, .x)
      ) %>% 
        bind_rows()
  }
  
  rv
  
}


gerar_df_para_um_k <- function(p, k) {
  
  if (k == 1) {
    return(tibble(ranking = rk(p))) # "-...-x"
  }
    
  # Escolher k-1 posições dentre p-1 para conter x
  df <- suppressMessages(
    as_tibble(
      comboGeneral(p - 1, k - 1),
      .name_repair = 'universal'
    )
  )
  
  df %>% 
    rowwise() %>% 
    mutate(
      v = list(c_across(everything())),
      # Acrescentar 'x' na posição p (última posição de todo ranking)
      ranking = rk(c(v, p))
    ) %>% 
    select(ranking) %>% 
    ungroup()
    
}
