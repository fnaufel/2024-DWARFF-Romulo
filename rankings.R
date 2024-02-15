library(tidyverse)
library(RcppAlgos)
library(stringr)


# Classe rk (ranking) -----------------------------------------------------

# Construtor
new_rk <- function(posicoes_x) {
  
  k <- length(posicoes_x)     # qtde de elementos da lista
  p <- max(posicoes_x)        # o último elemento pertence à lista
  yes <- sort(posicoes_x)     # posições dos elems da lista no ranking
  tmp <- 1:p
  no <- tmp[!(tmp %in% yes)]  # posições do ranking q não são da lista

  structure(
    list(yes = yes, no = no, k = k, p = p), 
    class = 'rk'
  )
  
}

# Validador
validate_rk <- function(string_or_numeric) {
  
  stopifnot(
    '\nArgumento deve ser atômico.' =
      is.atomic(string_or_numeric)
  )
  
  stopifnot(
    '\nVetor não pode ser vazio.' =
      length(string_or_numeric) > 0
  )
  
  # Vetor com posições dos elementos da lista
  if (is.numeric(string_or_numeric)) {
    v <- string_or_numeric
    stopifnot(
      '\nValores precisam ser inteiros positivos, sem repetições.' =
        all(v == as.integer(v)) & 
          all(v > 0) &
          identical(v, unique(v))
    )
    
    return(string_or_numeric)
  }
  
  # Um string contendo apenas 'x' e '-'
  if (is.character(string_or_numeric)) {
    s <- string_or_numeric
    stopifnot(
      '\nArgumento (character) deve ter comprimento 1.' =
        length(s) == 1
    )
    stopifnot(
      '\nArgumento deve conter apenas "x" e "-", terminando com "x".' =
      str_detect(s, '[-x]+') & endsWith(s, 'x')
    )
    ranking <- str_split(s, '')[[1]]
    posicoes_x <- which(ranking == 'x')
    
    return(posicoes_x)
  }

}

# Helper
rk <- function(x) {
  
  new_rk(validate_rk(x))
  
}

# as.character: retorna e.g., "✔••✔"
as.character.rk <- function(x, unicode = FALSE, ...) {

  no <-  ifelse(unicode, '•', '-')
  yes <- ifelse(unicode, '✔', 'x')
  
  s <- rep(no, x$p)
  s[x$yes] <- yes
  
  paste0(s, collapse = '')
  
}

# print: imprime e.g., ranking: [✔••✔] (p = 4, k = 2)
print.rk <- function(x, unicode = FALSE, ...) {
  
  s <- as.character(x, unicode)
  cat(paste0('ranking: [', s, '] (p = ', x$p, ', k = ', x$k, ')'))
  
  invisible(x)
  
}

# pillar_shaft
pillar_shaft <- function(x, ...) {
  
  format(x)
  
}

# format
format.rk <- function(x, ...) {
  
  s <- as.character(x, unicode = FALSE)
  paste0('ranking: [', s, '] (p = ', x$p, ', k = ', x$k, ')')
  
}

# is.rk
is.rk <- function(x) {
  inherits(x, 'rk')
}

# plot
plot.rk <- function(x, y = NULL, fun = NULL, reta = TRUE, ... ) {

  df <- as_tibble(x)
  p <- x$p
  
  grafico <- df %>% 
    ggplot(aes(pos_lista, pos_ranking)) +
      geom_point() +
      scale_x_continuous(breaks = 1:p, labels = 1:p, limits = c(1, p)) +
      scale_y_continuous(breaks = 1:p, labels = 1:p, limits = c(1, p)) +
      labs(
        x = 'lista',
        y = 'ranking',
        title = format(x)
      )
  
  if (!is.null(fun)) {
    score <- do.call(fun, list(x))
    grafico <- grafico + labs(subtitle = paste0('Score = ', score))
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

# as_tibble
as_tibble.rk <- function(x, ...) {
  
  p <- x$p
  k <- x$k
  
  # Linhas da tibble com elementos da lista
  df <- tibble(
    nome = rep('x', k),
    pos_lista = 1:k,
    pos_ranking = x$yes
  )
  
  if (p > k) {
    
    # Linhas da tibble com outros elementos
    df <- df %>% 
      bind_rows(
        tibble(
          nome = rep('-', p - k),
          pos_lista = rep((sum((k+1):p) / (p - k)) , p - k),
          pos_ranking = x$no
        )
      ) %>% 
      arrange(pos_ranking)
      
  }
  
  df
  
}


# Gerar todos os rankings com p e k ---------------------------------------

criar_df_rankings <- function(
    p = NULL, k = NULL, unicode = FALSE, maxp = 2*k
) {

  if (is.null(k) & is.null(p)) {
    stop('\np ou k devem ser especificados.') 
  }
  
  # Se k e p foram especificados
  if (!is.null(k) & !is.null(p)) {
    stopifnot(
      '\np deve ser maior ou igual a k' = p >= k
    )
    return(gerar_df_para_um_k(p, k, unicode))
  }
  
  # Se só p foi especificado, variar k de 1 a p
  if (!is.null(p) ) {
    rv <- 1:p %>% 
      map(
        ~ gerar_df_para_um_k(p, .x, unicode)
      ) %>% 
        bind_rows()
    return(rv)
  }

  # Se só k foi especificado, variar p de 1 a maxp
  if (!is.null(k) ) {
    rv <- k:maxp %>% 
      map(
        ~ gerar_df_para_um_k(.x, k, unicode)
      ) %>% 
        bind_rows()
    return(rv)
  }
    
}


gerar_df_para_um_k <- function(p, k, unicode) {
  
  if (k == 1) {
    return(
      tibble(
        ranking = list(rk(p)),
        ranking_str = as.character(ranking[[1]], unicode = unicode)
      )
    ) # "-...-x"
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
      ranking = list(rk(c(v, p))),
      ranking_str = as.character(ranking, unicode = unicode)
    ) %>% 
    select(ranking, ranking_str) %>% 
    ungroup()

}
