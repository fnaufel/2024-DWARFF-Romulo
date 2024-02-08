
# knitr stuff -------------------------------------------------------------

library(knitr)

opts_chunk$set(
  collapse = FALSE,
  cache = FALSE,
  out.width = "100%",
  fig.align = 'center',
  fig.width = 7,
  fig.show = "hold"
)

# To center the results of a chunk (image, video etc.)
# Usage:
#         out.extra=center()
#
center <- function(width = '80%'){

  if (is_html_output()) {
    paste0(
      'class="center" style="width: ', width, ';"'
    )
  }

}

# To embed YT videos in HTML and the link (centered) in LaTeX
embed_yt <- function(code) {

  if (is_html_output()) {
    include_url(
      paste0(
        'https://www.youtube.com/embed/',
        code
      )
    )
  } else {
    cat(
      paste0(
        '```{=latex}\n',
        '\\begin{center} \\url{https://youtu.be/',
        code,
        '} \\end{center}\n',
        '```'
      )
    )
  }

}

# Options -----------------------------------------------------------------

options(dplyr.print_min = 6, dplyr.print_max = 6)
options(crayon.enabled = FALSE) # Supress crayon output
options(
  # Avoid scientific notation
  scipen = 15,
  # Use a comma as decimal separator
  OutDec = ',',
  # Number of decimal digits for numbers produced by inline R code
  fmdigits = 2,
  # Width of text output
  width = ifelse(is_html_output(), 72, 69),
  # Number of head elements to show in
  str = strOptions(vec.len = 3)
)

# Format numbers ----------------------------------------------------------

# Format a number with thousand separators (default point)
# and decimal comma enclosed in curly braces for LaTeX printing.
# CAREFUL: if called outside math mode, will print the braces!
fm <- function(
  x,
  digits = getOption('fmdigits', default = 3),
  big = '.',
  decimal = '{,}',
  ...
) {
  if (!is.numeric(x)) {
    x
  } else {
    if (any(x != floor(x))) {
      # floating point
      formatC(
        x,
        big.mark = big,
        decimal.mark = decimal,
        digits = digits,
        format = 'f',
        ...
      )
    } else {
      # no decimal point
      formatC(
        x,
        big.mark = big,
        decimal.mark = decimal,
        format = 'f',  # must be f to handle values outside integer range
        digits = 0,
        ...
      )
    }
  }
}

# Set this as a hook for inline R code
knitr::knit_hooks$set(inline = fm)

# summarytools ------------------------------------------------------------

library(summarytools)
st_options(
  plain.ascii = FALSE,
  dfSummary.style = 'grid',
  dfSummary.varnumbers = FALSE,
  dfSummary.valid.col = FALSE,
  dfSummary.graph.col = is_html_output(),
  headings = FALSE,
  dfSummary.graph.magnif = 1,
  tmp.img.dir = 'dfimg',
  lang = 'pt',
  subtitle.emphasis = FALSE
)

# Misc libraries ----------------------------------------------------------

library(janitor)
library(latex2exp)
library(kableExtra)
options(knitr.kable.NA = '')
library(conflicted)

# Tidyverse ---------------------------------------------------------------

library(tidyverse)
conflict_prefer("view", "tibble")
conflict_prefer("filter", "dplyr")

# Text size in plots
plot_text_size = ifelse(is_html_output(), 11, 13)

# Sober theme for ggplot
theme_set(
  theme_linedraw() +                         # Set simple theme for ggplot
    theme(                                   # with some tweaks
      text = element_text(size = plot_text_size),
      axis.title.y.left = element_text(
         angle = 0,                          # Never rotate y axis title
         margin = margin(r = 20),            # Separate y axis title a little
         vjust = .5                          # Leave y axis title in the middle
      ),
      axis.title.y.right = element_text(
         angle = 0,                          # Never rotate y axis title
         margin = margin(l = 20),            # Separate y axis title a little
         vjust = .5                          # Leave y axis title in the middle
      ),
      axis.ticks.y.right = element_blank(),  # No ticks on secondary y axis
      axis.title.x.bottom = element_text(
         margin = margin(t = 20)             # Separate x axis title a little
      ),
      axis.line = element_blank(),           # No axis lines
      panel.border = element_blank(),        # No frame
      panel.grid.minor = element_blank()     # No grid minor lines
    )
)

# Tidymodels --------------------------------------------------------------

library(tidymodels)
tidymodels_prefer()

# Destructuring assignment ------------------------------------------------

library(zeallot)

# Links to docs: LEAVE HERE AT THE END ------------------------------------

# Add package names to downlit options so that functions from these packages
# are correctly linked to the docs in the HTML output
library(sessioninfo)

packages <- session_info('attached')$packages %>%
  pull(package)

options(downlit.attached = packages)

# End setup ---------------------------------------------------------------
