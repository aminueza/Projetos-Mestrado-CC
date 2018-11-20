theme_report <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         ...) {
    ret <- ggplot2::theme_minimal(base_family = "Roboto-Regular",
                                  base_size = base_size, ...)
    ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                            margin=margin(b=strip_text_margin),
                                            family="Roboto-Bold")
    ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                               margin=margin(b=subtitle_margin),
                                               family="PT Sans")
    ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                            family="Oswald")
    ret
}

import_data <- function(){
    library(readr)
    library(tidyverse)
    cacc_raw = read_csv("https://raw.githubusercontent.com/nazareno/ciencia-de-dados-1/master/3-Agrupamento-e-PCA/dados/capes-cacc.csv", col_types = "") %>% 
        mutate_all(funs(replace(., is.na(.), 0))) %>% 
        filter(Teses + Dissertacoes > 0)
    
    cacc_raw %>% 
        write_csv(here::here("data/cacc.csv"))
}

read_projectdata <- function(){
    readr::read_csv(here::here("data/cacc.csv"), 
                    col_types = cols(
                        .default = col_integer(),
                        Instituição = col_character(),
                        Programa = col_character(),
                        Sigla = col_character(),
                        `Tem doutorado` = col_character(),
                        `Docentes colaboradores` = col_double(),
                        `Docentes permanentes` = col_double(),
                        `Docentes visitantes` = col_double()
                    ))
}
