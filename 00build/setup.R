# this script loads all the useful packages and sets up 
rm(list = ls())
# table options 
options(knitr.kable.NA = '-')

# Load packages ----------------------------------------------------------------------------------------------
# this function will install the packages used in this this project if they are not present
install_packages_if_necessary <- function(x) {
  if(!require(x, character.only = TRUE)){
    install.packages(x)
    library(x, character.only = TRUE)
  } else {
    library(x, character.only = TRUE)  
  }
}


pckg_name <- c(
  'tidyverse', 'magrittr', 'zoo',
  'furrr',
  'lemon', 'patchwork',
  'kableExtra', 
  'pomp', 'rstan', "deSolve"
)


# load all the packages 
lapply(pckg_name, install_packages_if_necessary)

# plotting defaults 
py_colour_palette_d <- c(
  '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', 
  '#9467bd', '#8c564b', '#e377c2',
  '#7f7f7f', '#bcbd22', '#17becf'
)


# setting the new scale 
scale_colour_discrete <- function(...) {scale_colour_manual(..., values = c(py_colour_palette_d))}
scale_fill_discrete <- function(...) {scale_fill_manual(..., values = c(py_colour_palette_d))}

# change default linewidth
update_geom_defaults('line',   list(linewidth = 0.8))

cap_axes <- function(gap = 1, ...) {coord_capped_cart(bottom='both', left = "none", gap = gap, ...)} 

# define a new new theme for the project
project_theme <- (
  theme_bw(base_size = 11) +
    theme(strip.background = element_blank(),
          panel.border = element_blank(), 
          axis.line = element_line(),
          legend.position = 'bottom',
          legend.background = element_blank(), 
          legend.key = element_blank(),
          axis.ticks.length=unit(.25, 'cm')) 
)




# test plot
# test_theme_data <- tibble(
#   x = 1:100
# ) %>%
#   mutate(
#     f = sin(x),
#     g = tan(x),
#     h = sin(x)+cos(x)
#   ) %>%
#   pivot_longer(cols = !x, names_to = 'functions', values_to = 'value')
# 
# test_theme_data %>%
#   ggplot(aes(x, value, colour = functions))+
#   geom_line()+
#   project_theme+
#   cap_axes()










