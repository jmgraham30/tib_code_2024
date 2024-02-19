#### LOAD LIBRARIES ####

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(igraph)
library(ggplot2)
library(tibble)

theme_set(theme_minimal(base_size = 13))
update_geom_defaults("point", list(color = "midnightblue", alpha = 0.8))

#### REUSABLE BITS ####

infection_frequency_step <- function(p_t,F_val,mu_val,sh_val){
  
  num_exp <- p_t*F_val*(1-mu_val)
  den_exp <- 1 + p_t*(F_val - 1 - sh_val) + (p_t^2)*sh_val*(1 - mu_val*F_val)
  
  p_t1 <- num_exp/den_exp
  
  return(max(0.0,min(p_t1,1.0)))
  
}