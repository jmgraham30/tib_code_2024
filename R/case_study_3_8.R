library(tidyverse)
library(latex2exp)
theme_set(theme_bw(base_size = 13))

c_val <- 10
ggplot(data.frame(u = c(0,20)), aes(x = u)) +
  geom_function(fun = function(u){u / (1 + u^2)},color="steelblue",linewidth=1) +
  geom_function(fun = function(u){0.1*(1 - u/c_val)},color="green",linetype="dashed",linewidth=1) +
  geom_function(fun = function(u){0.25*(1 - u/c_val)},color="green",linetype="dashed",linewidth=1) +
  geom_function(fun = function(u){0.45*(1 - u/c_val)},color="green",linetype="dashed",linewidth=1) +
  geom_function(fun = function(u){0.56*(1 - u/c_val)},color="green",linetype="dashed",linewidth=1) +
  geom_function(fun = function(u){0.7*(1 - u/c_val)},color="green",linetype="dashed",linewidth=1) +
  labs(x = "u", y = "", title = TeX("Plot of $\\frac{u}{1+u^2}$ and $h(1 - \\frac{u}{c})$")) + 
  ylim(c(-0.05,0.75))
