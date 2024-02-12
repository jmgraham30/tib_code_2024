library(tidyverse)
library(latex2exp)
library(rootSolve)
theme_set(theme_bw(base_size = 13))

c_val <- 10
ggplot(data.frame(u = c(0,20)), aes(x = u)) +
  geom_function(fun = function(u){u / (1 + u^2)},color="steelblue",linewidth=1) +
  geom_function(fun = function(u){0.1*(1 - u/c_val)},color="darkgreen",linetype="dashed",linewidth=1) +
  geom_function(fun = function(u){0.25*(1 - u/c_val)},color="darkorange",linetype="dashed",linewidth=1) +
  geom_function(fun = function(u){0.38*(1 - u/c_val)},color="darkorchid",linetype="twodash",linewidth=1) +
  geom_function(fun = function(u){0.45*(1 - u/c_val)},color="deepskyblue",linetype="dashed",linewidth=1) +
  geom_function(fun = function(u){0.56*(1 - u/c_val)},color="darkorchid",linetype="twodash",linewidth=1) +
  geom_function(fun = function(u){0.7*(1 - u/c_val)},color="brown",linetype="dashed",linewidth=1) +
  labs(x = "u", y = "", title = TeX("Plot of $\\frac{u}{1+u^2}$ and $h(1 - \\frac{u}{c})$")) + 
  ylim(c(-0.05,0.75))

ggplot(data.frame(u = c(0,5)), aes(x = u)) +
  geom_function(fun = function(u){2*c_val * u^3 - c_val^2*u^2 + c_val^2},color="steelblue",linewidth=1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
  labs(x = "u", y = "", title = TeX("Plot of $2c u^3 - c^2 u^2 + c^2$"))

# define the function
f <- function(u){2*c_val * u^3 - c_val^2*u^2 + c_val^2}

# find the roots
(f_roots <- uniroot.all(f, c(0,5)))

h <- function(u){u / ((1+u^2)*(1 - u/c_val))}

h(f_roots[1])
h(f_roots[2])
