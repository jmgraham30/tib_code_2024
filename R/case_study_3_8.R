library(tidyverse)
library(latex2exp)
theme_set(theme_bw(base_size = 13))

ggplot(data.frame(u = c(0,20)), aes(x = u)) +
  geom_function(fun = function(u){u / (1 + u^2)},color="steelblue",linewidth=1) +
  labs(x = "u", y = "", title = TeX("Plot of the function $f(u) = \\frac{u}{1+u^2}$"))
