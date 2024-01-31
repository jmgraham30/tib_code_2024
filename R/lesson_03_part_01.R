library(rootSolve)

# define the function
f <- function(x){cos(x) - x}

# find the roots
(f_roots <- uniroot.all(f, c(0,pi/2)))

abs(sin(f_roots))

library(tidyverse)

theme_set(theme_bw(base_size=13))

tibble(x = c(-pi/4,3*pi/4)) |>
  ggplot(aes(x=x)) + 
  geom_function(fun = function(x){x},linewidth=1) + 
  geom_function(fun = function(x){cos(x)},color="steelblue",size=1) + 
  geom_point(data=NULL,aes(x=f_roots,y=cos(f_roots)),color="darkgreen",size=3) + 
  labs(x="x",y="")
