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
  geom_point(x=0.0,y=0.0,color="green",size=2) +
  geom_point(x=0.0,y=cos(0.0),color="orange",size=2) +
  geom_segment(x=0.0,xend=0.0,y=0.0,yend=cos(0.0),linetype="dashed") +
  geom_point(x=cos(0.0),y=cos(0.0),color="orange",size=2) + 
  geom_segment(x=0.0,xend=cos(0.0),y=cos(0.0),yend=cos(0.0),linetype="dashed") +
  geom_point(x=cos(0.0),y=cos(cos(0.0)),color="orange",size=2) +
  geom_segment(x=cos(0.0),xend=cos(0.0),y=cos(0.0),yend=cos(cos(0.0)),linetype="dashed") +
  geom_point(x=cos(cos(0.0)),y=cos(cos(0.0)),color="orange",size=2) + 
  geom_segment(x=cos(0.0),xend=cos(cos(0.0)),y=cos(cos(0.0)),yend=cos(cos(0.0)),linetype="dashed") +
  geom_point(x=cos(cos(0.0)),y=cos(cos(cos(0.0))),color="orange",size=2) +
  geom_segment(x=cos(cos(0.0)),xend=cos(cos(0.0)),y=cos(cos(0.0)),yend=cos(cos(cos(0.0))),linetype="dashed") +
  labs(x="x",y="")


x_t <- numeric(10)
x_t[1] <- 0.0
for (i in 2:10){
  x_t[i] <- cos(x_t[i-1])
}

tibble(x = 1:10, x_t = x_t) |>
  ggplot(aes(x=x,y=x_t)) + 
  geom_point(color="steelblue",size=2) + 
  geom_line(color="steelblue",size=1) + 
  labs(x="t",y="x_t")



