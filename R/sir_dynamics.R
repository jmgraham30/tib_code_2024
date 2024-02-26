library(tidyverse)
library(deSolve)
library(patchwork)
library(ggthemes)

theme_set(theme_bw())

sir_model <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    dx <- -R0*x*y
    dy <- R0*x*y - y
    
    list(c(dx,dy))
  })
}

state <- c(x=0.90,y=0.1)

times <- seq(0,12,by=0.1)

sim1 <- as.data.frame(ode(y=state,times=times,func=sir_model,parms=c(R0=1.0)))

sim2 <- as.data.frame(ode(y=state,times=times,func=sir_model,parms=c(R0=5.6)))

p1 <- sim1 |> 
  rename(susceptible=x,infected=y) |>
  pivot_longer(cols=c(susceptible,infected),names_to="state",values_to="population") |>
  ggplot(aes(x=time,y=population,color=state)) + 
  geom_line(lwd=1) + 
  scale_color_colorblind() +
  ylim(c(0,1)) + 
  labs(x="Time",y="Popultion proportion",
       color = "State",
       title="Susceptibles persist") 

p2 <- sim2 |> 
  rename(susceptible=x,infected=y) |> 
  pivot_longer(cols=c(susceptible,infected),names_to="state",values_to="population") |>
  ggplot(aes(x=time,y=population,color=state)) + 
  geom_line(lwd=1) + 
  scale_color_colorblind() +
  ylim(c(0,1)) +
  labs(x="Time",y="Popultion proportion",
       color = "State",
       title="Susceptibles diminish") 

(p1 / p2)
