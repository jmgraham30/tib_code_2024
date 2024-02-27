library(tidyverse)
library(deSolve)
library(phaseR)
library(ggquiver)

# set the plot theme
theme_set(theme_bw(base_size = 13))


colors <- c("dx/dt=0" = "#551A8BFF", "dy/dt=0" = "#FF7F00FF")

mu_val <- 1.0

lv_vf <- expand.grid(x=seq(0,4,0.15), y=seq(0,3,0.15)) |>
  ggplot(aes(x=x,y=y)) +
  geom_quiver(aes(u=x - x*y,v= x*y - mu_val*y),color="darkgray") + 
  geom_hline(aes(color="dx/dt=0",yintercept = 0.0),linetype = "dashed") +
  geom_vline(aes(color="dx/dt=0",xintercept = 1.0),linetype = "dashed") +
  geom_vline(aes(color="dy/dt=0",xintercept = 0.0),linetype = "dashed") +
  geom_hline(aes(color="dy/dt=0",yintercept = 1.0),linetype = "dashed") +
  labs(x = "x",
       y = "y",
       color = "Null-cline") +
  scale_color_manual(values = colors)

lv_vf

lotka_volterra_rhs <- function(t, y, parms) {
  x <- y[1]
  y <- y[2]
  mu <- parms[1]
  dx <- x - x*y
  dy <- x*y - mu*y
  return(list(c(dx, dy)))
}

parms <- c(mu = 1.0)

y0 <- c(x = 0.5, y = 0.5)

times <- seq(0, 25, by = 0.1)

lotka_volterra_sol <- ode(y = y0, times = times, func = lotka_volterra_rhs, parms = parms)

lotka_volterra_sol_df <- as.data.frame(lotka_volterra_sol)

lotka_volterra_sol_df |>
  ggplot(aes(x = time)) +
  geom_line(aes(y = x, color = "Prey"),linewidth=1) +
  geom_line(aes(y = y, color = "Predator"),linewidth=1) +
  labs(x = "Time",
       y = "Population",
       color = "Species") +
  scale_color_manual(values = c("Prey" = "steelblue", "Predator" = "purple"))

lv_vf + 
  geom_path(data = lotka_volterra_sol_df, aes(x = x, y = y, color = "Solution"), linewidth = 1, color="black") 


system_p_flowField  <- flowField(lotka_volterra_rhs,
                                 xlim       = c(0, 4),
                                 ylim       = c(0, 3),
                                 parameters = c(mu=mu_val),
                                 points     = 19,
                                 add = FALSE)
system_p_nullclines <- nullclines(lotka_volterra_rhs,
                                  xlim       = c(0, 4),
                                  ylim       = c(0, 3),
                                  parameters = c(mu=mu_val),
                                  points     = 500)
state                        <- matrix(c(0.5,0.5,0.4,0.7,0.4,0.9),
                                       3, 2, byrow = TRUE)
system_p_trajectory <- trajectory(lotka_volterra_rhs,
                                  y0         = state,
                                  tlim       = c(0, 25),
                                  parameters = c(mu=mu_val),add=TRUE)
