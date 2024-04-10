library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(deSolve)
library(rootSolve)
library(ReacTran)
library(patchwork)
library(gganimate)

theme_set(theme_minimal(base_size = 11))

N <- 100
xgrid <- setup.grid.1D(x.up = 0, x.down = 1, N = N) 
x <- xgrid$x.mid
D.coeff <- 0.01

Diffusion <- function (t, Y, parms){
  tran <- tran.1D(C = Y, 
                  C.up = 0, C.down = 0,
                  flux.up = NULL, flux.down = NULL,
                  D = D.coeff, dx = xgrid) 
  list(dY = tran$dC, flux.up = tran$flux.up, flux.down = tran$flux.down)
}

Yini <- sin(6*pi*x)
times <- seq(from = 0, to = 2, by = 0.01)

out <- ode.1D(y = Yini, times = times, 
              func = Diffusion, 
              parms = NULL, dimens = N)

image(out, grid = x, mfrow = NULL, ylab = "Distance, x", main = "Y") 


df <- out |> 
  data.frame() |> 
  tibble() |>
  select(-c(flux.up, flux.down)) 

p <- ggplot(data = NULL,aes(x=x))

plot_dist <- function(t_val, domain = x, p_val = p){
  
  U_vals <- df |>
    filter(time == t_val) |>
    select(starts_with("X")) |>
    unlist()
  
  u_dist_plot <- p_val +
    geom_line(data=tibble(x_domain = domain, u_vals = U_vals),mapping=aes(x=x_domain,y=u_vals),linewidth=1) +
    labs(title = paste0("t = ", t_val), x = "x", y = "u(x,t)") + 
    xlim(c(0,1)) + ylim(c(-1,1))
  
  return(u_dist_plot)
  
}

p_0 <- plot_dist(times[1])
p_1 <- plot_dist(times[10])
p_2 <- plot_dist(times[20])
p_3 <- plot_dist(times[40])
p_4 <- plot_dist(times[60])
p_5 <- plot_dist(times[80])
p_6 <- plot_dist(times[100])
p_7 <- plot_dist(times[150])
p_8 <- plot_dist(times[201])

(final_plot <- (p_0 + p_1 + p_2) / (p_3 + p_4 + p_5) / (p_6 + p_7 + p_8))

df |>
  pivot_longer(cols = starts_with("X"), names_to = "position", values_to = "u") |>
  mutate(time = round(time, 2)) |>
  left_join(tibble(position = paste0("X",1:100), x = x)) |>
  ggplot(aes(x = x, y = u, color=time)) +
  geom_line(linewidth=1) +
  labs(title = 'Time: {frame_time}', x = 'x', y = 'u(x,t)') +
  transition_time(time) +
  ease_aes('linear')

#############################################################################
N <- 100
xgrid <- setup.grid.1D(x.up = 0, x.down = 1, N = N) 
x <- xgrid$x.mid
D.coeff <- 0.25


Diffusion <- function (t, Y, parms){
  tran <- tran.1D(C = Y, 
                  C.down = 1,
                  flux.up = 0,
                  D = D.coeff, dx = xgrid) 
  list(dY = tran$dC, flux.up = tran$flux.up, flux.down = tran$flux.down)
}

Yini <- x^2
times <- seq(from = 0, to = 4, by = 0.01)

out <- ode.1D(y = Yini, times = times, 
              func = Diffusion, 
              parms = NULL, dimens = N)

image(out, grid = x, mfrow = NULL, ylab = "Distance, x", main = "Y") 


df <- out |> 
  data.frame() |> 
  tibble() |>
  select(-c(flux.up, flux.down)) 

p <- ggplot(data = NULL,aes(x=x))

plot_dist <- function(t_val, domain = x, p_val = p){
  
  U_vals <- df |>
    filter(time == t_val) |>
    select(starts_with("X")) |>
    unlist()
  
  u_dist_plot <- p_val +
    geom_line(data=tibble(x_domain = domain, u_vals = U_vals),mapping=aes(x=x_domain,y=u_vals),linewidth=1) +
    labs(title = paste0("t = ", t_val), x = "x", y = "u(x,t)") + 
    xlim(c(0,1)) + ylim(c(0,1))
  
  return(u_dist_plot)
  
}

p_0 <- plot_dist(times[1])
p_1 <- plot_dist(times[10])
p_2 <- plot_dist(times[20])
p_3 <- plot_dist(times[40])
p_4 <- plot_dist(times[60])
p_5 <- plot_dist(times[80])
p_6 <- plot_dist(times[100])
p_7 <- plot_dist(times[150])
p_8 <- plot_dist(times[201])

(final_plot <- (p_0 + p_1 + p_2) / (p_3 + p_4 + p_5) / (p_6 + p_7 + p_8))

df |>
  pivot_longer(cols = starts_with("X"), names_to = "position", values_to = "u") |>
  mutate(time = round(time, 2)) |>
  left_join(tibble(position = paste0("X",1:100), x = x)) |>
  ggplot(aes(x = x, y = u, color=time)) +
  geom_line(linewidth=1) +
  labs(title = 'Time: {frame_time}', x = 'x', y = 'u(x,t)') +
  transition_time(time) +
  ease_aes('linear')

##############################################################################

N <- 100
xgrid <- setup.grid.1D(x.up = 0, x.down = 1, N = N) 
x <- xgrid$x.mid
D.coeff <- 0.01

Diffusion <- function (t, Y, parms){
  tran <- tran.1D(C = Y, 
                  flux.up = 0, flux.down = 0,
                  D = D.coeff, dx = xgrid) 
  list(dY = tran$dC, flux.up = tran$flux.up, flux.down = tran$flux.down)
}


Yini <- 2 + cos(6*pi*x)
times <- seq(from = 0, to = 2, by = 0.01)

out <- ode.1D(y = Yini, times = times, 
              func = Diffusion, 
              parms = NULL, dimens = N)

image(out, grid = x, mfrow = NULL, ylab = "Distance, x", main = "Y") 


df <- out |> 
  data.frame() |> 
  tibble() |>
  select(-c(flux.up, flux.down)) 

p <- ggplot(data = NULL,aes(x=x))

plot_dist <- function(t_val, domain = x, p_val = p){
  
  U_vals <- df |>
    filter(time == t_val) |>
    select(starts_with("X")) |>
    unlist()
  
  u_dist_plot <- p_val +
    geom_line(data=tibble(x_domain = domain, u_vals = U_vals),mapping=aes(x=x_domain,y=u_vals),linewidth=1) +
    labs(title = paste0("t = ", t_val), x = "x", y = "u(x,t)") + 
    xlim(c(0,1)) + ylim(c(1,3))
  
  return(u_dist_plot)
  
}

p_0 <- plot_dist(times[1])
p_1 <- plot_dist(times[10])
p_2 <- plot_dist(times[20])
p_3 <- plot_dist(times[40])
p_4 <- plot_dist(times[60])
p_5 <- plot_dist(times[80])
p_6 <- plot_dist(times[100])
p_7 <- plot_dist(times[150])
p_8 <- plot_dist(times[201])

(final_plot <- (p_0 + p_1 + p_2) / (p_3 + p_4 + p_5) / (p_6 + p_7 + p_8))

df |>
  pivot_longer(cols = starts_with("X"), names_to = "position", values_to = "u") |>
  mutate(time = round(time, 2)) |>
  left_join(tibble(position = paste0("X",1:100), x = x)) |>
  ggplot(aes(x = x, y = u, color=time)) +
  geom_line(linewidth=1) +
  labs(title = 'Time: {frame_time}', x = 'x', y = 'u(x,t)') +
  transition_time(time) +
  ease_aes('linear')
