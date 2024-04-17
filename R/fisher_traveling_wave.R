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

s_f <- function(x){
  ifelse(x <= 0.0,0.0,exp(-1/x))
}

s_h <- function(x,r_1 = 1,r_2 = 2){
  return(s_f(r_2 - x) / (s_f(r_2 - x) + s_f(x - r_1)))
}

N <- 1000
xgrid <- setup.grid.1D(x.up = 0, x.down = 30, N = N) 
x <- xgrid$x.mid
D.coeff <- 1.0


Diffusion <- function (t, Y, parms){
  tran <- tran.1D(C = Y, 
                  D = D.coeff, dx = xgrid) 
  list(dY = tran$dC + Y*(1 - Y), flux.up = tran$flux.up, flux.down = tran$flux.down)
}


Yini <- s_h(x, r_1 = 1, r_2 = 2)
times <- seq(from = 0, to = 10, by = 0.01)



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
    xlim(c(0,30)) + ylim(c(0,1))
  
  return(u_dist_plot)
  
}

p_0 <- plot_dist(times[1])
p_1 <- plot_dist(times[300])
p_2 <- plot_dist(times[350])
p_3 <- plot_dist(times[400])
p_4 <- plot_dist(times[450])
p_5 <- plot_dist(times[500])
p_6 <- plot_dist(times[600])
p_7 <- plot_dist(times[700])
p_8 <- plot_dist(times[800])

(final_plot <- (p_0 + p_1 + p_2) / (p_3 + p_4 + p_5) / (p_6 + p_7 + p_8))

df |>
  pivot_longer(cols = starts_with("X"), names_to = "position", values_to = "u") |>
  mutate(time = round(time, 2)) |>
  left_join(tibble(position = paste0("X",1:1000), x = x)) |>
  ggplot(aes(x = x, y = u, color=time)) +
  geom_line(linewidth=1) +
  labs(title = 'Time: {frame_time}', x = 'x', y = 'u(x,t)') +
  transition_time(time) +
  ease_aes('linear')

