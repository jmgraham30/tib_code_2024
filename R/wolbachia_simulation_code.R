library(ggplot2)
library(tibble)

theme_set(theme_minimal(base_size = 13))
update_geom_defaults("point", list(color = "midnightblue", alpha = 0.8))

infection_frequency_step <- function(p_t,F_val,mu_val,sh_val){
  
  num_exp <- p_t*F_val*(1-mu_val)
  den_exp <- 1 + p_t*(F_val - 1 - sh_val) + (p_t^2)*sh_val*(1 - mu_val*F_val)
  
  p_t1 <- num_exp/den_exp
  
  return(max(0.0,min(p_t1,1.0)))
  
}


turelli_simulation <- function(p_0,F_val,
                               sh_val,mu_val,
                               max_iter=500, thresh = 10^(-8)){
  
  gen_i <- 0
  
  while (gen_i < max_iter & p_0 > thresh){
    p_0 <- infection_frequency_step(p_0,F_val=F_val,
                             sh_val=sh_val,mu_val=mu_val)
    gen_i <- gen_i + 1
  }
  
  return(gen_i)
  
}

turelli_simulation_p <- function(p_0,F_val,
                                 sh_val,mu_val,
                                 max_iter=500, thresh = 10^(-8)){
  
  gen_i <- 1
  p <- c(p_0)
  
  while (gen_i < max_iter & p_0 > thresh){
    p_0 <- infection_frequency_step(p_0,F_val=F_val,
                             sh_val=sh_val,mu_val=mu_val)
    gen_i <- gen_i + 1
    p[gen_i] <- p_0 
  }
  
  return(p)
  
}


p_0 <- 0.4
F_val <- 0.5 #1.05

mu_val <- 0.2 #0.02
sh_val <- 0.0 #0.1

F_val * (1 - mu_val)

F_val * mu_val

p_f <- turelli_simulation(p_0,F_val,sh_val,mu_val)
p_f

p <- turelli_simulation_p(p_0,F_val,sh_val,mu_val)

tibble(p=p) |>
  ggplot(aes(x=1:length(p),y=p)) +
  geom_point() +
  ylim(c(0,1)) + 
  labs(x = "Generation", y = "Frequency of infection")

