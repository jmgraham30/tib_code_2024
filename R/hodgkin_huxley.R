library(tidyverse)
library(latex2exp)
library(patchwork)
library(ggthemes)
library(deSolve)

theme_set(theme_bw(base_size=12))

alpha_h <- function(v){
  alpha_h<-0.07*exp(-(v+70)/20)  
}


alpha_m <- function(v){
  
  a_m <- ifelse(abs(v+45)>1.0e-8,(v+45)/10./(1-exp(-(v+45)/10)),1)
  
}


alpha_n <- function(v){
  alpha_n<-0.01*(-60.0-v)/(exp((-60-v)/10)-1)  
}


beta_h <- function(v){
  beta_h<-1./(exp(-(v+40)/10)+1)  
}


beta_m <- function(v){
  beta_m<-4*exp(-(v+70)/18)  
}


beta_n <- function(v){
  beta_n<-0.125*exp(-(v+70)/80)  
}

h_inf <- function(v){alpha_h(v)/(alpha_h(v) + beta_h(v))}
m_inf <- function(v){alpha_m(v)/(alpha_m(v) + beta_m(v))}
n_inf <- function(v){alpha_n(v)/(alpha_n(v) + beta_n(v))}

tau_h <- function(v){1/(alpha_h(v) + beta_h(v))}
tau_m <- function(v){1/(alpha_m(v) + beta_m(v))}
tau_n <- function(v){1/(alpha_n(v) + beta_n(v))}

v_df <- tibble(v=c(-100,50))

p_m_inf <- v_df |> ggplot(aes(x=v)) + 
  geom_function(fun=m_inf,linewidth=1,color="steelblue") + 
  ylim(c(0,1)) + 
  xlab("v [mV]") + 
  ylab(TeX(r'($m_{\infty}$)'))

p_m_tau <- v_df |> ggplot(aes(x=v)) + 
  geom_function(fun=tau_m,linewidth=1,color="steelblue") + 
  ylim(c(0,1)) + 
  xlab("v [mV]") + 
  ylab(TeX(r'($\tau_{m}$ (ms))'))

p_h_inf <- v_df |> ggplot(aes(x=v)) + 
  geom_function(fun=h_inf,linewidth=1,color="steelblue") + 
  ylim(c(0,1)) + 
  xlab("v [mV]") + 
  ylab(TeX(r'($h_{\infty}$)'))

p_h_tau <- v_df |> ggplot(aes(x=v)) + 
  geom_function(fun=tau_h,linewidth=1,color="steelblue") + 
  ylim(c(0,10)) + 
  xlab("v [mV]") + 
  ylab(TeX(r'($\tau_{h}$ (ms))'))

p_n_inf <- v_df |> ggplot(aes(x=v)) + 
  geom_function(fun=n_inf,linewidth=1,color="steelblue") + 
  ylim(c(0,1)) + 
  xlab("v [mV]") + 
  ylab(TeX(r'($n_{\infty}$)'))

p_n_tau <- v_df |> ggplot(aes(x=v)) + 
  geom_function(fun=tau_n,linewidth=1,color="steelblue") + 
  ylim(c(0,10)) + 
  xlab("v [mV]") + 
  ylab(TeX(r'($\tau_{n}$ (ms))'))

(p_m_inf + p_m_tau) / (p_h_inf + p_h_tau) / (p_n_inf + p_n_tau)


HH <- function (t, y, parameters) {
  with(as.list(c(y,parameters)),{
    # variables
    v <- y[1]
    h <- y[2]
    m <- y[3]
    n <- y[4]
    
    dv <- (gna*m^3*h*(vna - v) + gk*n^4*(vk - v) + gl*(vl - v) + iext)/C
    dh <- (h_inf(v) - h)/tau_h(v)
    dm <- (m_inf(v) - m)/tau_m(v)
    dn <- (n_inf(v) - n)/tau_n(v)
    
    return(list(c(dv, dh, dm, dn)))
  })
}

# parameters
parameters <- list(C=1,gk=36,gna=120,gl=0.3,vk=-82,vna=45,vl=-59,iext=10)

# initial conditions
v0 <- -50
m0 <- alpha_m(v0)/(alpha_m(v0)+beta_m(v0));

yini <- c(v=v0, h=1, m=m0, n=0.4)

t_initial <- 0
t_final <- 75
times <- seq(from = t_initial, to = t_final, by = 0.04)

out <- ode(y = yini, times = times, func = HH,
           parms = parameters,method = "ode45")

HHsol <- data.frame(t=out[,"time"],v=out[,"v"],h=out[,"h"],m=out[,"m"],n=out[,"n"])


p1 <- HHsol |>
  ggplot(aes(x = t, y = v)) +
  geom_line(aes(x = t, y = v),linewidth=1,color="steelblue") +
  labs(x="time [ms]",y = "v [mV]")

p2 <- HHsol |>
  pivot_longer(-c(t,v),names_to="Variable", values_to="Value") |>
  ggplot(aes(x = t, y = Value, color = Variable)) +
  geom_line(aes(x = t, y = Value),lwd=1) +
  scale_color_colorblind() +
  labs(x="time [ms]",y = " ") +
  guides(color=guide_legend(title="Gating variable"))

p1 / p2

ggplot(HHsol) + geom_path(aes(x = v, y = n, color = t),lwd=1) + 
  scale_color_viridis_c() +
  xlim(c(-100,70)) + ylim(c(0,1)) + xlab("v [mV]") + ylab("n")


