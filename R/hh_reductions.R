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
HH_parameters <- list(C=1,gk=36,gna=120,gl=0.3,vk=-82,vna=45,vl=-59,iext=10)


# time points
t_initial <- 0
t_final <- 75
times <- seq(from = t_initial, to = t_final, by = 0.04)

# initial conditions
v0 <- -50
m0 <- alpha_m(v0)/(alpha_m(v0)+beta_m(v0))
yini <- c(v=v0, h=1, m=m0, n=0.4)

# numerical solution
out <- ode(y = yini, times = times, func = HH,
           parms = HH_parameters,method = "ode45")

HHsol <- data.frame(t=out[,"time"],v=out[,"v"],h=out[,"h"],m=out[,"m"],n=out[,"n"])

# plot
HHsol |> mutate(h_plus_n=h+n) |> filter(t >= 20) |>
  ggplot(aes(x=t,y=h_plus_n)) + geom_line(linewidth=1,color="steelblue") + 
  geom_hline(yintercept=0.83,linetype="dashed") + ylim(c(0,1)) +
  labs(x="time [ms]",y = "h+n") +
  theme_bw() + theme(text=element_text(size=15))

## Reduction of the Hodgkin-Huxley model
HH_reduced <- function (t, y, parameters) {
  with(as.list(c(y,parameters)),{
    # variables
    v <- y[1]
    n <- y[2]
    
    dv <- (gna*m_inf(v)^3*(0.83-n)*(vna - v) + gk*n^4*(vk - v) + gl*(vl - v) + iext)/C
    dn <- (n_inf(v) - n)/tau_n(v)
    
    return(list(c(dv, dn)))
  })
}

HH_v_null <- function(v,n,parameters){
  with(as.list(c(v,n,parameters)),{
    
    rhs <- gna*m_inf(v)^3*(0.83-n)*(vna - v) + gk*n^4*(vk - v) + gl*(vl - v) + iext
    
    return(rhs)
    
  })
}

compute_v_null <- function(v_val){
  if (v_val < -81.52){
    n_val <- 1.0
  }else if(v_val >= -81.52 && v_val <= 44.78 ){
    f <- function(n){HH_v_null(n,v=v_val,parameters=HH_parameters)}
    n_val <- uniroot(f,interval=c(0,1))$root 
  }else {
    n_val <- 0.0
  }
  
  return(n_val)
  
}

# initial conditions
yini <- c(v=-55, n=0.0)

# numerical solution
out <- ode(y = yini, times = times, func = HH_reduced,
           parms = HH_parameters,method = "ode45")

HHsol_reduced <- data.frame(t=out[,"time"],v=out[,"v"],n=out[,"n"])

v_nulls <- map_dbl(HHsol_reduced$v,compute_v_null)
n_nulls <- n_inf(HHsol_reduced$v)

HHsol_reduced <- HHsol_reduced %>% mutate(v_nulls=v_nulls,n_nulls=n_nulls)

ggplot(HHsol_reduced) + geom_line(aes(x=v,y=v_nulls),color="darkgreen",lwd=1) +
  annotate(geom="text", x=58, y=0.25, label="v nullcline",
           color="darkgreen") + 
  geom_line(aes(x=v,y=n_nulls),color="purple",lwd=1) +
  annotate(geom="text", x=50, y=0.9, label="n nullcline",
           color="purple") + 
  xlim(c(-100,70)) + ylim(c(0,1)) +
  xlab("v [mV]") + ylab("n") +
  theme_bw() + theme(text=element_text(size=15))

ggplot(HHsol_reduced) + geom_line(aes(x=v,y=v_nulls),color="darkgreen",lwd=1) +
  geom_line(aes(x=v,y=n_nulls),color="purple",lwd=1) +
  geom_path(aes(x = v, y = n),linetype="dashed",lwd=1) + 
  xlim(c(-100,70)) + ylim(c(0,1)) +
  xlab("v [mV]") + ylab("n") +
  theme_bw() + theme(text=element_text(size=15))


p1 <- ggplot(HHsol_reduced) + geom_line(aes(x=t,y=v),lwd=1) +
  ylim(c(-100,70)) +
  xlab("time [ms]") + ylab("v [mV]") +
  theme_bw() + theme(text=element_text(size=15))
p2 <- ggplot(HHsol_reduced) + geom_line(aes(x=t,y=n),lwd=1) +
  ylim(c(0,1)) +
  xlab("time [ms]") + ylab("n") +
  theme_bw() + theme(text=element_text(size=15))

p1 + p2
