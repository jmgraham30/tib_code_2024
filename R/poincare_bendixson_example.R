library(phaseR)


########## Example 1 ##########
gallery_example_1 <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    
    r <- sqrt(state[1]^2 + state[2]^2)
    if(r == 0){
      dx <- 0.0
      dy <- 0.0
    }else{
      dx <- state[1]*(1 - r^2 + mu * state[1] / r) - state[2]
      dy <- state[2]*(1 - r^2 + mu * state[1] / r) + state[1] 
    }
    
    list(c(dx,dy))
  })
}

nonlinear_flowfield  <- flowField(gallery_example_1,
                                  xlim       = c(-2, 2),
                                  ylim       = c(-2, 2),
                                  parameters = c(mu = 0.2),
                                  points     = 17,
                                  add = FALSE)
nonlinear_nullclines  <- nullclines(gallery_example_1,
                                    xlim       = c(-2, 2),
                                    ylim       = c(-2, 2),
                                    parameters = c(mu = 0.2),
                                    points=100,add.legend=FALSE)

state <- matrix(c(-0.1,-0.1,-0.1,0.2,
                  0.1,-1,0.1,2,
                  0.1,-1,0.1,2,-0.05,-2,
                  0.5,1,0.05,-2,0.5,2,
                  2,1,2,2,-1,2),13,2,byrow = TRUE)
nonlinear_trajs <- trajectory(gallery_example_1,
                              y0         = state,
                              tlim       = c(0, 10),
                              parameters = c(mu = 0.2),add=TRUE)
