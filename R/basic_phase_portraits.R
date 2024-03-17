# Load packages
library(phaseR)


################### Linear Systems ###################

# Define general linear system
lin_sys <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    
    dx <- A %*% state
    
    list(dx)
  })
}

#### Example 1
A <- matrix(c(-2,-1,2,-5),2,2,byrow=TRUE)

# Compute values example 1
(sum(diag(A)))
(det(A))
(eigen(A)$values)

# Obtain phase portrait example 1
parms_mat <- list(A=A)
linear_flowField  <- flowField(lin_sys,
                               xlim       = c(-2, 2),
                               ylim       = c(-2, 2),
                               parameters = parms_mat,
                               points     = 19,
                               add = FALSE)
state                        <- matrix(c(1,1,1,-1,-1,1,-1,-1,0,2,0,-2,-1,0,1,0,1,2,-1,-2),
                                       10, 2, byrow = TRUE)
linear_trajectory <- trajectory(lin_sys,
                                y0         = state,
                                tlim       = c(0, 10),
                                parameters = parms_mat,
                                add=TRUE)

#### Example 2
A <- matrix(c(2,3,1,0),2,2,byrow=TRUE)

# Compute values example 2
(sum(diag(A)))
(det(A))
(eigen(A)$values)

# Obtain phase portrait example 2
parms_mat <- list(A=A)
linear_flowField  <- flowField(lin_sys,
                               xlim       = c(-2, 2),
                               ylim       = c(-2, 2),
                               parameters = parms_mat,
                               points     = 19,
                               add = FALSE)
state                        <- matrix(c(0.09487,0.03162,-0.09487,-0.03162,-0.7071,0.7071,0.7071,-0.7071,
                                         -1,1.8,-1.75,0.5,1,-1.5,1,-0.7),
                                       8, 2, byrow = TRUE)
linear_trajectory <- trajectory(lin_sys,
                                y0         = state,
                                tlim       = c(0, 10),
                                parameters = parms_mat,
                                add=TRUE)

#### Example 3
A <- matrix(c(-2,-3,3,-2),2,2,byrow=TRUE)

# Compute values example 3
(sum(diag(A)))
(det(A))
(eigen(A)$values)

# Obtain phase portrait example 3
parms_mat <- list(A=A)
linear_flowField  <- flowField(lin_sys,
                               xlim       = c(-2, 2),
                               ylim       = c(-2, 2),
                               parameters = parms_mat,
                               points     = 19,
                               add = FALSE)
state                        <- matrix(c(0.09487,0.03162,-0.09487,-0.03162,-0.7071,0.7071,0.7071,-0.7071,
                                         -1,1.8,-1.75,0.5,1,-1.5,1,-0.7),
                                       8, 2, byrow = TRUE)
linear_trajectory <- trajectory(lin_sys,
                                y0         = state,
                                tlim       = c(0, 10),
                                parameters = parms_mat,
                                add=TRUE)

#### Example 4
A <- matrix(c(-3,10,-1,3),2,2,byrow=TRUE)

# Compute values example 4
(sum(diag(A)))
(det(A))
(eigen(A)$values)

# Obtain phase portrait example 4
parms_mat <- list(A=A)
linear_flowField  <- flowField(lin_sys,
                               xlim       = c(-5, 5),
                               ylim       = c(-4, 4),
                               parameters = parms_mat,
                               points     = 19,
                               add = FALSE)
state                        <- matrix(c(0.1,0.1,0.5,0.5,1,1,2,2),
                                       4, 2, byrow = TRUE)
linear_trajectory <- trajectory(lin_sys,
                                y0         = state,
                                tlim       = c(0, 10),
                                parameters = parms_mat,
                                add=TRUE)

#### Example 5
A <- matrix(c(-3,-1,-1,2,-1,3,4,-1,-2),3,3,byrow=TRUE)

# Compute values example 5
(sum(diag(A)))
(det(A))
(eigen(A)$values)


######################## Nonlinear Systems ###################

#### Example 1
gallery_example_1 <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    
    dx <- state[1]*(1 - 0.5*state[1] - state[2])
    dy <- state[2]*(state[1] - 1 - 0.5*state[2])
    
    list(c(dx,dy))
  })
}

nonlinear_flowfield  <- flowField(gallery_example_1,
                                  xlim       = c(-3, 3),
                                  ylim       = c(-3, 3),
                                  parameters = NULL,
                                  points     = 17,
                                  add = FALSE)
nonlinear_nullclines  <- nullclines(gallery_example_1,
                                    xlim       = c(-3, 3),
                                    ylim       = c(-3, 3),
                                    points=100,add.legend=FALSE)
eq1 <- findEquilibrium(gallery_example_1, y0 = c(0,0),
                       plot.it = TRUE,summary=FALSE)
eq2 <- findEquilibrium(gallery_example_1, y0 = c(2,0),
                       plot.it = TRUE,summary=FALSE)
eq3 <- findEquilibrium(gallery_example_1, y0 = c(0,-2),
                       plot.it = TRUE,summary=FALSE)
eq4 <- findEquilibrium(gallery_example_1, y0 = c(6/5,2/5),
                       plot.it = TRUE,summary=FALSE)
state <- matrix(c(-0.1,-1,-0.1,2,
                  0.01,-1,0.01,2,
                  0.1,-1,0.1,2,-0.05,-2,
                  0.5,1,0.05,-2,0.5,2,
                  2,1,2,2,-1,2),13,2,byrow = TRUE)
nonlinear_trajs <- trajectory(gallery_example_1,
                              y0         = state,
                              tlim       = c(0, 10),
                              parameters = NULL,add=TRUE)

#### Example 2
gallery_example_2 <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    
    dx <- state[2]
    dy <- state[1]*(1 - state[1]^2) + state[2]
    
    list(c(dx,dy))
  })
}

nonlinear_flowfield  <- flowField(gallery_example_2,
                                  xlim       = c(-3, 3),
                                  ylim       = c(-3, 3),
                                  parameters = NULL,
                                  points     = 17,
                                  add = FALSE)
nonlinear_nullclines  <- nullclines(gallery_example_2,
                                    xlim       = c(-3, 3),
                                    ylim       = c(-3, 3),
                                    points=100,add.legend=FALSE)
eq1 <- findEquilibrium(gallery_example_2, y0 = c(0,0),
                       plot.it = TRUE,summary=FALSE)
eq2 <- findEquilibrium(gallery_example_2, y0 = c(1,0),
                       plot.it = TRUE,summary=FALSE)
eq3 <- findEquilibrium(gallery_example_2, y0 = c(-1,0),
                       plot.it = TRUE,summary=FALSE)
state <- matrix(c(0.01,0.01,0.01,-0.01,-0.01,0.01,-0.01,-0.01,
                  -0.5,0.0,0.5,0.0,
                  -1,-0.1,-1,0.1,1,-0.1,1,0.1),10,2,byrow = TRUE)
nonlinear_trajs <- trajectory(gallery_example_2,
                              y0         = state,
                              tlim       = c(0, 10),
                              parameters = NULL,add=TRUE)

#### Example 3
gallery_example_3 <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    
    dx <- state[2]
    dy <- -state[1] + (1 - state[1]^2) * state[2]
    
    list(c(dx,dy))
  })
}

nonlinear_flowfield  <- flowField(gallery_example_3,
                                  xlim       = c(-3, 3),
                                  ylim       = c(-3, 3),
                                  parameters = NULL,
                                  points     = 17,
                                  add = FALSE)
nonlinear_nullclines  <- nullclines(gallery_example_3,
                                    xlim       = c(-3, 3),
                                    ylim       = c(-3, 3),
                                    points=100,add.legend=FALSE)
eq1 <- findEquilibrium(gallery_example_3, y0 = c(0,0),
                       plot.it = TRUE,summary=FALSE)
state <- matrix(c(0.01,0.01,0,2.5,2.5,-2.5),3,2,byrow = TRUE)
nonlinear_trajs <- trajectory(gallery_example_3,
                              y0         = state,
                              tlim       = c(0, 50),
                              parameters = NULL,add=TRUE)


