library(phaseR)
library(plotly)



########## Example 1 ##########
gallery_example_1 <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    
    dx <- -state[2]
    dy <- (state[1]^3 - state[1])
    
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
eq2 <- findEquilibrium(gallery_example_1, y0 = c(1,0),
                       plot.it = TRUE,summary=FALSE)
eq3 <- findEquilibrium(gallery_example_1, y0 = c(-1,0),
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

X <- seq(-2,2,by=0.1)
Y <- seq(-2,2,by=0.1)
my_fi <- function(x,y) {0.5 * x^4 - x^2 + y^2}
Z <- outer(X,Y,my_fi)

plot_ly(x = X, y = Y, z = t(Z), type = "contour", 
        colorscale = 'Jet',
        autocontour = F,
        contours = list(
          start = 0,
          end = 8,
          size = 1))



########## Example 2 ##########
gallery_example_2 <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    
    dx <- state[2]^3
    dy <- -state[1]^3
    
    list(c(dx,dy))
  })
}

nonlinear_flowfield  <- flowField(gallery_example_2,
                                  xlim       = c(-1, 1),
                                  ylim       = c(-1, 1),
                                  parameters = NULL,
                                  points     = 17,
                                  add = FALSE)
nonlinear_nullclines  <- nullclines(gallery_example_2,
                                    xlim       = c(-1, 1),
                                    ylim       = c(-1, 1),
                                    points=100,add.legend=FALSE)
eq1 <- findEquilibrium(gallery_example_2, y0 = c(0,0),
                       plot.it = TRUE,summary=FALSE)

state <- matrix(c(0.2,0,0.5,0,0.8,0),3,2,byrow = TRUE)
nonlinear_trajs <- trajectory(gallery_example_2,
                              y0         = state,
                              tlim       = c(0, 250),
                              parameters = NULL,add=TRUE)

X <- seq(-1,1,by=0.01)
Y <- seq(-1,1,by=0.01)
my_fi <- function(x,y) {1/4 * (x^4 + y^4)}
Z <- outer(X,Y,my_fi)

plot_ly(x = X, y = Y, z = t(Z), type = "contour", 
        colorscale = 'Jet',
        autocontour = F,
        contours = list(
          start = 0,
          end = 0.25,
          size = 0.01))


########## Example 3 ##########
gallery_example_3 <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    
    dx <- 2 - state[2]
    dy <- 2 * state[1]^3
    
    list(c(dx,dy))
  })
}

nonlinear_flowfield  <- flowField(gallery_example_3,
                                  xlim       = c(-3, 3),
                                  ylim       = c(0, 4),
                                  parameters = NULL,
                                  points     = 17,
                                  add = FALSE)
nonlinear_nullclines  <- nullclines(gallery_example_3,
                                    xlim       = c(-3, 3),
                                    ylim       = c(0, 4),
                                    points=100,add.legend=FALSE)
eq1 <- findEquilibrium(gallery_example_3, y0 = c(0,2),
                       plot.it = TRUE,summary=FALSE)


state <- matrix(c(0.1,2,0.5,2,1,2,1.2,2),4,2,byrow = TRUE)
nonlinear_trajs <- trajectory(gallery_example_3,
                              y0         = state,
                              tlim       = c(0, 15),
                              parameters = NULL,add=TRUE)

X <- seq(-3,3,by=0.1)
Y <- seq(-1,5,by=0.1)
my_fi <- function(x,y) {0.5 * x^4 + 0.5 * y^2 - 2 * y}
Z <- outer(X,Y,my_fi)

plot_ly(x = X, y = Y, z = t(Z), type = "contour", 
        colorscale = 'Jet',
        autocontour = F,
        contours = list(
          start = 0,
          end = 8,
          size = 0.5))

