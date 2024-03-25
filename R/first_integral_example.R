library(phaseR)
library(plotly)

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

