library(phaseR)

rhs <- function(t, u, parms) {
  list(3 * u^2 - u^3)
}

phase_line_plot <- phasePortrait(rhs,
                                        ylim   = c(-2.0, 5.0),
                                        points = 10,
                                        frac   = 0.5,
                                        state.names = "u")

slope_field <- flowField(rhs,
                           xlim       = c(0, 3),
                           ylim       = c(-2.5, 5.0),
                           parameters = NULL,
                           points     = 15,
                           system     = "one.dim",add=FALSE,
                           state.names = "u")

numerical_solutions <- trajectory(rhs,
                                 y0   = c(-1.0,0.0,0.5,1.5,3.0,4.5,5.0),
                                 tlim=c(0,3),
                                 system = "one.dim")
