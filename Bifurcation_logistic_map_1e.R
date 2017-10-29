# Numerically solve the logistic map for various values of the parameters 
# observing the bifucation strcuture described in the lecture.

library(polynom)
library(ggplot2)
r_list <- seq(2,4,0.0025)
logistic_map <- function(x,r) return(r*x*(1 - x))
x_list1 <- c()
y_list1 <- c()
xpoly <- polynomial(c(0,1))

for (r in r_list) {
  # one cycles; steady states
  results1 <- polyroot(c(0,1/r -1,1))
  results_nocomplex1 <- ifelse(Im(results1) == 0 & r > 1, as.numeric(results1), NaN) 
  for (x in results_nocomplex1) {
    x_list1 <- c(x_list1, r)
    y_list1 <- c(y_list1, x)
  }
  
  # two cycles
  poly1 <- polynomial(c(0,r,-r))
  poly2 <<- logistic_map(poly1, r)
  period2poly <- poly2 - xpoly
  results2 <- polyroot(period2poly)
  results2_rounded <- round(results2, 3)

  results_nocomplex2 <- ifelse(Im(results2_rounded) == 0, as.numeric(results2_rounded), NaN)
  for (x in results_nocomplex2) {
    x_list1 <- c(x_list1, r)
    y_list1 <- c(y_list1, x)
  }  
  # three or more cycles
  for (i in c(3,4)) {
    if (i == 3) {
      poly_previous <- poly2
      }
    else {
      poly_previous <- poly_current
    }

    poly_current <- logistic_map(poly_previous, r)
    period_k_poly <- poly_current - xpoly
    results_k <- polyroot(period_k_poly)
    results_k_rounded <- round(results_k, 3)
    
    results_nocomplex_k <- ifelse(Im(results_k_rounded) == 0, as.numeric(results_k_rounded), NaN)
    for (x in results_nocomplex_k) {
      if (!(is.nan(x))) {
        x_list1 <- c(x_list1, r)
        y_list1 <- c(y_list1, x)
      }
    }
  }  
}
# Print results
print(ggplot() + geom_point(aes(x = x_list1, y = y_list1), size = 0.05) + 
          ggtitle("Logistic Map") +
          xlab("r") +
          ylab('X*')+
          coord_cartesian(ylim = c(-1,1)))
  
