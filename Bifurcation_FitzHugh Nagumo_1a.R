# Numerically solve the FitzHugh Nagumo equations for various values of the parameter I  
# Observe the bifucation structure described in the lecture.

# libraries
library(ggplot2)

# initial variables
I_list<- seq(0, 1, 0.01) 
C1 <- 0.139
C2 <- 0.08
C3 <- 0.02
  
iterations_to_not_plot<- 300 
iterations_to_plot<- 500
iteration_movement <- 0.001
x_list <- c()
y_list <- c()
I_output <- c()
x <- 0.01
y <- 0.01

# loop to create points
for(I in I_list){
  x<- 0.1 # setting x0
  for (i in 1:iterations_to_not_plot) { # run through non printed points to get to equilibrium
    x<- x + (x*(C1 - x)*(x-1) - y - I)*iteration_movement
    y<- y + (C2*x - C3*y)*iteration_movement}
  for (i in 1:iterations_to_plot) { # calculate points to be plotted that should be at equilibrium
    x<- x + (x*(C1 - x)*(x-1) - y - I)*iteration_movement
    y<- y + (C2*x - C3*y)*iteration_movement
    x_list <- c(x_list, x)
    y_list <- c(y_list, y)
    I_output <- c(I_output, I)
  }
}

# create chart
print(ggplot() + geom_point(aes(x = I_output, y = x_list, colour = 'cyl'), size = 0.005, pch = ".")
      + geom_point(aes(x = I_output, y = y_list, colour = 'r'), size = 0.005, pch = ".")+ 
        ggtitle("FitzHugh-Nagumo equations") +
        xlab("I") +
        ylab('X* and Y*'))
        #coord_cartesian(ylim = c(0,1), xlim = c(0, 1)))
