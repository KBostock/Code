# Numerically solve the logistic map for various values of the parameter r  
# Observe the bifucation structure described in the lecture.

# libraries
library(ggplot2)

# initial variables
r_list<- seq(2.5, 4, 0.001) 
iterations_to_not_plot<- 500 
iterations_to_plot<- 1000
x_list <- c()
y_list <- c()

# loop to create points
for(r in r_list){
  x<- 0.1 # setting x0
  for (i in 1:iterations_to_not_plot) { # run through non printed points to get to equilibrium
    x<- r*x*(1-x)}
  for (i in 1:iterations_to_plot) { # calculate points to be plotted that should be at equilibrium
    x<- r*x*(1-x)
    x_list <- c(x_list, r)
    y_list <- c(y_list, x)
  }
}

# create chart
print(ggplot() + geom_point(aes(x = x_list, y = y_list), size = 0.005, pch = ".") + 
        ggtitle("Logistic Map") +
        xlab("r") +
        ylab('X*')+
        coord_cartesian(ylim = c(0,1)))
