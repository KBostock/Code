# The Dripping Tap Problem

# install.packages("rootSolve")

# Libraries
library(rootSolve)
library(ggplot2)

# Initial conditions
M0 <- 0.1
v0 <- 0.1
R <- 0.5
A <<- ((M0)^(0.5))*(v0 - 0.66*R - 0.165)
B <<- -0.33*M0  

r_list<- seq(0.06, 0.2, 0.01) 
iterations_to_not_plot<- 20 
iterations_to_plot<- 100
x_list <- c()
y_list <- c()

# Create the function

x_t <-function(t) (A*sin(((1/(M0 + R*t))^0.5)*t) + B*cos(((1/(M0 + R*t))^0.5)*t))*exp((-(0.5 + R)/(M0 +R*t))*t) + (M0 + R*t)*0.33 - 1 

# result for iteration one
root <- uniroot(x_t, c(0,8))$root
x_list <- c(1) # iteration
y_list <- c(root) # time intervals

# results for iterations two onwards
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

# Plot the steady states versus R
print(ggplot() + geom_point(aes(x = x_list, y = y_list), size = 0.005, pch = ".") + 
        ggtitle("Logistic Map") +
        xlab("r") +
        ylab('X*')+
        coord_cartesian(ylim = c(0,1)))




# test to plot the chart and see if the root found is the first one and what is the pattern
# t_list <- seq(0,10,0.5)
# y_list <- x_t(t_list)
# 
# library(ggplot2)
# print((ggplot() +
#          geom_point(aes(x = t_list, y = y_list), colour = 'red') +
#          ggtitle("t v y") +
#          xlab("t") +
#          ylab('Xt+1'))+
#         coord_cartesian(ylim = c(-5,5))) 

# Redundant code:
# AB <- function(M0, v0, R) {
#   A <<- ((M0)^(0.5))*(v0 - 0.66*R - 0.165)
#   B <<- -0.33*M0  
# }