# The Dripping Tap Problem

# install.packages("rootSolve")
# install.packages("Deriv")

# Libraries
library(rootSolve)
library(ggplot2)
library(Deriv)

# Initial conditions
M0 <- 0.1
v0 <- 0.1
x0 <- 0

k <- 1
b <- 0.5
p <- 1
a <- 12
g <- 0.33
xc <- 1

r_list<- seq(0.04, 0.2, 0.01) 
iterations_to_not_plot<- 20 
iterations_to_plot<- 50

x_list <- c()
y_list <- c()

# results for iterations
for(R in r_list){
  A <<- ((M0/k)^(0.5))*(v0 - 2*g*R/k +(b+R)*x0/M0 - g*b/k)
  B <<- x0 -g*M0/k 
  x_t <-function(t) (A*sin(((k/(M0 + R*t))^0.5)*t) + B*cos(((k/(M0 + R*t))^0.5)*t))*exp((-(b)/(M0 +R*t))*t) + (M0 + R*t)*g/k - xc 
  
  for (i in 1:iterations_to_not_plot) { # run through non printed points to get to equilibrium
    # find solution for current iteration
    t_value <- uniroot(x_t, c(0,200))$root
    
    #find velocity at that point
    diff_x_t <-Deriv(x_t)
    Vc <- diff_x_t(t_value)
  
    # find mass at that point
    Mc <- M0 + R*t_value
  
    # reset initial values and function
    M0 <-(1 - a*Vc)*Mc
    v0 <- Vc
    delta_M <- a*Mc*Vc
    x0 <- xc - ((3*delta_M/(4*pi*p))^(1/3))*delta_M/Mc
    A <<- ((M0/k)^(0.5))*(v0 - 2*g*R/k +(b+R)*x0/M0 - g*b/k)
    B <<- x0 -g*M0/k 
    x_t <-function(t) (A*sin(((k/(M0 + R*t))^0.5)*t) + B*cos(((k/(M0 + R*t))^0.5)*t))*exp((-(b)/(M0 +R*t))*t) + (M0 + R*t)*g/k - xc 
  }
  for (i in 1:iterations_to_plot) { # calculate points to be plotted that should be at equilibrium
    # find solution for current iteration
    t_value <- uniroot(x_t, c(0,100))$root
    
    #find velocity at that point
    diff_x_t <-Deriv(x_t)
    Vc <- diff_x_t(t_value)
    
    # find mass at that point
    Mc <- M0 + R*t_value
    
    # reset initial values and function
    M0 <-(1 - a*Vc)*Mc
    v0 <- Vc
    delta_M <- a*Mc*Vc
    x0 <- xc - ((3*delta_M/(4*pi*p))^(1/3))*delta_M/Mc
    A <<- ((M0/k)^(0.5))*(v0 - 2*g*R/k +(b+R)*x0/M0 - g*b/k)
    B <<- x0 -g*M0/k 
    x_t <-function(t) (A*sin(((k/(M0 + R*t))^0.5)*t) + B*cos(((k/(M0 + R*t))^0.5)*t))*exp((-(b)/(M0 +R*t))*t) + (M0 + R*t)*g/k - xc 
    
    x_list <- c(x_list, R)
    y_list <- c(y_list, t_value)
    print(paste("r value at", R))
  }
}

# Plot the steady states versus R
print(ggplot() + geom_point(aes(x = x_list, y = y_list), size = 0.001, pch = ".") + 
        ggtitle("Leaky tap bifucation") +
        xlab("Rate of flow = R") +
        ylab('Time interval between drops = t')+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)))
       
