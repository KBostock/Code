# Numerically solve the logistic map for various values of the parameters 
# observing the bifucation strcuture described in the lecture.

logistic_map <- function(x,r) return(r*x*(1 - x))

# Set parameter regions to review

x_list <- seq(0, 1, 0.01)

library(ggplot2)

# Plotting for varying r
for (r in r_list) {
  y_list <- logistic_map(x_list, r)
  title_text = paste("At r =", r)
  print((ggplot() +
    geom_point(aes(x = x_list, y = y_list), colour = 'red') +
    ggtitle(title_text) +
    xlab("Xt") +
    ylab('Xt+1'))+
    coord_cartesian(ylim = c(-2,2))) 
}
# Plot all on the same chart
df = as.data.frame(x_list)
for (r in r_list) {
  df <- cbind(df, logistic_map(x_list, r))
}
colnames(df) <- c("x", r_list)      

ggplot(df) + 
  geom_point(x = x, y = value, group = col)
        

# Plotting versus time
for (r in r_list) {
  time_list <- seq(0,100,1)
  title_text = paste("At r =", r)
  x0 = 0.5
  result_list = c(x0)
  iterations <- length(time_list)-1
  for (n in 1:iterations) {
   x = result_list[length(result_list)]
   result_list <- c(result_list, logistic_map(x, r))
  }
  print(ggplot() + geom_line(aes(x = time_list, y = result_list)) + 
    ggtitle(title_text) +
    xlab("Time") +
    ylab('Xt')+
    coord_cartesian(ylim = c(0,1))) }

# creating a bifurcation diagram
# 1. need to plot steady states for each value of r
# 2. need to plot 2 period states for each value of r, then higher order period states
# 3. need to plot if they are stable or unstable



# x_list <- c("-1")
# y_list <- c("-1")
# 
# results <- polyroot(c(0,1,1))
# results_nocomplex <- ifelse(Im(results) == 0, as.numeric(results), NaN) 
#                     
# for (x in as.numeric(polyroot(c(10,1,1)))) {
#   ifelse(is.complex(x), print(paste(x, "complex")), print(paste(x, "not complex")))
#   ifelse(is.complex(x), y_list <- y_list, ifelse(y_list == c("-1"), y_list <- c(x), y_list <- c(y_list, x)))
#   
# }
# print(ggplot() + geom_line(aes(x = x_list, y = y_list)) + 
#         ggtitle("Logistic Map") +
#         xlab("r") +
#         ylab('X*')+
#         coord_cartesian(ylim = c(0,1)))

library(polynom)
library(ggplot2)
r_list <- seq(0.25,4,0.1)
logistic_map <- function(x,r) return(r*x*(1 - x))
x_list1 <- c()
y_list1 <- c()
x_list2 <- c()
y_list2 <- c()

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
  poly2 <- logistic_map(poly1, r)
  xpoly <- polynomial(c(0,1))
  period2poly <- poly2 - xpoly
  results2 <- polyroot(period2poly)
  results2_rounded <- round(results2, 3)

  results_nocomplex2 <- ifelse(Im(results2_rounded) == 0, as.numeric(results2_rounded), NaN)
  for (x in results_nocomplex2) {
    x_list2 <- c(x_list2, r)
    y_list2 <- c(y_list2, x)
  }
}
# Print results
print(ggplot() + geom_point(aes(x = x_list1, y = y_list1, colour = "red")) + 
          geom_point(aes(x = x_list2, y = y_list2, colour = "blue")) +
          ggtitle("Logistic Map") +
          xlab("r") +
          ylab('X*')+
          coord_cartesian(ylim = c(-1,1)))

# poly1 <- polynomial(c(0,1,-1))
# poly2 <- logistic_map(poly1, 1)
# xpoly <- polynomial(c(0,1))
# period2poly <- poly2 - xpoly  
#   
  
