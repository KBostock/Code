# Numerically solve the logistic map for various values of the parameters 
# observing the bifucation strcuture described in the lecture.

logistic_map <- function(x,r) return(r*x*(1 - x))

# Set parameter regions to review
r_list <- seq(0.5,4,0.25) # 4 * 2 = 8 iterations
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