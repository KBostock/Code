# Numerical solving of Fibonacci's rabbits with death after d generations

d <- 6
x_list <- c(1,1)
y_list <- 1:100
for (i in 3:100) {
  if (i - 2 - d < 1) {
    new_x <- x_list[i - 2] + x_list[i - 1]}  
  else {
    new_x <- x_list[i - 2] - x_list[i - 2 - d] + x_list[i - 1] - x_list[i-1-d]}  
  x_list <- c(x_list, new_x)
  }

title <- paste("Rabbits with death after", d, "generations")
print(ggplot() + geom_point(aes(x = x_list, y = y_list), size = 0.005, pch = ".") + 
        ggtitle(title) +
        xlab("r") +
        ylab('X*')+
        coord_cartesian(ylim = c(0,1)))