library(ggplot2)
library(fMultivar)

# Parameters
k = 5

# Generate data
n_data <- 50
x1 <- rnorm(n = n_data, mean = 0) - 0
x2 <- rnorm(n = n_data, mean = 0) + 5
x <- as.data.frame(cbind(x1, x2))
qplot(data = x, x = x$x1, y = x$x2) +
    scale_x_continuous(limits = c(-15,15)) +
    scale_y_continuous(limits = c(-15,15))

# Visualize
