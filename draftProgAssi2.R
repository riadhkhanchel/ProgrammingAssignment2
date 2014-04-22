hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
x <-hilbert(4)
inverse.x <- solve(x)

b <- inverse.x %*% x
b
