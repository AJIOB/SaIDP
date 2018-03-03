y <- function(x) {
  return (sin(3 * x) + cos(x))
}

custom_plot <- function(x, y, head_title) {
  plot.new()
  plot(x, y, type = "l", xlab = "x", ylab = "y", main = head_title)
}

N <- 16
T <- 2 * pi

a <- 0
b <- a + T

basic_xdots <- seq(a, b, by = 0.01)
basic_ydots <- y(basic_xdots)
custom_plot(basic_xdots, basic_ydots, "Basic function")

xdots <- seq(0, N - 1, by = 1)
