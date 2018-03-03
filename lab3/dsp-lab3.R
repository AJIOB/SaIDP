y <- function(x) {
  return (sin(3 * x) + cos(x))
}

custom_plot <- function(x, y, head_title) {
  plot(x, y, type = "l", xlab = "x", ylab = "y", main = head_title)
}

# Dots num
N <- 16

#Period
T <- 2 * pi

#Interval
a <- 0
b <- a + T

# basic function
basic_xdots <- seq(a, b, by = 0.01)
basic_ydots <- y(basic_xdots)
custom_plot(basic_xdots, basic_ydots, "Basic function")

#discrete basic function
xdots <- seq(0, N - 1, by = 1)
basic_xdots <- seq(a, b, length.out = (N + 1))
basic_xdots <- basic_xdots[ basic_xdots < b]
basic_ydots <- y(basic_xdots)
custom_plot(basic_xdots, basic_ydots, "Discrete basic function")

#custom sign
custom_sign <- function(num) {
  return (if (sign(num) > 0) 1 else (-1))
}

#rademacher function
r <- function(k, t) {
  if (t < 0 || t >= 1)
  {
    stop("t must be in interval [0; 1)")
  }
  return (custom_sign(sin((2 ^ k) * pi * t)))
}
