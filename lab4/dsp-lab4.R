# Made by IvAlex

y <- function(x) {
  sin(3 * x) + cos(x)
}

custom_plot <- function(x, y, head_title) {
  plot(x, y, type = "l", xlab = "x", ylab = "y", main = head_title)
  abline(h = 0, col = "gray60")
}

custom_freq_plot <- function(x, y, head_title) {
  plot(x, y, type = "h", xlab = "x", ylab = "y", main = head_title)
  abline(h = 0, col = "gray60")
}

# Dots num
N <- 16

#Period
T <- 2 * pi

#Interval
a <- 0
b <- a + T

# 
custom_seq_interval = function(from, before, num_intervals){
  res <- seq(from, before, length.out = (num_intervals + 1))
  res[-length(res)]
}

# basic function
basic_xdots <- seq(a, b, by = 0.01)
basic_ydots <- y(basic_xdots)
custom_plot(basic_xdots, basic_ydots, "Basic function")

#discrete basic function
basic_xdots <- custom_seq_interval(a, b, N)
basic_ydots <- y(basic_xdots)
custom_plot(basic_xdots, basic_ydots, "Discrete basic function")

hamming_a <- 25/46
hamming_b <- 21/46
hidden_hamming_window_ <- function(n, N){
  hamming_a - hamming_b * cos(2 * pi * n / (N - 1))
}

# Hamming Window
hamming_window <- function(dots){
  N <- length(dots)
  res <- hidden_hamming_window_(dots, N)
  res
}

hamming_ydots <- hamming_window(basic_ydots)
custom_plot(basic_xdots, hamming_ydots, "Discrete basic function after Hamming Window modification")
