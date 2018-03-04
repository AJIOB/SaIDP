# Made by IvAlex

y <- function(x) {
  sin(3 * x) + cos(x)
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
normilized_xdots <- custom_seq_interval(0, 1, N)
basic_xdots <- custom_seq_interval(a, b, N)
basic_ydots <- y(basic_xdots)
custom_plot(normilized_xdots, basic_ydots, "Discrete basic function")

#custom sign
custom_sign <- function(num) {
  return (if (sign(num) > 0) 1 else (-1))
}

#Rademacher function
r <- function(k, t) {
  if (t < 0 || t >= 1)
  {
    stop("t must be in interval [0; 1)")
  }
  custom_sign(sin((2 ^ k) * pi * t))
}

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  binary_vector[-(1:(length(binary_vector) - noBits))]
}

#Walsh function
wal <- function(n, t, r_num) {
  n_bin <- c(0, number2binary(n, r_num))
  res <- 1
  
  for(k in seq(1, r_num, by = 1)) {
    # because array indexes from 1
    if (xor(n_bin[r_num - k + 2], n_bin[r_num - k + 1])) {
      res <- res * r(k, t)
    }
  }
  
  res
}

#Test (temp)
wal(6, 0.9, 3)

FWT <- function(dots, size) {
  if (size >= 4) {
    indexes <- seq(1, size / 2, by = 1)
    
    seq1 <- c()
    seq2 <- c()
    
    for(i in indexes) {
      seq1 <- c(seq1, (dots[i + size / 2] + dots[i]))
      seq2 <- c(seq2, (dots[i] - dots[i + size / 2]))
    }
    res <- c(FWT(seq1, size / 2), FWT(seq2, size / 2))
    res
  } else {
    dots
  }
}

res <- FWT(basic_ydots, N)
custom_plot(basic_xdots, res, "FWT")