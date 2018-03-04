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
basic_xdots <- custom_seq_interval(a, b, N)
basic_ydots <- y(basic_xdots)
custom_plot(basic_xdots, basic_ydots, "Discrete basic function")

#custom sign
custom_sign <- function(num) {
  ifelse(num > 0, 1, -1)
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
    res <- res * (r(k, t) ^ bitwXor(n_bin[r_num - k + 2], n_bin[r_num - k + 1]))
  }
  
  res
}

custom_is_integer <- function(x) {
  x %% 1 == 0
}

#Discrete Walsh Transform block
# i - non-normilized t value
normilized_wal <- function(n, i, N){
  wal(n, (i + 0.5)/N, log2(N))
}

# i - walsh function number
DWT_one <- function(dots, i) {
  size <- length(dots)
  sum(normilized_wal(i, seq(0, size - 1, by = 1), size) * dots) / sqrt(size)
}

#Discrete Walsh Transform
DWT <- function(dots) {
  size <- length(dots)
  r_num <- log2(size)
  if (!custom_is_integer(r_num)) {
    stop("Size must be an integer power of 2")
  }
  
  buff <- c()
  for (i in seq(0, size - 1, by = 1)) {
    buff <- c(buff, DWT_one(dots, i))
  }
  buff
}

dwt_ydots <- DWT(basic_ydots)
custom_plot(basic_xdots, dwt_ydots, "DWT")

#Inversed DWT
dwt_new_ydots <- DWT(dwt_ydots)
custom_plot(basic_xdots, dwt_new_ydots, "DWT restored function")

# Fast Walsh Transform
FWT_help <- function(dots, i, pow, delta) {
  dots[i] + ((-1) ^ pow) * dots[i + delta]
}

FWT <- function(dots, size) {
  size <- size / 2
  if (size < 1) {
    dots
  }
  else {
    indexes <- seq(1, size, by = 1)
    
    seq1 <- FWT_help(dots, indexes, 0, size)
    seq2 <- FWT_help(dots, indexes, 1, size)
    
    c(FWT(seq1, size), FWT(seq2, size)) / sqrt(2)
  }
}

fwt_ydots <- FWT(basic_ydots, N)
custom_plot(basic_xdots, fwt_ydots, "FWT")

# FWT (Walsh-Hadamard) to DWT converter
index_FWT_to_DWT <- function(n, curr = c(0)) {
  if (n < 2) {
    curr
  }
  else {
    n <- n / 2
    index_FWT_to_DWT(n, c(curr, rev(curr + n)))
  }
}

custom_sort <- function(dots) {
  dots[index_FWT_to_DWT(length(dots)) + 1]
}

custom_plot(basic_xdots, custom_sort(fwt_ydots), "FWT on DWT positions (resorting)")

#Inversed FWT
fwt_new_ydots <- FWT(fwt_ydots, N)
custom_plot(basic_xdots, fwt_new_ydots, "FWT restored function")
