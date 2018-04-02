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
  res <- dots * hidden_hamming_window_(seq(0, N - 1), N)
  res
}

hamming_ydots <- hamming_window(basic_ydots)
custom_plot(basic_xdots, hamming_ydots, "Discrete basic function after Hamming Window modification")

#Fourier transform
## Sorting indexes generator
sort_index_gen <- function (arr, k){
  if (k < 1) {
    arr
  } else {
    sort_index_gen(c(arr, (k + arr)), k / 2);
  }
}
  
#БПФ с прореживанием по частоте
fft_butterfly <- function(arr, dir){
  n <- length(arr)
  w <- 1
  left <- NULL
  right <- NULL
  Wn <- exp(((-1) ^ dir) *2 * 1i * pi / n)
  if (length(arr) < 2) {
    return(arr)
  }
  
  for(index in seq(1, n/2, by = 1)) {
    left <- c(left, arr[index]+arr[index+(n/2)]) 
    right <- c(right, (arr[index]-arr[index+(n/2)])*w)
    w <- w*Wn
  }
  
  c(fft_butterfly(left, dir), fft_butterfly(right, dir))
}

custom_fft <- function(dots){
  N <- length(dots)
  fft_butterfly(dots, 0)[sort_index_gen(c(0), N / 2) + 1]/N
}

custom_ifft <- function(idots){
  N <- length(idots)
  fft_butterfly(idots, 1)[sort_index_gen(c(0), N / 2) + 1]
}

fft_ydots <- custom_fft(basic_ydots)
custom_freq_plot(basic_xdots, Mod(fft_ydots), "FFT")

ifft_ydots <- custom_ifft(fft_ydots)
custom_plot(basic_xdots, Re(ifft_ydots), "Inversed FFT")
