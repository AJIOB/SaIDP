# Made by IvAlex

library("foreach")
library("numbers")

y <- function(x) {
  sin(3 * x) + cos(x) + sin(6 * x + 3) + 1 + cos (15 * x) + sin (14 * x) + cos (13 * x) +
    cos (12 * x) + cos (11 * x) + cos (10 * x) + cos (9 * x) + cos (8 * x) + cos (7 * x) + 
    cos (5 * x) + cos (4 * x) + cos (2 * x)
}

custom_plot <- function(x, y, head_title) {
  plot(x, y, type = "l", xlab = "x", ylab = "y", main = head_title)
  abline(h = 0, col = "gray60")
}

custom_freq_plot <- function(x, y, head_title) {
  len <- length(x)
  n_right <- div(len + 1, 2)
  new_x <- c(x[seq(1, n_right)], (x[seq(n_right + 1, len)] - len))
  plot(new_x, y, type = "h", xlab = "x", ylab = "y", main = head_title)
  abline(h = 0, col = "gray60")
}

# Dots num
N <- 32

#Period
T <- 2 * pi

#Interval
a <- - T / 2
b <- a + T

#ideal finite impulse responce filter borders
left_filter_fir <- 3
right_filter_fir <- 9

# 
custom_seq_interval = function(from, before, num_intervals){
  interval_prefix_len <- (before - from) / num_intervals
  res <- seq(from + (interval_prefix_len / 2), before, by = interval_prefix_len)
  res
}

# basic function
basic_xdots <- seq(a, b, by = 0.01)
basic_ydots <- y(basic_xdots)
custom_plot(basic_xdots, basic_ydots, "Basic function")

#discrete basic function
basic_xdots <- custom_seq_interval(a, b, N)
basic_freq_xdots <- seq(0, N - 1)
basic_ydots <- y(basic_xdots)
custom_plot(basic_xdots, basic_ydots, "Discrete basic function")

hamming_a <- 25/46
hamming_b <- 1 - hamming_a
hidden_hamming_window_ <- function(n, N){
  hamming_a - hamming_b * cos(2 * pi * (n - N / 2) / (N - 1))
}

# Hamming Window
hamming_window <- function(dots){
  N_local <- length(dots)
  window_xdots <- seq(0, N_local - 1)
  window_ydots <- hidden_hamming_window_(window_xdots, N_local)
  custom_freq_plot(window_xdots, window_ydots, "Hamming window example")
  res <- dots * window_ydots
  res
}

#Fourier transform
## Sorting indexes generator
sort_index_gen <- function (arr, k){
  if (k < 1) {
    arr
  } else {
    sort_index_gen(c(arr, (k + arr)), k / 2);
  }
}
  
# Frequency decimation FFT
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

custom_cyclic_convolution <- function(y1_dots, y2_dots){
  y1_idots <- custom_fft(y1_dots)
  y2_idots <- custom_fft(y2_dots)
  y_idots <- y1_idots * y2_idots
  custom_ifft(y_idots)
}

# FIR Filter
ideal_filter_resolver <- function(dot){
  res <- foreach(x = dot) %dopar% {
    if (x > div(N + 1, 2)){
      x <- N - x
    }
    if ((x >= left_filter_fir) & (x <= right_filter_fir)) {
      1
    }
    else {
      0
    }
  }
  unlist(res)
}

## FIR ideal calcs
fir_filter <- list(ideal = list(freq = ideal_filter_resolver(basic_freq_xdots)))
fir_filter$ideal$time <- custom_ifft(fir_filter$ideal$freq)

custom_freq_plot(basic_freq_xdots, Mod(fir_filter$ideal$freq), "FIR ideal frequency (Amplitude)")
custom_plot(basic_xdots, Re(fir_filter$ideal$time), "FIR ideal timing")

ideal_conv_ydots <- custom_cyclic_convolution(basic_ydots, fir_filter$ideal$time)
custom_plot(basic_xdots, Re(ideal_conv_ydots), "Ideal FIR signal convolution")

## FIR window calcs
fir_filter$real <- list(time = hamming_window(fir_filter$ideal$time))
fir_filter$real$freq <- custom_fft(fir_filter$real$time)

custom_freq_plot(basic_freq_xdots, Mod(fir_filter$real$freq), "FIR real frequency (Amplitude)")
custom_plot(basic_xdots, Re(fir_filter$real$time), "FIR real timing")

real_conv_ydots <- custom_cyclic_convolution(basic_ydots, fir_filter$real$time)
custom_plot(basic_xdots, Re(real_conv_ydots), "Real FIR signal convolution")

iir_AJIOB <- function(){
  iir_b1 <- 0.3
  iir_a0 <- (1 + iir_b1) / 2
  iir_a1 <- - iir_a0
  iir_iter <- 10
  
  # IIR Filter
  iir_resolver <- function(xdots, ydots_prev) {
    ydots_prev <- rev(ydots_prev)
    for(i in seq(1, length(xdots))){
      x <- xdots[c(length(xdots), 1)]
      y <- x[2] * iir_a0 + x[1] * iir_a1 + ydots_prev[1] * iir_b1
      xdots <- c(xdots[-1], x[2])
      ydots_prev <- c(y, ydots_prev)
    }
    rev(ydots_prev[seq(1, length(xdots))])
  }
  
  res <- seq(1, length(basic_ydots)) * 0
  for(i in seq(1, iir_iter)){
    res <- iir_resolver(basic_ydots, res)
  }
  res
}

AJIOB_iir_ydots <- iir_AJIOB()
custom_plot(basic_xdots, AJIOB_iir_ydots, "IIR AJIOB")
custom_freq_plot(basic_freq_xdots, Mod(custom_fft(AJIOB_iir_ydots)), "IIR Freq AJIOB")
