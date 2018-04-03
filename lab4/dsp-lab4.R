# Made by IvAlex

library("foreach")
library("numbers")

y <- function(x) {
  sin(3 * x) + cos(x) + sin(6 * x + 3)
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
hamming_b <- 21/46
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
  
#?????? ?? ?????????????????????????? ???? ??????????????
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












IIR_Dots <- c(0, c(basic_ydots))
IIR_Dots


b0 <- 0.9
a0 <- (1 + b0) / 2
a1 <- -1 * a0



IIR_transformation <- function(x_dots, n, y_dots) {
  if (n <= length(x_dots)) {
    if (length(y_dots) == 0) {
      y <- a0 * x_dots[n] + a1 * x_dots[n - 1] - 0
      y_dots <- c(c(y_dots), y)
      return (IIR_transformation(x_dots, n + 1, y_dots))
    } else {
      y <- a0 * x_dots[n] + a1 * x_dots[n - 1] + b0 * y_dots[length(y_dots)]
      y_dots <- c(c(y_dots), y)
      return (IIR_transformation(x_dots, n + 1, y_dots))
    }
  }
  y_dots
}



wer <- IIR_transformation(IIR_Dots, 2, c())

length(wer)
length(basic_xdots)

custom_plot(basic_xdots, wer, "IIR")
custom_freq_plot(basic_freq_xdots, Mod(custom_fft(wer)), "wfwef")






