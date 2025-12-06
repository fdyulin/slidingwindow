#' @title Load Accelerometer File and Compute Acceleration Vector
#' @description Reads CSV and computes Euclidean norm.
#' @param file file Path
#'
#' @return df and plot
#'
#' @examples
#' slidingwindow::run_full_threshold_pipeline("Example_window.csv",1000,0.5)

#' @export
load_accel <- function(file, x="x", y="y", z="z") {
  df <- read.csv(file)
  df$time <- as.POSIXct(df$time, origin="1970-01-01")
  df$acc_vector <- sqrt(df[[x]]^2 + df[[y]]^2 + df[[z]]^2)
  df
}

binary_window <- function(window_vec){
  if(length(window_vec)<2) return(NA_real_)
  if(tail(window_vec,1) > window_vec[1]) 1 else -1
}

threshold_window <- function(window_vec, threshold) {

  diff_val <- max(window_vec) - min(window_vec)

  if (abs(diff_val) >= threshold) {
    return(diff_val)
  } else {
    return(0)
  }
}

#' @export
slide_window_binary <- function(df, col="acc_vector", window_size){
  n <- nrow(df)
  res <- rep(NA_real_, n)
  for(i in 1:(n-window_size)){
    res[i] <- binary_window(df[[col]][i:(i+window_size-1)])
  }
  df$window_result <- res
  df
}

#' @export
slide_window_threshold <- function(df, col="acc_vector", window_size, threshold) {

  n <- nrow(df)
  result <- rep(NA, n)

  for (i in 1:(n - window_size)) {

    window_vec <- df[[col]][i:(i + window_size - 1)]
    result[i]  <- threshold_window(window_vec, threshold)
  }

  df$window_result <- result
  return(df)
}

#' @export
plot_accel <- function(df, window_col="window_result"){
  ggplot2::ggplot(df, ggplot2::aes(x=time))+
    ggplot2::geom_line(ggplot2::aes(y=acc_vector), color="black")+
    ggplot2::geom_line(ggplot2::aes(y=.data[[window_col]]), color="red")
}

#' @export
run_full_binary_pipeline <- function(file, window_size){
  df <- load_accel(file)
  df <- slide_window_binary(df, window_size=window_size)
  list(df=df, plot=plot_accel(df))
}

run_full_threshold_pipeline <- function(file, window_size, threshold){
  df <- load_accel(file)
  df <- slide_window_threshold(df, col="acc_vector", window_size, threshold)
  list(df=df, plot=plot_accel(df))
}

