#' @title Load Accelerometer File and Compute Acceleration Vector
#'
#' @description Reads CSV and computes Euclidean norm.
#'
#' @param file file path for a .csv file
#' @param x variable to use for x (default = "x")
#' @param y variable to use for y (default = "y")
#' @param z variable to use for z (default = "z")
#'
#' @return dataframe with combined acceleration vector
#'
#' @importFrom utils read.csv
#' @export
load_accel <- function(file, x="x", y="y", z="z") {
  df <- read.csv(file)
  df$time <- as.POSIXct(df$time, origin="1970-01-01")
  df$acc_vector <- sqrt(df[[x]]^2 + df[[y]]^2 + df[[z]]^2)
  df
}

#' Binary window
#'
#' @description
#' Takes a window and returns a binary value 1 och -1 depending
#' on direction of acceleration.
#'
#' @param window_vec a vector contaning a window
#'
#' @importFrom utils tail
#' @returns a vector containing the binary output
binary_window <- function(window_vec){
  if(length(window_vec)<2) return(NA_real_)
  if(tail(window_vec,1) > window_vec[1]) 1 else -1
}


#' threshold window
#'
#' @description
#' Takes a window and returns the acceleration difference if it exceeds
#' given threshold.
#'
#' @param window_vec a vector containing a window
#' @param threshold threshold to use
#'
#' @returns a vector with applied threshold
threshold_window <- function(window_vec, threshold) {

  diff_val <- max(window_vec) - min(window_vec)

  if (abs(diff_val) >= threshold) {
    return(diff_val)
  } else {
    return(0)
  }
}



#' sliding window binary
#'
#' @description
#' Applies sliding window filter using threshold method.
#'
#' @param df a dataframe
#' @param col column containing the acceleration vector
#' @param window_size window size to use
#'
#' @returns a dataframe with a column `window result` containing the filtered
#' acceleration vector
#'
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

#' sliding window threshold
#'
#' @description
#' Applies sliding window filter using threshold method.
#'
#' @param df a dataframe
#' @param col column containing the acceleration vector
#' @param window_size window size to use
#' @param threshold threshold to use
#'
#' @returns a dataframe with a column `window result` containing the filtered
#' acceleration vector
#'
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

#' plot accel
#'
#' @description
#' Creates a plot of the acceleration vector and the filtered vector
#'
#' @param df a dataframe
#' @param window_col the filtered column
#'
#' @importFrom rlang .data
#' @export
plot_accel <- function(df, window_col="window_result"){
  ggplot2::ggplot(df, ggplot2::aes(x=.data$time))+
    ggplot2::geom_line(ggplot2::aes(y=.data$acc_vector), color="black")+
    ggplot2::geom_line(ggplot2::aes(y=.data[[window_col]]), color="red")
}


#' run full binary pipeline
#'
#' @param file input file
#' @param window_size window size
#'
#' @returns a list containing:
#' - a dataframe with a combined acceleration vector and applied threshold filter
#' - a plot of the acceleration vector and filtered vector
#'
#' @export
run_full_binary_pipeline <- function(file, window_size){
  df <- load_accel(file)
  df <- slide_window_binary(df, window_size=window_size)
  list(df=df, plot=plot_accel(df))
}

#' run full threshold pipeline
#'
#' @param file input file
#' @param window_size window size
#' @param threshold threshold to use
#'
#' @returns a list containing:
#' - a dataframe with a combined acceleration vector and applied threshold filter
#' - a plot of the acceleration vector and filtered vector
#'
#' @examples
#' csvfile <- system.file("extdata", "Example_window.csv", package = "slidingwindow")
#' run_full_threshold_pipeline(csvfile,1000,0.5)
#'
#' @export
run_full_threshold_pipeline <- function(file, window_size, threshold){
  df <- load_accel(file)
  df <- slide_window_threshold(df, col="acc_vector", window_size, threshold)
  list(df=df, plot=plot_accel(df))
}

