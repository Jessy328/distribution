

#' @title Get r, d, p, and q functions from a given PDF and CDF.
#'
#' @description This function returns the p, q, d, and r functions corresponding to the input PDF and CDF.
#'
#' @param pdf the probability density function
#' @param cdf the cumulative distribution function
#'
#' @return r,d,p,q functions
#'
#' @examples
#' pdf <- function(x, mean =0, sd =1) {dnorm(x, mean = mean, sd = sd)}
#' cdf <- function(x, mean =0, sd =1) {pnorm(x, mean = mean, sd = sd)}
#' normal_functions <- rdpq(pdf, cdf)
#' normal_functions$p(1.96) #0.9750021
#' normal_functions$p(0.5) # 0.6914625
#'
#' normal_functions$q(0.975) #1.959964
#' normal_functions$q(0.6914625) #0.5
#'
#' normal_functions$d(0) #0.3989423
#' normal_functions$r(10) # -0.5604756, -0.2301775,1.5587083,0.07050839,0.1292877,1.7150649,0.4609162, -1.2650612, -0.6868529, -0.4456619...
rdpq <- function(pdf, cdf) {

  # Define the p function
  p <- function(q) {cdf(q)}
  # Define the q function
  q <- function(p) {
    uniroot(function(x) p(x) - p, interval = c(-10,10))$root }
  # Define the d function
  d<- function(x) {pdf(x)}

  # Define the r function
  r <- function(n) {
    x <- numeric(n)
    for (i in 1:n) {
      x[i] <- uniroot(function(x) cdf(x) - runif(1), interval = c(-10,10))$root }
    x}
  return(list(p = p, q = q, d = d, r = r))
}
