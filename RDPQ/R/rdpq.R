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
#' \donttest{
#' rdpq(((1/gamma(delta))*((-log(1-((theta*(exp(-x^lambda))^v)/(1-(1-theta)*(exp(-x^lambda))^v))^(1/v)))^(delta-1))*((theta^(1/v))*lambda*(x^(lambda-1))*exp(-x^lambda))/((1-(1-theta)*(exp(-x^lambda))^v)^(1+(1/v)))),1-pgamma(-log(1-((theta*(exp(-x^lambda))^v)/(1-(1-theta)*(exp(-x^lambda))^v))^(1/v)),delta))
#' }
#'
#'
rdpq <- function(pdf, cdf) {
  # Define the p function
  p <- function(q) {
    integrate(pdf, lower = -Inf, upper = q)$value }
  # Define the q function
  q <- function(p) {
    uniroot(function(x) p(x) - p, interval = c(-10,10))$root }
  # Define the d function
  d<- function(x) {
    diff(cdf(x+c(-0.00001,0.00001)))/0.00002
  }

  # Define the r function
  r <- function(n) {
    x <- numeric(n)
    for (i in 1:n) {
      x[i] <- uniroot(function(x) cdf(x) - runif(1), interval = c(-10,10))$root }
    x }
  list(p = p, q = q, d = d, r = r)
}
