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


pdf <- function(x, mean =0, sd =1) {
  dnorm(x, mean = mean, sd = sd)
}

cdf <- function(x, mean =0, sd =1) {
  pnorm(x, mean = mean, sd = sd)
}

# Create the normal distribution functions
normal_functions <- rdpq(pdf, cdf)

# Test the p function
normal_functions$p(1.96) #0.9750021
normal_functions$p(0.5) # 0.6914625

# Test the q function
normal_functions$q(0.975) #1.959964
normal_functions$q(0.6914625) #0.5

# Test the d function
normal_functions$d(0) #0.3989423

# Test the r function
normal_functions$r(10) # -0.5604756, -0.2301775,1.5587083,0.07050839,0.1292877,1.7150649,0.4609162, -1.2650612, -0.6868529, -0.4456619```

