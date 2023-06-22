### d-function evaluates the PDF function- f(x)
pdf=function(x,delta,v,theta,lambda){
  y=((1/gamma(delta))*(( -log(1-((theta*(exp(-x^lambda))^v)/(1-(1-theta)*(exp(-x^lambda))^v))^(1/v)))^(delta-1))*((theta^(1/v))*lambda*(x^(lambda-1))*exp(-x^lambda))/((1-(1-theta)*(exp(-x^lambda))^v)^(1+(1/v)))
  )
  return(y)
}
### p-function evaluates the CDF function- F(x) = P(X<=x)
cdf=function(x,delta,v,theta,lambda){
  y = 1-pgamma(-log(1-((theta*(exp(-x^lambda))^v)/(1-(1-theta)*(exp(-x^lambda))^v))^(1/v)), delta)
  return(y)
}

x<-0:100
delta<-1
v<-10
theta<-300
lambda<-1

library(RDPQ)
rdpq(pdf,cdf)
