# check if Z is decreasing with h
set.seed(111)
n <- 200
Num.Cmp <- 8
pro <- rep(1/8, Num.Cmp)
multi <- sample(1:Num.Cmp, n, replace = T, prob=pro)
mu <- 3*((2/3)^(1:Num.Cmp)-1)
sigma <- (2/3)^(1:Num.Cmp)
x <- NULL
for (ii in 1:Num.Cmp) {
  com_txt <- paste("com", ii, " <- rnorm(length(which(multi==", ii, ")), mean=", mu[ii], ", sd=", sigma[ii], ")",sep="")
  eval(parse(text=com_txt))
  com_txt <- paste("x <- c(x, com", ii, ")", sep="")
  eval(parse(text=com_txt))
}

rodeo.local.bw <- function(xx, x, h.init=5/log(log(n)), beta=0.9, cn=log(n)/n){
  # bandwidth selection
  # para: xx        target point at which we want to estimate f(x)
  # para: x         samples
  # para: beta      learning rate
  # value: h.init   bandwidth
  h1 <- h.init
  while(TRUE){
    Z.i <- ((xx - x)^2 - h1^2) * exp(- (xx - x)^2 / h1^2 / 2) / h1^4 / sqrt(2*pi)
    Z <- mean(Z.i) 
    s <- var(Z.i)
    lam <- sqrt(2*s*log(n*cn)) / 10
    if (abs(Z) > lam) {
      h1 <- h1 * beta
    } else {
      return(h1)
    }
  }
}