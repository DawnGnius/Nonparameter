# success

# data preparation
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

# true density function, y is h, and z is v.
y <- seq(-3, 1, 0.01)
z <- rep(0, length(y))
for (ii in 1:Num.Cmp) {
  z <- z + pro[ii] * dnorm(y, mean=mu[ii], sd=sigma[ii])
}

# Rodeo local
rodeo.local.bw <- function(xx, x, h.init=1.3/log(log(n)), beta=0.9, cn=log(n)/n){
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

rodeo.local <- function(t, x){
  # estimate target points t
  # para: t   target points, a vector
  # para: x   data points, a vector
  h <- unlist(base::lapply(X=t, FUN=rodeo.local.bw, x=x))
  K <- stats::dnorm
  f.hat <- unlist(base::lapply(X=1:length(t), FUN=function(ii) mean(K((t[ii] - x) / h[ii]))/ h[ii]))
  return(f.hat)
}

t <- seq(-3, 1, 0.01)
h <- unlist(base::lapply(X=t, FUN=rodeo.local.bw, x=x))
plot(t, h, "l")
fit.rodeo <- rodeo.local(t=t, x=x)
plot(t, fit.rodeo, "l", lty=2, ylim=c(0,2.5), xlim=c(-3, 1))
lines(y, z, lty=1, lwd=2)