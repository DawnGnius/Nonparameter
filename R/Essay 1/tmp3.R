# data
n <- 800
x1 <- rbeta(n, 1, 2) * 2 / 3 + rbeta(n, 10 ,10) / 3
x2 <- runif(n, 0, 1)
samples <- cbind(x1, x2)

p <- dim(samples)[2]
n <- dim(samples)[1]

library(stats)
library(mvtnorm)   # for pmvnorm density of multivariate normal distribution
library(snowfall)  # for parallel loop

rodeo.local.bw <- function(xx, samples, h.init=1.5/log(log(n)), beta=0.9, cn=log(n)/n){
  # bandwidth selection
  # para: xx        target point at which we want to estimate f(x), p dim vector
  # para: x         samples, n*p matrix
  # para: beta      learning rate
  # value: h.hat    bandwidth selected, a p dim vector
  
  p <- dim(samples)[2]
  n <- dim(samples)[1]
  h.hat <- rep(h.init, p)
  
  tmpfun <- function(ii)  # for Z.i
    ((xx[jj] - samples[ii, jj])^2 - h.hat[jj]^2) * dmvnorm(xx, mean=samples[ii,], sigma=diag(h.hat))
  
  A <- 1:p
  while(length(A) > 0){
    # loop for each variable in set A
    for (jj in A) {
      # jj   idx for dims
      # ii   idx for samples
      Z.i <- unlist(base::lapply(1:n, FUN=tmpfun))
      Z <- mean(Z.i) /  h.hat[jj]^3 / prod(h.hat) / (2*pi)^(p/2)
      s <- var(Z.i) / h.hat[jj]^6 / (prod(h.hat))^2 / (2*pi)^(p)
      lam <- sqrt(2*s*log(n*cn)) / 10
      if (abs(Z) > lam) {
        h.hat[jj] <- h.hat[jj] * beta
      } else {
        A <- A[A != jj]
      }
    }
  }
  return(h.hat)
}

rodeo.local <- function(t, samples){
  # estimate target points t
  # para: t   target points, a matrix, each row is a point
  # para: x   data points, a matrix, each row is a point
  
  p <- dim(samples)[2]
  n <- dim(samples)[1]
  m <- dim(t)[1]
  
  # h.hat <- t(apply(t, MARGIN=1, FUN=rodeo.local.bw, x=x))   # this is a matrix, num of row is length of t
  # here I prefer not using parallel loop
  snowfall::sfInit(parallel=TRUE, cpus=10)
  snowfall::sfLibrary(stats)
  snowfall::sfLibrary(mvtnorm)
  snowfall::sfExport("n", "p", "samples")
  h.hat <- t(snowfall::sfApply(x=t, margin=1, fun=rodeo.local.bw, samples=samples))   # this is a matrix, num of row is length of t
  snowfall::sfStop()
  
  f.hat.t <- function(ii){
    # estimate at a single point xx, h is corresponding h.hat
    xx <- t[ii, ]
    h <- h.hat[ii, ]
    mean(unlist(lapply(X=1:n, FUN=function(ii) mvtnorm::dmvnorm(xx, mean=samples[ii,], sigma=diag(h)))))
  }
  
  # f.hat <- apply(X=t, MARGIN=1, FUN=f.hat.x)
  # here I prefer not using parallel loop
  snowfall::sfInit(parallel=TRUE, cpus=10)
  snowfall::sfLibrary(stats)
  snowfall::sfLibrary(mvtnorm)
  snowfall::sfExport("n", "p", "samples", "t", "h.hat")
  f.hat <- unlist(snowfall::sfLapply(x=1:m, fun=f.hat.t))
  snowfall::sfStop()
  
  return(list(f.hat=f.hat,h.hat=h.hat))
}

a.para <- 0.05
t1 <- matrix(plot3D::mesh(seq(0, 1, a.para), seq(0, 1, a.para))$x, ncol=1)
t2 <- matrix(plot3D::mesh(seq(0, 1, a.para), seq(0, 1, a.para))$y, ncol=1)
t <- cbind(t1, t2)

# stimate
suppressWarnings(res <- rodeo.local(t, samples))
fit.rodeo <- res$f.hat
h.hat <- res$h.hat

# plot using plot3D
xx <- plot3D::mesh(seq(0, 1, a.para), seq(0, 1, a.para))$x
yy <- plot3D::mesh(seq(0, 1, a.para), seq(0, 1, a.para))$y
zz <- matrix(fit.rodeo, nrow=nrow(xx), byrow=TRUE)
surf3D(x=xx, y=yy, z=zz, xlab="relevant variable", ylab="irrelevant variable", zlab="Density", main="Rodeo", bty = "f", colkey = FALSE)

hh1 <- matrix(h.hat[,1], nrow=nrow(xx), byrow=TRUE)
surf3D(x=xx, y=yy, z=hh1, xlab="relevant variable", ylab="irrelevant variable", zlab="Density", main="Bandwidth", bty = "f", colkey = FALSE)

hh2 <- matrix(h.hat[,2], nrow=nrow(xx), byrow=TRUE)
surf3D(x=xx, y=yy, z=hh2, xlab="relevant variable", ylab="irrelevant variable", zlab="Density", main="Bandwidth", bty = "f", colkey = FALSE)


# plot using plotly
library(plotly)
plotly()