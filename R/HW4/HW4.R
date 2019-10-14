library(bootstrap)
library(emplik)
library(plotrix)
data(law)

# data preparation
num_grids <- 100
x <- law$LSAT
y <- law$GPA
df <- cbind(x,y)
myresult <- matrix(NA, ncol = num_grids, nrow = num_grids)

# get limit of data
mu_x <- seq(min(x), max(x), length.out = num_grids)
mu_y <- seq(min(y), max(y), length.out = num_grids)

for(i in 1:num_grids)
  for(j in 1:num_grids) 
    # calculate log likelihood ratio by el.test function in emplik package
    # myresult[i,j] <- -0.5 * el.test(x= df, mu = c(mu_x[i], mu_y[j]))$"-2LLR"
    myresult[i,j] <- el.test(x= df, mu = c(mu_x[i], mu_y[j]))$"-2LLR"

# draw means of data
plot(mean(x), mean(y), cex = 2, col = "brown", pch = 19, xlab = "LSAT", ylab = "GPA", main = "Empirical Likelihood Contours", xlim=c(560,640), ylim=c(2.8,3.4))

# Query Graphical
par(new=TRUE)   

# draw contour of confidence regions using empirical likelihood by emplik package, in red

# contour(mu_x,mu_y, exp(-0.5*myresult), levels=c(0.5,0.1,0.05,0.01), col="black", add=TRUE)
contour(mu_x,mu_y, myresult, levels=c(qchisq(0.5,2),qchisq(0.9,2),qchisq(0.95,2),qchisq(0.99,2)), col="red", add=TRUE)

# confidence regions under normal assumption in blue.
# draw by plotrix package
# ref: liweiyu
m <- colMeans(law)
n <- nrow(law)
p <- 2
s <- cov(law)
values <- eigen(s)$values
vectors <- eigen(s)$vectors
c<-(n-1)*p/(n-p)*qf(0.5,p,n-p)
data.Ellipse(m[1],m[2],a=sqrt(values[1]*c/n), b=sqrt(values[2]*c/n),angle=atan(vectors[2]/vectors[1]),deg=FALSE,border='blue')
c<-(n-1)*p/(n-p)*qf(0.9,p,n-p)
draw.ellipse(m[1],m[2],a=sqrt(values[1]*c/n), b=sqrt(values[2]*c/n),angle=atan(vectors[2]/vectors[1]),deg=FALSE,border='blue')
c<-(n-1)*p/(n-p)*qf(0.95,p,n-p)
draw.ellipse(m[1],m[2],a=sqrt(values[1]*c/n), b=sqrt(values[2]*c/n),angle=atan(vectors[2]/vectors[1]),deg=FALSE,border='blue')
c<-(n-1)*p/(n-p)*qf(0.99,p,n-p)
draw.ellipse(m[1],m[2],a=sqrt(values[1]*c/n), b=sqrt(values[2]*c/n),angle=atan(vectors[2]/vectors[1]),deg=FALSE,border='blue')


