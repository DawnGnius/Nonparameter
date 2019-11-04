library("numDeriv")
library("viridis")
# Planets
th <- 2 * pi / 3
r <- 2
xi_1 <- r * c(cos(th + 0.5), sin(th + 0.5))
xi_2 <- r * c(cos(2 * th + 0.5), sin(2 * th + 0.5))
xi_3 <- r * c(cos(3 * th + 0.5), sin(3 * th + 0.5))

# Gravity force
gravity <- function(x) {

  (mvtnorm::dmvnorm(x = x, mean = xi_1, sigma = diag(rep(0.5, 2))) +
     mvtnorm::dmvnorm(x = x, mean = xi_2, sigma = diag(rep(0.5, 2))) +
     mvtnorm::dmvnorm(x = x, mean = xi_3, sigma = diag(rep(0.5, 2)))) / 3

}

# Compute numerically the gradient of an arbitrary function
attraction <- function(x) numDeriv::grad(func = gravity, x = x)

# Evaluate the vector field
x <- seq(-4, 4, l = 20)
xy <- expand.grid(x = x, y = x)
dir <- apply(xy, 1, attraction)

# Scale arrows to unit length for better visualization
len <- sqrt(colSums(dir^2))
dir <- 0.25 * scale(dir, center = FALSE, scale = len)

# Colors of the arrows according to their original magnitude
cuts <- cut(x = len, breaks = quantile(len, probs = seq(0, 1, length.out = 21)))
cols <- viridis::viridis(20)[cuts]

# Vector field plot
plot(0, 0, type = "n", xlim = c(-4, 4), ylim = c(-4, 4), xlab = "x", ylab = "y")
arrows(x0 = xy$x, y0 = xy$y, x1 = xy$x + dir[1, ], y1 = xy$y + dir[2, ],
       angle = 10, length = 0.1, col = cols, lwd = 2)
points(rbind(xi_1, xi_2, xi_3), pch = 19, cex = 1.5)



################################################################
# A simulated example for which the population clusters are known
# Extracted from ?ks::dmvnorm.mixt
mus <- rbind(c(-1, 0), c(1, 2 / sqrt(3)), c(1, -2 / sqrt(3)))
Sigmas <- 1/25 * rbind(ks::invvech(c(9, 63/10, 49/4)),
                       ks::invvech(c(9, 0, 49/4)),
                       ks::invvech(c(9, 0, 49/4)))
props <- c(3, 3, 1) / 7

# Sample the mixture
set.seed(123456)
x <- ks::rmvnorm.mixt(n = 1000, mus = mus, Sigmas = Sigmas, props = props)

# Kernel mean shift clustering. If H is not specified, then
# H = ks::Hpi(x, deriv.order = 1) is employed. Its computation may take some
# time, so it is advisable to compute it separately for later reusage
H <- ks::Hpi(x = x, deriv.order = 1)
kms <- ks::kms(x = x, H = H)

# Plot clusters
plot(kms, col = viridis::viridis(kms$nclust), pch = 19, xlab = "x", ylab = "y")


# Summary
summary(kms)
## Number of clusters = 3 
## Cluster label table = 521 416 63 
## Cluster modes =
##           V1          V2
## 1  1.0486466  0.96316436
## 2 -1.0049258  0.08419048
## 3  0.9888924 -1.43852908

# Objects in the kms object
kms$nclust # Number of clusters found
## [1] 3
kms$nclust.table # Sizes of clusters
## 
##   1   2   3 
## 521 416  63
kms$mode # Estimated modes
##            [,1]        [,2]
## [1,]  1.0486466  0.96316436
## [2,] -1.0049258  0.08419048
## [3,]  0.9888924 -1.43852908

# With keep.path = TRUE the ascending paths are returned
kms <- ks::kms(x = x, H = H, keep.path = TRUE)
cols <- viridis::viridis(kms$nclust, alpha = 0.5)[kms$label]
plot(x, col = cols, pch = 19, xlab = "x", ylab = "y")
for (i in 1:nrow(x)) lines(kms$path[[i]], col = cols[i])
points(kms$mode, pch = 8, cex = 2, lwd = 2)



#######################################################################
#Finally, the ks::kms function is applied to the iris[, 1:3] dataset to illustrate a three-dimensional example.
# Obtain PI bandwidth
H <- ks::Hpi(x = iris[, 1:3], deriv.order = 1)

# Many (8) clusters: probably due to the repetitions in the data
kms_iris <- ks::kms(x = iris[, 1:3], H = H)
summary(kms_iris)
## Number of clusters = 8 
## Cluster label table = 47 3 25 11 55 3 3 3 
## Cluster modes =
##   Sepal.Length Sepal.Width Petal.Length
## 1     5.065099    3.442888     1.470614
## 2     5.783786    3.975575     1.255177
## 3     6.726385    3.026522     4.801402
## 4     5.576415    2.478507     3.861941
## 5     6.081276    2.884925     4.710959
## 6     6.168988    2.232741     4.310609
## 7     6.251387    3.375452     5.573491
## 8     7.208475    3.596510     6.118948

# Force to only find clusters that contain at least 10% of the data
# kms merges internally the small clusters with the closest ones
kms_iris <- ks::kms(x = iris[, 1:3], H = H, min.clust.size = 15)
summary(kms_iris)
## Number of clusters = 3 
## Cluster label table = 50 31 69 
## Cluster modes =
##   Sepal.Length Sepal.Width Petal.Length
## 1     5.065099    3.442888     1.470614
## 2     6.726385    3.026522     4.801402
## 3     5.576415    2.478507     3.861941

# Pairs plot -- good match of clustering with Species
plot(kms_iris, pch = as.numeric(iris$Species) + 1,
     col = viridis::viridis(kms_iris$nclust))

# See ascending paths
kms_iris <- ks::kms(x = iris[, 1:3], H = H, min.clust.size = 15,
                    keep.path = TRUE)
cols <- viridis::viridis(kms_iris$nclust)[kms_iris$label]
rgl::plot3d(kms_iris$x, col = cols)
for (i in 1:nrow(iris)) rgl::lines3d(kms_iris$path[[i]], col = cols[i])
rgl::spheres3d(kms_iris$mode, radius = 0.05)
rgl::rglwidget()




