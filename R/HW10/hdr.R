library(ks)
# Function to compute and plot a kde level set. Observe that kde stands for an
# object containing the output of density(), although obvious modifications
# could be done to the function code could be done to receive a ks::kde object
# as the main argument
kde_level_set <- function(kde, c, add_plot = FALSE, ...) {

  # Begin and end index for the potantially many intervals in the level sets
  # of the kde
  kde_larger_c <- kde$y >= c
  run_length_kde <- rle(kde_larger_c) # Trick to compute the length of the
  # sequence of TRUEs that indicates an interval for which kde$y >= c
  begin <- which(diff(kde_larger_c) > 0) # Trick to search for the begin of
  # each of the intervals
  end <- begin + run_length_kde$lengths[run_length_kde$values] - 1 # Compute
  # the end of the intervals from begin + length

  # Add polygons to a density plot? If so, ... are the additional parameters
  # for polygon()
  if (add_plot) {

    apply(cbind(begin, end), 1, function(ind) {
      polygon(x = c(kde$x[ind[1]], kde$x[ind[1]],
                    kde$x[ind[2]], kde$x[ind[2]]),
              y = c(0, kde$y[ind[1]],
                    kde$y[ind[2]], 0), ...)
      })

  }

  # Return the [a_i, b_i], i = 1, ..., K in the K rows
  return(cbind(kde$x[begin], kde$x[end]))

}







# Simulate sample
n <- 200
set.seed(12345)
samp <- rnorm(n = n)

# We want to estimate the highest density region containing 0.75 probability
alpha <- 0.25

# For the N(0, 1), we know that this region is the interval [-x_c, x_c] with
x_c <- qnorm(1 - alpha / 2)
c_alpha <- dnorm(x_c)

# This corresponds to the c_alpha

# Theoretical level set
x <- seq(-4, 4, by = 0.01)
plot(x, dnorm(x), type = "l", ylim = c(0, 0.5), ylab = "Density")
rug(samp)
polygon(x = c(-x_c, -x_c, x_c, x_c), y = c(0, c_alpha, c_alpha, 0),
        col = rgb(0, 0, 0, alpha = 0.5), density = 10)
abline(h = c_alpha, col = 3, lty = 2) # Level

# Kde
bw <- bw.nrd(x = samp)
c_alpha_hat <- quantile(ks::kde(x = samp, h = bw, eval.points = samp)$estimate,
                        probs = alpha)
kde <- density(x = samp, bw = bw, n = 4096, from = -4, to = 4)
lines(kde, col = 2)
kde_level_set(kde = kde, c = c_alpha_hat, add_plot = TRUE,
              col = rgb(1, 0, 0, alpha = 0.5))
##         [,1]     [,2]
## [1,] -1.2337 1.378266
abline(h = c_alpha_hat, col = 4, lty = 2) # Level
legend("topright", legend = expression("True density", "Kde", "True level set",
                                       "Kde level set", "Level " * c[alpha],
                                       "Level " * hat(c)[alpha]),
       lwd = 2, col = c(1, 2, rgb(0:1, 0, 0, alpha = 0.5), 3:4),
       lty = c(rep(1, 4), rep(2, 4)))

