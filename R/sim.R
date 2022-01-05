# ncont: Number of continuous variables
# ndisc: Number of discrete variables

inner_radius <- 1 + rexp(1, 1e-1)
outer_radius <- inner_radius + rexp(1, 1e-1)
coords <- matrix(rnorm(10, 0, outer_radius))

# Radii should be in descending order
torus_interior <- function(coords, radii) {
  res <- coords[[1]]
  for (i in seq_along(radii))
    res <- sqrt(coords[[i+1]]^2 + res^2) - radii[[i]]
  res < 0
}

# Choose coordinates using a spherical Gaussian distribution
# such that the variance equals the outermost radius.

# Allow some variables to be random noise.

# Allow discrete variables to put their thumb on the scale, so to speak.
