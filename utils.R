# Generate random number for Invertible matrix
genInvMatrix <- function(size = 100, seed=123) {
    set.seed(seed)
    m <- matrix(rnorm(size * size, mean=10, sd=0.5), nrow=size, ncol=size)
    m <- abs(cor(m))
    m
}

