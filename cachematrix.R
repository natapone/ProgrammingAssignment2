## Compute the inverse of a square matrix and store to cache. Return solve if cache hit
## Compare performance between cache and without cache

## USAGE ##
# > source("cachematrix.R")
# > cache_profiler()

# Compare caching performance
cache_profiler <- function(size = 1000, seed=123) {
    inv_m <- genInvMatrix(size = size)
    message("Create matrix ", size , " * ", size)
    c <- makeCacheMatrix(inv_m)
    
    message("Solve inverse matrix without cache")
    t1 <- system.time(cacheSolve(c))
    print(t1)
    
    message("Solve inverse matrix with cache")
    t2 <- system.time(cacheSolve(c))
    print(t2)
}

# A list contains funtion to
# - get/set the value of matrix
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <- NULL
    }
    get <- function() x
    
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    
    list (
        set = set, 
        get = get,
        setsolve = setsolve,
        getsolve = getsolve
    )
}

## Solve Inverse matrix and store in cache
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    s <- solve(data, ...)
    
    ## Return a matrix that is the inverse of 'x'
    x$setsolve(s)
    s
    
}

# A list contains funtion to
# - get/set the value of vector
# - get/set the value of mean
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
        setmean = setmean,
        getmean = getmean
    )
}

# Calculated mean and stored in cache
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

# Generate random number for Invertible matrix
genInvMatrix <- function(size = 100, seed=123) {
    set.seed(seed)
    m <- matrix(rnorm(size * size, mean=10, sd=0.5), nrow=size, ncol=size)
    m <- abs(cor(m))
    m
}