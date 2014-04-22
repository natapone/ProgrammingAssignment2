## Put comments here that give an overall description of what your
## functions do

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
