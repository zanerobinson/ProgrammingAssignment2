# Calculating the inverse of a matrix is computationally intensive. These
# functions aim to reduce processing times by computing the inverse once and
# caching it for future use.

makeCacheMatrix <- function(x = matrix()) {
    # Creates a special "matrix", which is actually a list of four functions that
    # set the value of the matrix, get the value of the matrix, set the value of
    # the inverse matrix, and get the value of the inverse matrix respectively.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    # Returns the inverse of the special "matrix" provided by makeCacheMatrix. 
    # First, checks if the inverse has been previously calculated, and if not,
    # calculates and then caches it for future use.
    m <- x$getInverse()
    if(!is.null(m)) {     # Checks if inverse has been cached previously.      
        message('Getting cached data.')
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) # Solves for the inverse 
    x$setInverse(m)       # Caches the inverse 
    m
}
