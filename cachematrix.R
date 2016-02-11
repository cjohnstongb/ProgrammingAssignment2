## Put comments here that give an overall description of what your
## functions do
# these functions calculate the inverse of a matrix and store the result in a cache 
# so that if called again the inverse is not recalculated

## Write a short comment describing this function
# makeCacheMatrix takes a matrix as an argument and returns a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve takes the list created by makeCacheMatrix and if the inverse is calculated 
# and cached returns that.   If not, then the inverse is calculated and stored in the cache
# and the result returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


