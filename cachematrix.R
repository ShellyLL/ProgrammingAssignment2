## Two Functions to Cach the Inverse of a Matrix

## Function makeCacheMatrix creates a special "maxtrix" to 
## Cach the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
## set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
## get the value of the matrix
    get <- function() x
## set the inverse of the matrix
    setInverse <- function(solve) m <<- solve
## get the inverse of the matrix
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

## The following function calculates the inverse of the special "matrix" 
## which is created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the value in the cache via 
## the setInverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}