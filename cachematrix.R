## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix creates an inverse-cachable matrix object from its argument.
# cacheSolve returns a potentially cached inverse.

## Write a short comment describing this function
#
# makeCacheMatrix takes a matrix "x"
# and returns a list of functions set, get, setinverse, getinverse
# for working with that matrix.
# Functions set and get set and get the matrix x respectively.
# Functions setinverse, getinverse get and set the inverse of matrix x respectively.
# Calling 'set' will clear the inverse cache.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <- NULL
    }
    get <- function() x
    setinverse <- function(pinverse) inverse <<- pinverse
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
#
# cacheSolve takes a list 'x' as produced by the makeCacheMatrix function
# and a variable list of arguments for the solve function.
# If the inverse of the matrix represented by x is cached, that value is returned.
# Otherwise, solve(x, ...) is called to cache and return a new inverse value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (! is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
