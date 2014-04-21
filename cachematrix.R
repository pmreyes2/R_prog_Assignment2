## cachematrix.R
## author: Pablo M. Reyes

## This file contains a couple of functions: makeCacheMatrix and cacheSolve
## The function makeCacheMatrix creates a matrix holder and provides
## a set of functions to get and set the matrix and its inverse.
## The function cacheSolve computes the inverse of the matrix
## created by the function makeCacheMatrix. If the inverse has already been
## calculated, and the matrix has not changed, then the calculation is skipped
## and the output is retrieven from the cache.


## The function makeCacheMatrix is used to define, and hold a square matrix
## and its inverse
## usage:
## 1) Creating a matrix holder, passing a matrix as argument:
## > x <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2))
## 2) View the matrix elements by using the method get():
## > x$get()

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invx <<- inverse
    getinverse <- function() invx
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve is used to calculate the inverse of a matrix
## assuming it is invertible
## usage:
## Calculating the inverse:
## > cacheSolve(x)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invx <- x$getinverse()
    if (!is.null(invx)){
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$setinverse(invx)
    invx
}
