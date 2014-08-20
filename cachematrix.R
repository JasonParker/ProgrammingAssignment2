## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function accepts a matrix as its single argument
## and returns a list of functions. The cacheSolve function takes the result
## of the makeCacheMatrix function (which is a list of functions) and gives
## the inverse of the original matrix.

## Write a short comment describing this function

## This function returns a list of functions that provide each element
## of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## This function takes the list of functions return by makeCacheMatrix
## and calculates the inverse matrix.

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