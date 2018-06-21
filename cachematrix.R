## R-programming Assignment 2 - Gavin Ewan


## The functions makeCacheMatrix and cacheSolve allow us to calculate the inverse of a square matrix.
## This inverse is stored in a special 'matrix' object that can be called whenever the inverse is required
## again, rather than carrying out the computationally slow process of calculating the inverse from scratch
## each time that it is required.
## For the purposes of this assignment, we assume that the matrix is always a square invertible matrix.

## The makeCacheMatrix function creates the special 'matrix' object that can cache its inverse so that the
## cacheSolve function does not need to perform the whole process of inverting the matrix from scratch every
## time.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function will either calculate the inverse of the matrix from scratch, or will call
## the special 'matrix' object created by the makeCacheMatrix function to provide the pre-calculated 
## inverse again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data .... ")
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
