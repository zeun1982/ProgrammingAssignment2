## cachematrix.R: used to store the inverses of matrices with their original. 

## makeCacheMatrix(matrix())
## This function stores a matrix and provides
## four additional functions to call:
## set(matrix) stores a new matrix
## get() returns the stored matrix
## setinverse(inverse) stores the passed inverse as the inverse for the matrix
## getinverse() returns the stored inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverseQ
    getinverse <- function() i
    list(set=set, get=get, 
         setinverse = setinverse, getinverse=getinverse)
}

## cacheSolve(x, ...) 
## This function looks in the provided makeCacheMatrix for 
## a stored inverse. If there is one, it returns the inverse;
## if there is none, it calculates, stores and returns the inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        ## The inverse is already stored
        message("getting cached data")
        return(i)
    }
    ## THe inverse is not yet stored
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
