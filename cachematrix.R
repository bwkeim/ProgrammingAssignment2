## These functions calculate the inverse of a square matrix,
## since inverting matrices is a computationally expensive task, if a given matrix
## has already been inverted the functions return a cached version of the inverted matrix

## To use the following function you must initialize an object invoking this function e.g. b <- makeCacheMatrix().  
## The function returns a list with four elements.  The four element labels are: 
## "set", "get", "setInverse" and "getInverse"

makeCacheMatrix <- function(x = matrix()) {
    Imat <- NULL
    set <- function (y) {
         x <<- y
         Imat <<- NULL
    }
    get <- function() x
    setInverse <- function(invert) Imat <<- invert
    getInverse <- function() Imat
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function must be used in conjunction with makeCacheMatrix.  A makeCacheMatrix "object" must
## be created before this function is invoked.  The cacheSolve function returns and stores the inverse of a matrix.
## On subsequent calls using the same matrix the stored version is returned (not a recalculated version).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Imat <- x$getInverse()
    if(!is.null(Imat)) {
        ## message ("getting cached inverse matrix")
        return(Imat)
    }
    Imat <- x$get()
    Imat <- solve(Imat)
    x$setInverse(Imat)
    Imat
}
