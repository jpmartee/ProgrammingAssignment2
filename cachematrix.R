## This pair of functions cache the inverse of a matrix.


## The followig function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(A = matrix()) {
    inverse <- NULL
    set <- function(y) {
        A <<- y
        inverse <<- NULL
    }
    get <- function() A
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}




## The following function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve retrieves the inverse from the
## the cache.

cacheSolve <- function(A, ...) {
    inverse <- A$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- A$get()
    inverse <- solve(matrix, ...)
    A$setinverse(inverse)
    inverse
}
