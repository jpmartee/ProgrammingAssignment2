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
    ## The output is a list containg 4 functions.
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}




## The following function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve retrieves the inverse from the
## the cache.

cacheSolve <- function(A, ...) {
    ## Searches object for cached inverse
    inverse <- A$getinverse()
    
    ## If inverse is cached we return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # Otherwise, caculate inverse
    matrix <- A$get()
    inverse <- solve(matrix, ...)
    
    # Cache inverse
    A$setinverse(inverse)
    
    #Return inverse
    inverse
}
