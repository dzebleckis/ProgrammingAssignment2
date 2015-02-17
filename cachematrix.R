## Pair of functions that cache the inverse of a matrix.
## Use makeCacheMatrix builder to build matrix proxy which could be
## used as argument for cacheSolve.

## Create proxy for matrix which inverse can be cached.

makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    set <- function(y) {
        m <<- y
        inverse <<- NULL
    }
    get <- function() m
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse

    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculate inverse of matrix and cache it.

cacheSolve <- function(proxy, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- proxy$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- proxy$get()
    inverse <- solve(matrix, ...)
    proxy$setInverse(inverse)
}
