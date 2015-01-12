## Functions for calculation of inverse of an invertible matrix 
## with support of caching.
##
## Sample usage:
## > m <- makeCacheMatrix(matrix(1:4, 2, 2))
## > cacheSolve(m)  # calculates inverse of the matrix m
## > cacheSolve(m)  # second call returns inverse of the matrix m from the cache
## > m$get() %*% cacheSolve(m)  # returns identity matrix

## Creates an object encapsulating matrix and a cached value.

makeCacheMatrix <- function(x = matrix()) {
  cacheValue <- NULL
  get <- function() x
  setCachedInverse <- function(inv) cacheValue <<- inv
  getCachedInverse <- function() cacheValue
  list(get = get,
       setCachedInverse = setCachedInverse,
       getCachedInverse = getCachedInverse)
}


## Calculates inverse of the matrix wrapped by makeCacheMatrix matrix 
## or returns cached value for the inverse.

cacheSolve <- function(x, ...) {
  cacheValue <- x$getCachedInverse()
  if(!is.null(cacheValue)) {
    message("getting cached data")
    return(cacheValue)
  }
  matrix <- x$get()
  cacheValue <- solve(matrix)
  x$setCachedInverse(cacheValue)
  cacheValue  
}
