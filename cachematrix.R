## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.
#
## makeCachMatrix creates a special "matrix" object that can cache the inverse.
## It is really a list containing 4 functions:
### set to store the value of the matrix
### get to retrieve the value of the matrix
### setinc to store the value of the inverse to the cache
### getinv to retrieve the value of the cached inverse.
#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL            # inv is the cached inverse
  set <- function(y) {
    x <<- y              # x is the cached matrix
    invc <<- NULL        # x has changed -> inverse is invalid
  }
  get <- function() x   
  setinv <- function(invc) inv <<- invc
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
#
## The cacheSolve function returns the inverse of the special "matrix"
## created with the function makeCacheMatrix.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache via the getinv function,
## skips the computation and returns the cached inverse.
## Otherwise, it calculates the inverse of the matrix, sets the value of the
## inverse in the cache via the setinv function and returns the calculated
## inverse.
## Remark: This function assumes that the matrix is always invertible,
## so there is no check for this possible failure. 
#
cacheSolve <- function(x, ...) {
  invs<- x$getinv()      # retrieve cached value of inverse
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)         # cached valur is valid -> return it
  }
  data <- x$get()        # retrieve cached value of matrix
  invs <- solve(data)    # compute inverse
  x$setinv(invs)         # cache new value
  invs                   # return new value
}
