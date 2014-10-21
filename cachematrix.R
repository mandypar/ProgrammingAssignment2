## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## get the value of the matrix
  get <- function() {
    x
  }
  ## set the value of the inverse
  setSolve <- function(solve) {
    s <<- solve
  }
  ## get the value of the inverse
  getSolve <- function() {
    s
  }
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}
