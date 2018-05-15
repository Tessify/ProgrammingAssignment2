## Put comments here that give an overall description of what your
## functions do

## The function 'makeCacheMatrix' creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  y <<- x
  xInv <<- solve(x)
}


## The function 'cachesolve' computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (exists("y") && exists("xInv")) {
    if (y==x && !is.null(xInv)) {
      message("getting cached data")
      return(xInv)
    } else {
      xInv <- solve(x)
    }
  } else {
    xInv <- solve(x)
  }
  xInv
}
