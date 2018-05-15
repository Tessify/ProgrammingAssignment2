

## The function 'makeCacheMatrix' creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Create $set object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Create $get object
  get <- function() x
  ## Create $setsolve object
  setsolve <- function(solve) m <<- solve
  ## Create $getsolve object
  getsolve <- function() m
  ## Create listobject with objects defined above
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The function 'cachesolve' computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Retreive cached inverse matrix and check if it is not empty. 
  ## If not empty, retreive cached inverse matrix and end function.
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If cached inverse matrix is empty, get cached data. Calaculate 
  ## inverse matrix of data. Save new inverse matrix and show it.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
