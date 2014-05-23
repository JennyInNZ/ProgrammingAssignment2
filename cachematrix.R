# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse has already been calculated (and 
# the matrix has not changed), then cacheSolve should retrieve the inverse 
# from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  # Checks to see if the inverse matrix has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else
  {
    message("calculating inverse")
  }
  # The inverse matrix has not yet been calculated - do that now
  # and set the inverse into the cache.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
