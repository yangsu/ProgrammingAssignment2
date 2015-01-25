## the functions below are to be used together to allow for memoizing of the inverse matrix computation
# First create a cacheable matrix:
#   cachedM <- makeCacheMatrix(m)
# Then compute the inverse of the give matrix by:
#   cacheSolve(cachedM)


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  value <- NULL
  set <- function(y) {
          x <<- y
          value <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) value <<- inverse
  getinverse <- function() value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
