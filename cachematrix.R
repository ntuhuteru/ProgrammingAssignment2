## The function created will create a object storing a matrix, which caches the inverse.


##following function creates a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##the following function computes the inverse of the makeCacheMatrix-matrix,
##in case the inverse was already calculated, the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
         inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
