## Functions to calculate and cache the inverse of a matrix. The first 
## function creates a generalised matrix which stores the cached inverse.
## The cached value is returned if the inverse has previously been calculated
## and the matrix is unaltered.

## A function to create a matrix object with a cache of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## A function to compute the inverse of the object returned by
## 'makeCacheMatrix'. Returns a cached value if it exists.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Returning cached inverse.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
