## makeCacheMatrix creates a cached matrix and provides methods to get/set the 
## matrix and its inverse matrix.
## cacheSolve calculates the inverse matrix from makeCacheMatrix and
## caches it to the structure. If there are no changes, it will return the
## stored inverse matrix.

## This function creates a special "matrix" object that can cache its inverse
## It provides 4 methods to set or get the matrix,
## to set or get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(m) {
    x <<- m
    cm <<- NULL
  }
  get <- function() x
  setInverse <- function(im) cm <<- im
  getInverse <- function() cm
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix-function. If the inverse has already been calculated
## (and the matrix has not changed), then the function will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {        
  im <- x$getInverse()
  if (!is.null(im)) {
    print("getting cached matrix")
    return(im)
  }
  cm <- x$get()
  im <- solve(cm)
  x$setInverse(im)
  im
}
