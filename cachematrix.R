## Pair of functions that used to increase the speed of calculating the
## inverse of an invertible matrix by using caching.
## makeCacheMatrix
##   Function used to cache a matrix and it's inverse.
## cacheSolve
##   Function that returns the inverse of a matrix utilizing caching.


## makeCacheMatrix
##   Function used to cache a matrix and it's inverse.
##   Creates functions to get/set a matrix and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Two local variables x & inv are used to store a matrix and it's inverse.
  ## A Get/Set pair of functions are created for assignment and retrieval
  ## of the locally stored values.

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL  # note: reset matrix inverse when matrix "x" is set.
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ## Return the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

## cacheSolve
##   Function that returns the inverse of a matrix utilizing caching.
##   Input x is a variable created using the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Assumption - supplied matrix is always invertible.

  ## Return Cached Matrix Inverse if found.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Matrix Inverse not cached.
  ## Compute the matrix inverse, Cache it, and return it.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}