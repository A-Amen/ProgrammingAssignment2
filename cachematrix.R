## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Simple and standard get/set class like function that caches the inverse
## of matrix using setInv.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(newInv) inv <<- newInv
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## Accepts output of makeCacheMatrix above and returns
## the inverse by checking its cache or setting inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
