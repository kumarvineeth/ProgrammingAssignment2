## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a supermatrix that encapsulates facilities 
## to set, get a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setInv <- function() inv_m <<- solve(x) #calc. matrix-inverse
  getInv <- function() inv_m
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
## This function implements the makeCacheMatrix() function and demonstrates
## the cache facility facilitated by the R lexically scoping provison
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("retrieving cached data...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}