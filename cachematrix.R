
## this function creates a supermatrix that encapsulates facilities 
## to set, get a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setInv <- function(x) inv_m <<- solve(x) #calc. matrix-inverse
  getInv <- function() inv_m
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
## This function implements the makeCacheMatrix() function and demonstrates
## the cache facility leveraging R's lexical scoping concepts
cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if (!is.null(i)) {
    message("retrieving cached data...") # from second run..
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) # runs only once - the first iteration..
  x$setInv(i)
  i
}

# usecases:
# U <- matrix(1:4,2,2)
# U1 <- makeCacheMatrix(U)
# cacheSolve(U1) - first time, uses the solve() function
# cacheSolve(U1) - outputs "retrieving cached data..." followed by inverse..
# end of usecases
