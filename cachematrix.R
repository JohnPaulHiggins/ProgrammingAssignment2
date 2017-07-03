## These functions define a matrix-esque "Matrix" object capable of
## storing its own inverse.

## makeCacheMatrix is a function that initializes the "Matrix" object,
## and equips it with the necessary accessor and mutator functions.

makeCacheMatrix <- function(x = matrix()) {
  # Initial value of inverse is null, prior to calculation
  inv <- NULL
  
  # Sets value of "Matrix" to y, resets inv
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Returns "Matrix" object
  get <- function() x
  
  # Sets inverse of "Matrix" to invrs
  setinv <- function(invrs) inv <<- invrs
  
  # Returns inverse
  getinv <- function() inv
  
  # Return value of function is a list of all four items
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates and returns the inverse of a "Matrix" object.
## If the inverse has already been calculated and cached, it is returned
## without being calculated again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## First, we set the inverse equal to the cached value in our "Matrix"
  inv <- x$getinv()
  
  ## If inv is not NULL, then it has already been calculated, and it is
  ## returned without further calculation.
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise, either the inverse has not been calculated, or x
  ## has been changed. In either case, the inverse is calculated, cached
  ## and returned.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
