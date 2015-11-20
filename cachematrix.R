## This pair of functions caches the inverse of a matrix. It is assumed that the matrix supplied is always 
## invertible.


## The first function makeCacheMatrix sets the value of the matrix, then gets the value, then sets the value of the
## inverse of the matrix, and finally gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    # We use the '<<-' operator to assign a value to an object in an environment that is different 
    # from the current environment.
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function calculates the inverse of the meatrix created with the above function. 
# It first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
# the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the 
# value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
  # Check if the inverse has been calculated
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
  # Get the cached value and skip the computation
    return(i)
  }
  
  # Otherwise calculate the inverse here:
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i      ## Return the matrix that is the inverse of 'x'
}