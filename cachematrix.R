

# Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # The variable used to store the inverse matrix
  
  # A function that sets the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse matrix cache when the matrix changes
  }
  
  # Gets the function of the matrix
  get <- function() x
  
  # Sets the function of the inverse matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Gets the function of the inverse matrix
  getInverse <- function() inv
  
  # Returns a list of the above four methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Computes the inverse of the matrix and returns the cached result if it has already been cached
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If the inverse matrix is already cached, return it directly
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If there is no cache, the inverse matrix is computed and cached
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv
}
