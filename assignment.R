# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cache
  inverse <- NULL
  
  # Setter function to set the matrix
  set <- function(newValue) {
    x <<- newValue
    inverse <<- NULL  # Invalidate the cache when the matrix changes
  }
  
  # Getter function to get the matrix
  get <- function() {
    x
  }
  
  # Getter function to get the cached inverse
  getInverse <- function() {
    inverse
  }
  
  # Setter function to set the cached inverse
  setInverse <- function(newInverse) {
    inverse <<- newInverse
  }
  
  # Return a list of functions
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

# Function to compute the inverse of a matrix and cache the result
cacheSolve <- function(cacheMatrix, ...) {
  # Check if the inverse is already cached
  cachedInverse <- cacheMatrix$getInverse()
  if (!is.null(cachedInverse)) {
    message("Getting cached inverse")
    return(cachedInverse)
  }
  
  # If not cached, compute the inverse
  data <- cacheMatrix$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  cacheMatrix$setInverse(inv)
  
  inv
}
