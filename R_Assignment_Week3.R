# makeCacheMatrix function creates a special "matrix" object that can
# cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize inverse matrix to NULL
  inv <- NULL
  
  # Set function updates the matrix and resets the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get function retrieves the matrix
  get <- function() x
  
  # setinv function sets the cached inverse
  setinv <- function(inverse) inv <<- inverse
  
  # getinv function retrieves the cached inverse
  getinv <- function() inv
  
  # Get nrow and ncol functions retrieve the number of rows and columns
  getnrow <- function() nrow(x)
  getncol <- function() ncol(x)
  
  # Return a list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv,
       getnrow = getnrow, getncol = getncol)
}

# cacheSolve function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Check if the inverse has already been cached
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  
  # If not, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the computed inverse
  x$setinv(inv)
  
  # Return the computed inverse
  inv
}
