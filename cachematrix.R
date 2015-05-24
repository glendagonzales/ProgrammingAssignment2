## makeCacheMatrix and cacheSolve are a pair of functions that cache the inverse of a matrix.
## Matrix inversion is a costly computation and there may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly.

## makeCacheMatrix create a special object that stores a matrix and cache's its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## Initialize inverse
      inverse <- NULL
      
      ## Set the value of the matrix and set the inverse to null
      set <- function(y) {
        x <<- y
        inverse <<- NULL
      }
      
      ## Get the value of the matrix
      get <- function() x
      
      ## Set the value of the inverse of the matrix
      setinverse <- function(newInverse) inverse <<- newInverse
      
      ## Get the value of the inverse of the matrix
      getinverse <- function() inverse
      
      ## Registers the functions
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated. If not, it calculates the
## inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      ## Get the inverse from the cache
      inverse <- x$getinverse()
      
      ## If the value is null, calculate the inverse and set the inverse
      if (is.null(inverse)) {
        inverse <- solve(x$get())
        x$setinverse(inverse)
      }
      
      ## Return a matrix that is the inverse of 'x'
      inverse
}
