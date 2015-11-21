## ProgrammingAssignment2 - jamesjaloi
## Functions that can:
## Create a matrix and cache its inverse (to save computing time for future
## inverse operations)
## Compute the inverse of the matrix or retrieve the inverse from the cache

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## set the inverse to NULL
  inverse <- NULL
  changed <- TRUE
  
  ## set function
  set <- function(y) {
    x <<- y
    inverse <<- NULL
    changed <<- TRUE
  }
  
  ## get function
  get <- function() x
  
  getchanged <- function() changed
  
  ## setinverse function
  setinverse <- function(inv) {
    inverse <<- inv
    changed <<- FALSE
  }

  ## getinverse function
  getinverse <- function() inverse
  
  list(set = set, get = get,
       getchanged = getchanged, setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## First get the cached inverse 
  inverse <- x$getinverse()
  
  ## If the inverse is not null and matrix has not been changed
  ##  return the cached inverse
  if(!is.null(inverse) && !x$getchanged() ) {
    message("getting cached data")
    return(inverse)
  }
  
  ## Calculate the inverse of the matrix using solve function
  ## First get the original matrix
  data <- x$get()
  ## Calculate the inverse
  inverse <- solve(data, ...)
  ## Set the inverse for the matrix object
  x$setinverse(inverse)
  ## Return the inverse
  inverse
  
  
}
