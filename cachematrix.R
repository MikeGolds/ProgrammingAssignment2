## The below functions calculate the inverse of a matrix and saves it to the cache,
## thus the next time the user attempts to caclulate the inverse of the matrix the saved
## value is returned. By using the cache this saves time in longer computations.

## This function creates the special matrix, containing functions to:
## set and get the value of both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Create a Matrix
  
    m <- NULL
      ## Define Cache.
  
  set <- function(y) {
    x <<- y     
      ## Assign matrix "y" to "x".
    
    m <<- NULL  
      ## Set "m" back to NULL.
  }
  
  get <- function() x 
      ## Return Matrix.
  
  setinverse <- function(inverse) m <<- inverse 
      ## Set "m" = to "x" inverse.
  
  getinverse <- function() m 
      ## Return "x" from cache.
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calculates the inverse of the matrix, that is in the function above.
## However firstly it checks if the inverse was already calculated.
## If the inverse was already calculated, it retrieves the inverse from the cache,
## thus skipping the calculation. If the inverse has not been already calculated the
## functon below takes the inverse and stores the "new" inverse in the cache.

cacheSolve <- function(x, ...) {
  ## Return the inverse matrix x.
  
    m <- x$getinverse()
    
  if(!is.null(m)) {
    message("Getting Cached Data...")
      return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  m
}