## These functions take an array and invert it but first check whether the invertion was already calculated

## This function creates a list populated with 4 functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                    ## set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                      ## get the value of the vector
  setinvert <- function(solve) m <<- solve ## set the value of the inverted matrix
  getinvert <- function() m                ## get the value of the inverted matrix
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) { 
  m <- x$getinvert()
  if(!is.null(m)) {  ## Checks whether the inversion was previously calculated.
    ## If so, retrieves the cache value and skips the calculation. 
    message("getting cached data")
    return(m)
  }
  data <- x$get()    ## Otherwise, inverts the matrix 
  m <- solve(data, ...)
  x$setinvert(m)
  m
}