## Below are two functions viz. makeCacheMatrix and cacheSolve. 
## Both the functions are to be used in conjunction to create a martix
## and cache the computed inverse of that matrix. makeCacheMatrix is 
## called in cacheSolve to compute the matrix being created in makeCacheMatrix.

## Function makeCacheMatrix takes a matrix as an input, caches the inverse
## of the matrix and outputs a list of four functions that can be used to 
## display the entered matrix, display cached inverse of the entered matrix, 
## over write the entered matrix with another matrix and hardcode an inverse 
## matrix (if needed). This hardcoded matrix inverse will be overwritten once 
## the matrix inverse is re-computed by the function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinv <- function(inv) mi <<- inv
  getinv <- function() mi
  ## Use the below functions to interact with makeCacheMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function cacheSolve takes the matrix entered in function makeCacheMatrix
## and returns it's inverse. This matrix inverse is passed back to function
## makeCacheMatrix so it can cache it. The computed matrix inverse can be 
## displayed using either of the two functions; either cacheSolve or makeCacheMatrix.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  mi <- x$getinv()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data)
  x$setinv(mi)
  mi
}
