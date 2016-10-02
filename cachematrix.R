## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly.
## The pair of functions below creates and matrix and caches
## the inverse of a matrix for access.

## This function creates a special matrix  object to work with caching inverse function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix<-NULL
  
  ## set function is used to set new values without creating 
  ## a new makeCacheMatrix object
  set <- function(y = matrix()) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  ## get the current matrix in object
  get <- function() x
  
  ## set inverse of matrix
  setinverse <- function(x) inverseMatrix <<- x
  
  ## get inverse of matrix
  getinverse <- function() inverseMatrix
  
  ## set up list of functions for use
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## this function looks for cache inverse of matrix before
## recalculating an inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## get cached inverse of matrix if it exists
  inverseMatrix <- x$getinverse()  
  
  ## if not inversematrix is not empty look up cache
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  ## retrieve matrix to be inversed
  data <- x$get()
  
  ## inverse matrix
  inverseMatrix <- solve(data, ...)
  
  ## send inversed matrix to be stored
  x$setinverse(inverseMatrix)
  
  
  inverseMatrix
}
