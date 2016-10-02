## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly.
## The pair of functions below creates and matrix and caches
## the inverse of a matrix for access.

## This function creates a special matrix  object to work with caching inverse function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix<-NULL
  
  ## set function is used to set new values wihtout creating 
  ## a new makeCacheMatrix object
  set <- function(y = matrix()) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  ## get the current matrix in object
  get <- function() x
  
  setinverse <- function(x) inverseMatrix <<- x
  
  getinverse <- function() inverseMatrix
  
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
  
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinverse(inverseMatrix)
  
  
  inverseMatrix
}
