## Gets the input matrix and calculates the inverse of the matrix and caches it
## Returns the Cached Inverse of the matrix data if the matrix value is already used

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  
  set <- function(y){
    x <<- y
    minverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(matrixinverse){
    minverse <<- matrixinverse
  }
  
  getinverse <- function() minverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Computes the inverse of the special matrix returned by the "makeCacheMatrix". 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  matrixinverse <- x$getinverse()
  
  if(!is.null(matrixinverse)){
    message("Getting Cached Data")
    return(matrixinverse)
  }
  
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  
  matrixinverse
  ## Return a matrix that is the inverse of 'x'
}
