## Computing "Matrix inversion" is usually time consuming
## and there may be some benefits to catching the inverse of a matrix 
## rather than compute it many times.

## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## This function produces a list containing a function to do the following operations:
##      1- set the value of the matrix
##      2- get the value of the matrix
##      3- set the value of the inverse
##      4- get the value of the inverse

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function "cacheSolve" computes the inverse of the special "matrix" 
## returned by the previous function "makeCacheMatrix".
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cacheSolve" function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
