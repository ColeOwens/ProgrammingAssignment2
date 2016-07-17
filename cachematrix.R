## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:  
## Function "makeCacheMatrix" is designed to create a cache of the inverse
## of the input matrix.  To 'cache' (or save for later use) allows the 
## computer to save time computing the function and prevents the computer
## from having to recompute the inverse each time. 

makeCacheMatrix <- function(x = matrix()) {
  xinverse <- NULL
  set <- function(y){
    x <<- y
    xinverse <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) xinverse <<- solve
  getMatrix <- function() xinverse
  list(set=set, get=get,
       setMatrix=setMatrix, 
       getMatrix=getMatrix)
}
## Write a short comment describing this function:
## Function 'cacheSolve' is designed to compute the inverse of the cached 
## matrix created by the makeCacheMatrix function.  IF the inverse was already
## calculated within, the function will retreive the cached inverse matrix 
## from the cache.
cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  xinverse <- x$getMatrix()
  if(!is.null(xinverse)){
    message("Getting cached data.")
    return(xinverse)
  }
  matrix <- x$get()
  xinverse <- solve(matrix, ...)
  x$setMatrix(xinverse)
  xinverse
}
