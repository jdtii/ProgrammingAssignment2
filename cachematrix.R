## x is a square invertible matrix and there is no error
## handling to validate this (per instructions)
## this function creates a list of functions that
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL

  ## set the matrix; initialize the inv variable
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the matrix data
  get = function() x
  
  ## set the inverse
  ## `<<-` is used to assign a value to a variable that can be
  ## used in another environment than the one it was created in 
  setinverse = function(inverse) inv <<- inverse
  
  ## get the inverse of the matrix
  getinverse = function() inv
  
  ##Build the list of functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## "x" is the list of functions created in makeCacheMatrix()
## this function returns the inverse of the original square matrix
## that was input to makeCacheMatrix() by calculating or 
## using the cached value if it has already been caculated
cacheSolve <- function(x, ...) {
  
  ## get the inverse and check to see if it has already been set
  ## if so, then retrieve it, don’t recalculate it, and exit the function
  inv = x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## if inv hasn’t been set yet, then we need to get the matrix and
  ## do the inversion by passing the get matrix function to solve()
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  ## now that we have calculated the inversion, we store it in cache
  x$setinverse(inv)
  ## exit the function
  return(inv)
}
