## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    inv <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    inv
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Method for function solve to solve a linear system of equations, or equivalently, 
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  
  ## Set the inverse to the object
  x$setInverse(m)
}
