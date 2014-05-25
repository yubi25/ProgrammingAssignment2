## Put comments here that give an overall description of what your
## functions do

## Prepare teh matrix and its inverse with "methods" to create,get, and set the matrix and inverse
#
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  inverse <- NULL
  
  ## Sets the matrix and its inverse
  set <- function( matrix ) {
    m <<- matrix
    inverse <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() { return(m) }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse_matrix) { i <<- inverse_matrix }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    return(inverse)
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  my_matrix <- x$getInverse()
  
  ## Return the inverse if already set
  if( !is.null(my_matrix) ) {
    message("getting previously cached data")
    return(my_matrix)
  }
  
  ## Get the matrix from our object
  my_data <- x$get()
  
  ## Calculate the inverse 
  my_matrix <- solve(my_data) %*% my_data
  
  ## Set the inverse to the object
  x$setInverse(my_matrix)
  
  return(my_matrix)
}
