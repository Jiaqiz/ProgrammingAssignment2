 ## TWo functions that cache the inverse of a matrix  
  ## Creates a special matrix object that can cache its inverse
  makeCacheMatrix <- function( x = matrix() ) {   
    ## Initialize the inverse
    i <- NULL    
    ## Method to set the matrix
    set <- function(y) {
      x <<- y
      i <<- NULL
    }   
    ## Method the get the matrix
    get <- function() x    
    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
      i <<- inverse
    }   
    ## Method to get the inverse of the matrix
    getInverse <- function() i
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
## 2. Compute the inverse of the special matrix returned by "makeCacheMatrix" above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x <- x$getInverse()
  ## Just return the inverse if its already set
  if( !is.null(x) ) {
    message("getting cached data")
    return(x)
  }
  ## Get the matrix from our object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  x <- solve(data) %*% data
  ## Set the inverse to the object
  x$setInverse(x)
  ## Return the matrix
  x
}
