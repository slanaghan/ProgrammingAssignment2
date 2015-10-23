## makeCacheMatrix and cacheSolve together return the inverse of a matrix
## either by calculating it, or retrieving it if it is stored.

## makeCacheMatrix takes as input an invertible matrix and returns an object 
## that stores the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y) {
    x <<- y
    minverse <<- NULL                      
  }
  get <- function() x # Return original matrix
  setinverse <- function(solve) minverse <<- solve # Store inverse of original matrix
  getinverse <- function() minverse # Return inverse of original matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes the ouput of makeCacheMatrix (containing a function and 
## its inverse, if already calculated), and returns the inverse of the function.

cacheSolve <- function(x, ...) {
  minverse <- x$getinverse()               # Get matrix inverse from input vector
  if(!is.null(minverse)) {                 # If inverse is already cached, get it from vector
    message("getting cached data")
    return(minverse)
  }
  minverse <- solve(x$get())               # If inverse is not cached, find the inverse,
  x$setinverse(minverse)                   # then store inverse in the input vector in parent
  minverse                                 # environment.
}
