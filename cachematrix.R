## Put comments here that give an overall description of what your
## functions do  

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) minverse <<- solve
  getinverse <- function() minverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #m <- makeCacheMatrix(x)
  #x$setinverse()
  #print(x)
  minverse <- x$getinverse()
  #minverse
  if(!is.null(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  data <- x$getinverse()
  minverse <- solve(x$get())
  x$setinverse(minverse)
  minverse
}
