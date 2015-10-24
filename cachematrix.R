# The following two functions makeCacheMatrix & cacheSolve cache the inverse of a matrix.

# makeCacheMatrix is a function which:
# 1. sets the value of a matrix; 2. gets the value of a matrix
# 3. sets the value of inverse of a matrix; 4. gets the value of inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invM <<- inverse
  getinverse <- function() invM
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The cacheSolve function returns the inverse of a square matrix, assuming the matrix is invertible
# If the matrix has been previously computed cacheSolve uses this matirx as no additional
# computation is required. Otherwise, it computes the inverse and sets the value in the cache 
# through the setinverse function.

cacheSolve <- function(x, ...) {
  invM <- x$getinverse()
  if(!is.null(invM)) {
    message("getting cached data.")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data)
  x$setinverse(invM)
  invM
}