## Put comments here that give an overall description of what your
## functions do

## Function takes matrix as its input and produces 4 functions:
  ## 1st, set function - allows to cache new matrix for future computations
  ## 2nd, get function - allows to retrieve cached matrix
  ## 3rd, setInverse function - allows to cache inverse of a matrix
  ## 4th, getInverse function - allows to retrieve cached inverse of a matrix
## In the end function produces list output including all above mentioned functions for easy access.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CacheSolve function takes makeCacheMatrix function output as its input and 
## retrieves inverse. If the inverse was previously calculated, we receive message
## "getting inverse data" and the inverse itself. Ortherwise we get the matrix by
## calling x$get() and produce its inverse by calling m <- solve(data, ...) function. At
## last we cache inverse by calling x$setInverse(m). The last code piece "m" displays
## currently calculated matrix inverse.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting inverse data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

a = matrix(c(1,2,3,4),2,2)
matrixList = makeCacheMatrix(a)
cacheSolve(matrixList)
## This function proves that seting and getting inverse matrix works appropriately
matrixList$getInverse() %*% a 
