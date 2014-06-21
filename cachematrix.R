## Functions that given a matrix prepare a data structure 
## which stores the matrix itself as well as cached computations 
## and operate over this data structure.

## Given a matrix x returns a list with functions
## for getting/setting the matrix and caching its inverse
makeCacheMatrix <- function(x = matrix()) {
  m3xi <- NULL
  set <- function(m) {
    x <<- m
    m3xi <<- NULL
  }
  get <- function() x
  setinverse <- function(i) m3xi <<- i
  getinverse <- function() m3xi
  
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## calculates the inverse of the given matrix 
## returned from makeCacheMatrix(...).
## If a cached value is found calculations are skipped
cacheSolve <- function(x, ...) {
  if ( is.null(x$getinverse()) ) { # if(no cache) calc and cache
    x$setinverse( solve(x$get()) )
  }
  return(x$getinverse())
}
