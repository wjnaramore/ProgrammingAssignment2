## functions makeCacheMatrix and cacheSolve work together to store a matrix and its inverse within 
## an environment created by makeCacheMatrix

## makeCacheMatrix stores a matrix and its inverse within its environment
## the argument "x" is a matrix, whose inverse can preferably be solved
## this function returns a list of functions as follows:
## 'set' can reset the matrix (and its inverse) stored within this function environment
## 'get' returns the matrix stored
## 'setinverse' sets the inverse, and is calculated in the cacheSolve function below
## 'getinverse' returns the inverse, if it has been calculated
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(m_inverse) m <<- m_inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve checks if the inverse of the matrix has been calculated and stored.
## if it already has been calculated, it returns that value.
## if it hasn't been calculated, solves the inverse and stores it to the makeCacheMatric envirnoment
## argument 'x' is a list returned by the makeCachMatrix function
## the inverse 'm' is returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

