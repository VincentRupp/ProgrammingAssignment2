## Put comments here that give an overall description of what your
## functions do
  #makeCacheMatrix sets up a matrix capable of caching its inverse
  #cacheSolve finds the inverse of the matrix

## Write a short comment describing this function
#makeCacheMatrix makes a matrix capable of caching its inverse
  #Other functions are provided to provide limited functionality
  #Get, set, and getInverse, in particular
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(invArg) inv <<- invArg
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
#This function returns the inverse of the matrix
#If the inverse has already been calculated, it returns the cached inverse
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
      message("Getting cached inverse.")
      return(inv)
    }
    
    mtrx <- x$get()
    inv <- solve(mtrx)
    x$setInverse(inv)
    inv
}