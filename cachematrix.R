## Naren R Bakshi
# Intro to R week 3, assignment 2
# The main task is to get the inverse of a special matrix from the function
# called makeCacheMatrix and cache the inverse and then make another function
# called cacheSolve, which retrives the csched version if we already have the inverse
# available from the previous function, else calculate the same.
## We are using R Lexical Scoping principals here.

## this function is a list of functions to set and get the matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverted) inverse <<- inverted
  getInverse <- function() inverse
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## CacheSolve computes the inverse of the special matrix, which is returned by the
# make CacheMatrix function
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("Getting cached inversed matrix")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}