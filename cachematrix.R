## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a "special matrix". It takes in one parameter -
## a matrix. It returns a list that contains functions to set the value of the
## matrix, get the value of the matrix, set the value of the inverse, and get
## the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of the special matrix from makeCacheMatrix
## It first checks if the innverse of the matrix has already been calculated
## If it has, then it simply returns the inverse from the cache data
## If not, it calculates the inverse of the matrix using the solve() function
## It then sets the inverse in the cache using the setInverse() function
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data...")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr,...)
  x$setInverse(inv)
  inv
}
