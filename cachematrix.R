## This is a pair of functions: 
## makeCacheMatrix: This function creates a special matrix object
## that can cache its inverse 
## 
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve
## the inverse from the cache. 

## Create a special matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  set_inverse <- function(inverse) {inv <<- inverse}
  get_inverse <- function() {inv}
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  }


## Retrieve inverse of matrix from cache if calculated, calculate and store in
## cache if not calculated.

cacheSolve <- function(x, ...) {
      inv <- x$get_inverse()
      if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$set_inverse(inv)
      inv
}
