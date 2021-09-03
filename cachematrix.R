## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function to cache a Matrix

makeCacheMatrix <- function(x = matrix()) {
  my_inv <- NULL
  set <- function(y){
    x <<- y
    my_inv <<- NULL
  }
  get <- function() {x}
  setInv <- function(inverse) {my_inv <<- inverse}
  getInv <- function() {my_inv}
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve is a function to compute & return the inverse matrix

cacheSolve <- function(x, ...) {
  my_inv <- x$getInv()
  
  if(!is.null(my_inv)) {
    message("getting cached data")
    return(my_inv)
  }
  
  my_mat <- x$get()
  my_inv <- solve(my_mat, ...)
  x$setInv(my_inv)
  my_inv
}