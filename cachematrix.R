## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function that creates "matrix object" that can cache a matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  m = NULL #initialize null holder for matrix
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function () {x}
  set_inverse <- function (mat) {m <<- mat}
  get_inverse <- function () {m}
  #list environment for "matrix object"
  list( set = set, get = get, 
        set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
## This function will compute the inverse of the special "matrix object" 
cacheSolve <- function(x, ...) {
  m <- x$get_inverse()
  # check if the inverse is already cached
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  # get the matrix from the "matrix object"
  data <- x$get()
  # invert the matrix
  m <- solve(data, ...)
  # set the just made inverse to the "matrix object"
  x$set_inverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
