## Put comments here that give an overall description of what your
## functions do
#These functions allow the creatiion of a square matrix and computing of its inverse

## Write a short comment describing this function

#This is a function that creates or returns a list (object) of functions that let manage the values of the 
#matrix including the matrix itself and its inverse.
makeCacheMatrix <- function(mt = matrix()) {
  inv <- NULL
  set <- function(y) {
    mt <<- y
    inv <<- NULL
  }
  get <- function() mt
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
#In the function below we are able to compute the inverse of a matrix via an object x returned by the 
#preceeding function makeCacheMatrix. As it can be a long computing process it first check out if the object
#already contain this value from previous calculation.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
