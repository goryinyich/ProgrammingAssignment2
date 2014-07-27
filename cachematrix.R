## The functions below demonstrate how to implement
## simple caching mechanism in R

## The function creates special object to store
## matrix and its cached inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## The function returns the inverse of the matrix
## but first checks if it has been already computed
## before
cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if (!is.null(inv)) return (inv)
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  inv
}
