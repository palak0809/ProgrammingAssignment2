## The functions below can be used to find out the inverse of a matrix.
## In case, we have the inverse value already computed in the cache, the aim is
## to retrieve the inverse value from the cache to save computation time
## In case we don't have the inverse value in cache, we compute and return it.

## makeCacheMatrix function returns a list of functions to get the input matrix,
## set the input matrix, get the inverse of the matrix and set the inverse value
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve function checks whether the inverse value is present in the cache
## If the inverse value is found in cache, it returns the cached inverse value
## otherwise, it computes the inverse by calling the functions in the above function

cacheSolve <- function(x, ...) { 
  ## Returns a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  dataset <- x$get()
  inv <- solve(dataset, ...)
  x$setinv(inv)
  inv
  }