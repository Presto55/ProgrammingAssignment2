## these two functions create a special matrix object and create an efficient 
## means of retrieval for the inverse matrix if it is required more than once


## makeCacheMatrix creates a special matrix object cacheSolve calculates inverse of the matrix.
## If matrix inverse has already been calculated, it will print comment and retrieve from saved data instead

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix 
## If the cached inverse is already available, cacheSolve retrieves it
## otherwise it calculates and returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}