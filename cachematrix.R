## The function makeCacheMatrix returns a list of 3 functions - one to get the matrix value that has been passed to 
## makeCacheMatrix as an argument, one to set the inverse matrix value and another to get it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  setinverse <- function(inverse) {
	inv <<- inverse
  }
  getinverse <- function() inv
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function inquires if an inverse matrix has been calculated, and if so, returns its value from cache. 
## If not, it calculates it using the solve function and stores its value.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
	message("getting inverse matrix from cache")
	return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
