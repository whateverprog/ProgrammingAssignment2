## Programming Assignment 2
## Caching the Inverse of a Matrix

## Cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv_x <<- solve
  getinv <- function() inv_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Compute the inverse of a matrix in case it has not been 
## cached before

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinv()
  if(!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data)
  x$setinv(inv_x)
  inv_x
}
