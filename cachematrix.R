## makeCacheMatrix stores functions to manipulate matrices
##cacheSolve searches for inverse matrix or computes if not in memory


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


## cacheSolve checks if inverse has been
## calculated and then computes inverse if not

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
   data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv) ##add inverse 
    inv
}
