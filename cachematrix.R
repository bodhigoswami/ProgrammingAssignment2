## There are two functions. One stores cache value of matrix inputted
## the other fuction checks if cache is present. If not it executes.

## makeCacheMatrix obtains a matrix x and produces a cache of it's inverse

makeCacheMatrix <- function(x)
{
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is used to first check if inverse is available in cache. If so, it
## outputs the cahced value. If not it outputs a new value. Message is popped
## idicating if cache was done or not

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } else {message("no cache found")}
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}