## These functions provide a way to compute matrix inversion and 
##cache the result, saving computing time and power

## This function creates a special matrix object that can cache 
##its inverse 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}
##Computes the inverse of the special matrix object created by
##makeCacheMatrix. If the inverse has already been calculated,
##cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve (data, ...)
      x$setinverse(i)
      i
}
