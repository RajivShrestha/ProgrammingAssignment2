## Matrix inversion is usually a costly computation. So, there 
## are benefits to caching the inverse of a matrix rather than 
## computing it repeatedly. Below are a pair of functions that
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #inverse is not computed yet
  set <-function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i    #set the inverse
  getinv <- function() inv     # return the inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()   #to check if inverse is already available
        if(!is.null(inv)) {   
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) # compute the inverse
        x$setinv(inv)  #set the calculated inverse to cache
        inv
}

