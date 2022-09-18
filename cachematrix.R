## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates and lists four functions that
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse value of the matrix
## 4) get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inverse

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computers the inverse of the matrix returned in makeCacheMatrix and caches it. If it had previously
## been computed it skips the computation and retrieves the cached matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
            message("getting cached data")
            inverse
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
