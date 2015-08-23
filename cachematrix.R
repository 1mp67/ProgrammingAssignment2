## makeCacheMatrix creates a special matrix object that can cache its inverse. 
## cacheSolve returns the inverse of the special matrix object, either from the
## the cache or by computing it.


## cacheSolve takes a special matrix (created by makeCacheMatrix function above)
## as an argument and checks to see if the inverse has already been calculated.
## If so, it returns the cached value. If not, then it calculates the inverse
## using the solve function, and sets and returns this inverse.

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL       # start off with inverse set to NULL
     setmatrix <- function(y){
          x <<- y             # sets matrix x to y
          inverse <<- NULL
     }
     getmatrix <- function() x
     setinverse <- function(inv) inverse <<- inv
     getinverse <- function() inverse
     list(setmatrix = setmatrix, getmatrix = getmatrix, 
          setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inverse <- x$getinverse()
     if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     data = x$getmatrix()
     inverse <- solve(data, ...)
     x$setinverse(inverse)
     inverse
}
