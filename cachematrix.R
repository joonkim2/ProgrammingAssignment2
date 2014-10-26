## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Using the example special makeVector function as template,
# this function is a list of functions to
#    1. set the value of the matrix
#    2. get the value of the matrix
#    3. set the inverse of the matrix
#    4. get the inverse of the matrix
#
# Assume that matrix is square and inversible. No checks.

makeCacheMatrix <- function(x = matrix()) {
     nvrs <- NULL
     set <- function(y) {
          x <<- y
          nvrs <<- NULL
     }
     
     get <- function() x
     setsolve <- function(solve) nvrs <<- solve
     getsolve <- function() nvrs
     
     list(set = set, get = get, setsolve = setsolve,
          getsolve = getsolve)
}


## Write a short comment describing this function
# Using the functions from makeCacheMatrix, this function computes
# the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     nvrs <- x$getsolve()
     if(! is.null(nvrs)) {
          message("getting cached data")
          return(nvrs)
     }
     
     data <- x$get()
     nvrs <- solve(data, ...)
     x$setsolve(nvrs)
     nvrs
}
