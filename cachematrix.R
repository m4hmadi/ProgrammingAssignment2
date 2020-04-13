## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      mat_inv <- NULL
      set <- function(y) {
            x <<- y
            mat_inv <<- NULL
      }
      get <- function() x
      setinv <- function(matrix_inverse) mat_inv <<- matrix_inverse
      getinv <- function() mat_inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then
# the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      mat_inv <- x$getinv()
      if(!is.null(mat_inv)) {
            message("getting cached data")
            return(mat_inv)
      }
      data <- x$get()
      mat_inv <- solve(data, ...)
      x$setinv(mat_inv)
      mat_inv
}
