## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## If the contents of a matrix are not changing, it may make sense to cache the
## inversion of the matrix so that when we need it again, it can be looked up
## in the cache rather than recomputed.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {

     # Initialize the cache
     m <- NULL
     
     # Write the matrix and initialize the cache
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     # Return the matrix
     get <- function() x
     
     # Write the inverse in the cache 
     setInv <- function(inv) m <<- inv
     
     # Return the inverse in the cache
     getInv <- function() m
     
     # Put the functions in a list
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
     
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
     
     # Check if the inverse has already been calculated 
     m <- x$getInv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     # Get the matrix that hasn't already been calculated
     data <- x$get()
     
     # Compute the inverse
     m <- solve(data, ...)%*%data
     
     # Put the inverse in the cache
     x$setInv(m)
     
     # Return a matrix that is the inverse of 'x'
     m
    
}