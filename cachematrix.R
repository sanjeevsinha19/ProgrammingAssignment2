## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache


##The first function, makeCacheMatrix creates a "Matrix', which is really a list containing a function to

##set the value of the Matrix
##get the value of the Matrix
## Set Inverse of Matrix
## get the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function returns the inverse of Matrix  .
# However, it first checks to see if the Inverse of the Matrix is in cache. If so, it gets the Inverse of Matrix from
# the cache and skips the computation.Otherwise, it computes the inverse of the Matrix and sets the value using solve()



cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
