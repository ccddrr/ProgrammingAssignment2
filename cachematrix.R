## The two functions makeCacheMatrix() and cacheSolve() work as a pair for storing
## a matrix value and optionally the corresponding matrix inverse in a cached fashion.


## This function creates a special "matrix" object that can cache its inverse.
## Use its get() method for retrieving the stored matrix value
makeCacheMatrix <- function(x = matrix()) {
  # the cached data
  inv <- NULL
  
  # the matrix value setter method
  set <- function(y) {
    x <<- y                        # set value
    inv <<- NULL                   # invalidate the cache
  }
  
  # the matrix value getter method
  get <- function() x
  
  # update the cached contents
  setinverse <- function(arg) inv <<- arg
  
  # get the chached contents
  getinverse <- function() inv
  
  return ( list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) )
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # try to retrieve a cached matrix inverse
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # cache is invalid -> inverse must be calculated
  data <- x$get()
  inv <- solve(data, ...)
  
  # update cache
  x$setinverse(inv)
  
  return(inv)
}


## Sort of unit test: verify that the above 2 funcs work correctly
mytest <- function()
{
  x <- matrix(c(1,1,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1.5),4,4)
  X <- makeCacheMatrix(x)
  
  Y <- cacheSolve(X)
  
  Z <- Y %*% X$get()
  
  all.equal(Z, diag(1,4) )
}



