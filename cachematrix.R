## The functions implement a cached version to compute the inverse of a matrix.
## If called for the first time or with a new matrix, the inverse will be
## computed and cached. As long as the matrix doesn't change, any further calls 
## will always return the cached version rather than recomputing the inverse.

## Example:
## 
## mat <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##
## mat$get -> returns the matrix
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## mat$getinverse -> inverse hasn't been computed yet
## NULL 
## 
## cacheSolve(mat) -> computes and returns the inverse
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## mat$getinverse -> returns the cached inverse
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## cacheSolve(mat) -> same when calling cacheSolve once again
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## mat$set(matrix(c(4,3,2,1), nrow=2, ncol=2)) -> sets a different matrix
##
## mat$getinverse -> returns NULL as cached inverse has been invalidated
## NULL
## 
## cacheSolve(mat) -> re-computes the inverse
##       [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2


## makeCacheMatrix provides a list of four functions, one pair to set and
## get a matrix, another pair to set and get its (cached) inverse.
## The inverse won't be computed here, that's the task of the cacheSolve function.
## So, only use $setInverse through cacheSolve, don't call it directly!
## If a new matrix is passed to the $setMatrix function, the cached inverse
## will be invalidated.

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix passed as part of the list object 'x'
## (created by makeCacheMatrix). If the inverse is found in the cache, the cached
## version is returned (lines 32-36), else it is computed, cached and returned (lines 37-40).

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- m$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setinverse(i)
  i
}
