## In order to save time when computing repeatedly matrix inversion, we generate
## special matrix objects that store the matrix and cache its inverse the first
## time it is computed.
## 1) To create a special matrix object we use the function 'makeCacheMatrix'
## 2) To obtain the inverse of a special matrix object we use the funcion 
##    'cacheSolve'. If it is the first time it is called, it computed the 
##    inverse and caches the value, otherwise it simple returns the cached 
##    value.
  


## The function 'makeCacheMatrix' creates a special "matrix" object that can 
## cache its inverse. This special "matrix" object is really a list containing
## function to set and get the matrix and its inverse:
## * 'set': stores the matrix. If there is a cached inverse value it is
##   (i.e., set to NULL)
## * 'get': returns the matrix
## * 'setinverse': stores the inverse of the matrix in the cache
## * 'getinverse': returns the cached inverse. It returns NULL if the inverse
##   has not been cached previously.

makeCacheMatrix <- function(x = matrix()) {
  cached_inv <- NULL
  set <- function(y) {
    x <<- y
    cached_inv <<- NULL # Discards previous cache
  }
  get <- function() x
  setinverse <- function(inverse) cached_inv <<- inverse
  getinverse <- function() cached_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function 'cacheSolve' computes the inverse of the special "matrix" 
## returned by 'makeCacheMatrix'. If the inverse has already been calculated
## (and the matrix has not changed), then 'cacheSolve' should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) { 
    ## If there is a cached inverse matrix we use them
    message("getting cached data")
    return(inverse)
  }
  ## Otherwise we compute the inverse and store it in the cache using the
  ## function 'setinverse'
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}