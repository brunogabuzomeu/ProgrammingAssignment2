## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly .
## The folowing functions creates a cache of the inverse of a matrix: 
## no new computation will take place if no change in data.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Check if proper argument
  if (class(x)!="matrix") 
    stop('You need to call this function using a matrix!')

  CacheInvMatrix <- NULL
  set <- function(y) {
    x <<- y
    CacheInvMatrix <<- NULL
  }
  get <- function() x
  setCacheInvMatrix <- function(InvMatrix) CacheInvMatrix <<- InvMatrix
  getCacheInvMatrix <- function() CacheInvMatrix
  list(set = set, get = get, setCacheInvMatrix = setCacheInvMatrix, getCacheInvMatrix = getCacheInvMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## ========================================================
## We assume that the matrix supplied is always invertible.
## ========================================================
## Return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
  # Check if proper argument
  if (class(x)!="list") 
    stop('You need to call this function using an object returned by the makeCacheMatrix() function!')
  if (!all(names(x)==c("set","get","setCacheInvMatrix","getCacheInvMatrix"))) 
    stop('You need to call this function using a list returned by the makeCacheMatrix() function!')
  
  # Check if a cache is stored
  CacheInvMatrix <- x$getCacheInvMatrix()
  if(!is.null(CacheInvMatrix)) {
    message("Returning cached data")
    return(CacheInvMatrix)
  }
  
  # Compute new value and store it into cache
  data <- x$get()
  InvMatrix <- solve(a=data, b=data, ...)
  x$setCacheInvMatrix(InvMatrix)
  
  message("Returning newly calculated data")
  InvMatrix
}
