## Two functions that enables you to manage the matrix and to compute the 
## functions do

# makeCacheMatrix is a function that contains 4 methods
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setmatrix      set the value of a matrix
# * getmatrix      get the value of a matrix
# * set     get the cahced value (inverse of the matrix)
# * get     get the cahced value (inverse of the matrix)
#

makeCacheMatrix <- function(x = matrix()) {

  ## initialising the cache
  m <- NULL 
  
  ## A method that enables the reciving of the new matrix and initialising the cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## returns the stored matrix (not cached)
  get <- function() {x}
  
  ## cache the given matrix directly 
  setcache <- function(solve) {m <<- solve}
  
  ## returns the cached matrix
  getcache <- function() {m}
  
  ## Setting a list of the mthods
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
  }


# The following function computes the inverse of a matrix 
# and store it using makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  ##Getting the cached matrix
  m <- x$getcache()
  
  ##Checking if the cache is NA and returns the value if it is not NA
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
  
  ##Getting the stored matrix
  data <- x$get()
  
  ##Inversing the stored matrix
  m <- solve(data, ...)
  
  ##Storing the matrix directly into cache.
  x$setcache(m)
  
  ##Returing the cached value.
  m
}
