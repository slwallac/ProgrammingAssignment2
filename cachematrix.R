##Assignmnet: Chaching the Inverse of a Matrix

##1. maxkeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
##makeCacheMatrix <- function(x = matrix()) {}
## These functions introduce a special matrix object which caches its inverse.
## The function is designed so as to produce a special "matrix than can be used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Matrix value setting based on function definition and clearing of prior inverse matrix cache.
  set <- function(y) {
    x <<- y    # Need to set the value
    m <<- NULL # Important step for clearing the cache
  }
  # Function definition to determine the value of the matrix
  get <- function() x
  # Use setInverse() to set the inverse
  setInverse <- function(inverse) m <<- inverse
  # Use getInverse() to define the inverse
  getInverse <- function() m
  
  # Return list including the previously mentioned functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##Will be used to return a matrix that is considered to be the inverse of 'x'.
##cacheSolve <- function(x, ...) {}
##cacheSolve is a function that will be defined and usedto calculate the inverse of the matrix ouputted from the function makeCacheMatrix.
##If it has been determedthat the

cacheSolve <- function(x) {
  m <- x$getInverse() #Looks for the cached value of the inverse matrix
  if(!is.null(m)) { #Function executed based on the principle that the cache was not empty because in that case the inverse is in the cache.
    message("getting cached data")
    return(m)
  }
  # If the cache is empty then it has to be calculated, cached, and then once cached, returned.
  data <- x$get()  # Retreive matrix value
  m <- solve(data) # Inverse calculation
  x$setInverse(m)  # Cache result
  m                # Return inverse
}
