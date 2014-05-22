## Put comments here that give an overall description of what your
## functions do
# 

# These two functions work together. The first one stores all the necessary functions
# in a list (cf. description) which will be used to compute the inverse of the matrix
# and check if this one has already been calculated. Depending on the result, two operations
# are possible using the second functions:
# 1) The inverse has been calculated previously and the data will be collected without
# any calculation
# 2) The data has not been calculated. This involve the computation of the inverse and
# this one is then stored in the workspace in order to be used later (if necessary)

## Write a short comment describing this function:
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Creation of a special matrix object, able to cache its inverse
  # makeCacheMatrix is a function that will return 4 functions (in a list):
  # set the matrix, get the matrix, set the inverse matrix and get it
  # They will be used to compute the inverse of an invertible matrix using 
  # the CacheSolve function
  
  # To test in the CacheSolve function if in the cache:
  inv_matrix <- matrix(data=NA,nrow=1,ncol=1)
  
  set <- function(y) {
    # Set the matrix
    x <<- y # parent: makeCacheMatrix
    inv_matrix <<- matrix(data=NA,nrow=1,ncol=1)
  }
  get <- function(){
    # Get the matrix
    return(x)
  }
  setsolve <- function(inverted){
    # Set the inverse matrix
    inv_matrix <<- inverted # parent: makeCacheMatrix
  }
  getsolve <- function(){
    # Get the inverse matrix
    return(inv_matrix)
  }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function:
# This function computes the inverse of the special "matrix" returned by the 
# makeCacheMatrix function. If the inverse has already been calculated (and 
# the matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # x corresponds to what has been returned by the MakeCacheMatrix function
  
  inv_matrix <- x$getsolve() # Is NULL if not computed yet
  # Thus, we can test it to know if we need to compute it or not
  # If it has already been computed, we get back the value, avoiding any calculation
  if(is.na(inv_matrix[1,1]) == FALSE) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  # This part is used when the inverse has not been computed yet:
  data <- x$get() # We get the matrix
  inv_matrix <- solve(data, ...) # We compute the inverse and all the solve() options 
  # are preserved
  x$setsolve(inv_matrix) # The inverse of the matrix is stored
  return(inv_matrix)
}
