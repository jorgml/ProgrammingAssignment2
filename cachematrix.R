### cachematrix.R
###
###   This code is to help to cache a potentially time-consuming computation. For example Matrix inversion is usually 
### a costly computation. It may be of some benefit to cache the inverse of a matrix rather than compute it repeatedly.
### The code contains functions for calculating the inverse of a matrix, caching the results but firstly check the cache 
### to avoid re-computation if it has been computed before.
### It contains below two parts, both are functions that provide an example of caching matrix inversion.
### The first part (function makeCacheMatrix) takes a matrix as input and creates a "Special Matrix", including the caching option.
### The second part (function cacheSolve) uses the first function in its implementation and computes the inverse of 
### the matrix while checking the cache.

## makeCacheMatrix
## Code of the funtion to cache a matrix and calculate the inverse of that matrix.
##
## This function takes a matrix as its input, creates a special "matrix" object that can cache its inverse (in new environment) 
## and return a list of functions: set, get, setInverse, getInverse in is "inverse" variable. 
## It searchs if we have set a previous value. If we have not set a value, getinverse return "null".
## 
## arguments: x, the matrix to be cached, default = empty matrix 
## returns:   a list with functions to access the cache
##             - get: gets the cached matrix
##             - set: sets a new matrix to be cached
##             - getInv: gets the cached inverse of the matrix
##             - setInv: sets the inverse of the matrix to be cached

makeCacheMatrix <- function( x = matrix() ) {      # The function requires as input a matrix 'x' that cannot be set later.
  inv <- NULL                                      # In the first time "inv" (the cache) value is null, but to be filled later.
  set <- function(y) {                             # Assign cache to a new environment
    x <<- y                                        # Set the matrix introduced by user.
    inv <<- NULL                                   # The "inv" value is null.
  } 
  
  get <- function() x                              # This function returns the matrix 'x'.
  getInverse <- function() inv                     # This function returns the inverse of matrix 'x'. 
  setInverse <- function(inverse) inv <<- inverse  # Here pass the inverse and assign it to cache.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) # Return of the following list:
}                                                                              #  set, get, setInverse and getInverse.

## cacheSolve
## Code of the function to solve a cached-matrix (x) and cache the result.
##
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It first checks to see if the inverse matrix has already been calculated & cached (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.
## So if its available it gets the inverse from the cache and returns the inverse, otherwise it does the computation 
## and sets the inverse matrix in cache.
##
## arguments: x, the cached-matrix (made using makeCacheMatrix)
##            ... extra agruments pased to the solve function
## returns:   the inverse of the matrix, if possible taken form the cache,
##            prints a message if the inverse was taken form cache
##

cacheSolve <- function( x, ... ) {   # The function search if there is an inverse matrix saved in 
  inverse <- x$getInverse()          # cache memory from the above function and allocated to "inverse".
  
  if( !is.null( inverse ) )          # It checks if there is not an inverse matrix already been 
  {                                  # calculated in cache, by asking if is not empty "inverse".
    message("getting cached data")   # If there is a cache then it will print a message indicating is getting cache matrix
    return(inverse)                  # And it will return (no computation needed) the cached output from the "inverse".
  }
  
  data <- x$get()                    # If the cache is empty (there is no cache), the matrix 'x' is taken from 
                                     # makeCacheMatrix and the inverse caluclated.
  inverse <- solve( data, ... )      # The inverse matrix is calculated here by solve and stored in "inverse".
  x$setInverse(inverse)              # Save the inverse matrix "inverse" to the cache.
  return(inverse)                    # And return it.
}

### This is a test script for the above functions:
## x <- matrix(1:4,nrow=2,ncol=2)
## y <- makeCacheMatrix(x)
## cacheSolve(y)
## solve(y)
### Run again and it will get from cache.
## cacheSolve(y)
##