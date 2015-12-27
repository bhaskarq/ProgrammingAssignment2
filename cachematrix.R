## This file contains 2 functions makeCacheMatrix and cacheSolve, which calcualate and the Inverse of a matrix and cache values
## so that they need not be calculated repeteadly (for ex in a loop etc..) and the values can be pulled from a cache in case the 
## values have been calculated already.


##makeCacheMatrix function just stores the values of few elements (caching them). No calculations are performed in this function
##this function contains 4 other functions listed below
##set() - sets the value of the cachedmatrix to the input matrix,it resets the value of the variable inv_val to null value when ever a matrix is sent to set 
##get() - is an functions which just returns the value of the cached matrix
##setInv() - this function just sets the value of inv_val to the value which is passed to the function
##getInv() - this function just returns the value of the inv_val
makeCacheMatrix <- function(x = matrix()) {
  inv_val <- NULL
  set <- function(y) {
    x <<- y
    inv_val <<- NULL
  }
  get <- function() x
  setInv <- function(inv) inv_val <<- inv
  getInv <- function() inv_val
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## this function is to calculate the inverse of a matrix, in the case that this inverse has not been cached before
#incase the inverse for the matrix was calculated earlier and is presently in the cache variable, 
#it will be fetched insted of being calculated again

cacheSolve <- function(x, ...) {
  inv_val <- x$getInv()
  #Checking if inv_val is already in the cache, if present then get the value
  if(!is.null(inv_val)) {
    message("getting cached data")
    return(inv_val)
  }
  #this portion kicks in if the inverse is not calculated earlier
  data <- x$get()
  #using solve to calculate the inverse and then storing it in the cache variable
  inv_val <- solve(data, ...)
  x$setInv(inv_val)
  inv_val
}
