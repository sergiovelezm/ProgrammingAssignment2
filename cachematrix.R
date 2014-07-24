## These functions cache the inverse of a matrix in order to not compute it repeatedly
## functions do

## creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the Inverse of the matrix
#get the value of the the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      #set: caches the matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      #get: returns the special matrix
      get <- function() x 
      
      #setInverse: caches the value of the inverse
      setInverse <- function(inverse) inv <<- inverse 
      
      #getInverse: returns the inverse of the special matrix
      getInverse <- function() inv 
      
      #The function returns a list of functions
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function calculates the inverse of the special matrix defined above. it first 
# checks to see if the inverse has already been calculated. if already calculated, it 
# gets the mean from the cache and skips computation. Otherwise it calculates the inverse
# and sets the value in the cache using the setInverse function.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting data from previous calculations")
            return(inv)
      }
      data <- x$get()
      message("Calculating Inverse for the first time")
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
}
