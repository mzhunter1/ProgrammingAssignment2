## Programming Assignment #2
## M. Zeb Hunter
## 9-12-2015
## Coursera R Programming Course rprogr-032

## This function creates a list of functions which when supplied a data matrix, sets the matrix in the cache, 
## returns the data matrix from cache, sets an inverse of the matrix in cache and returns the stored inverse. 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      # Set the matrix in the cache
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      
      # Get the matrix from the cache
      get <- function() x
      
      # Set the inverse of a matrix in the cache
      setinverse <- function(invs) i <<- invs
      
      # Get the inverse of a matrix last stored in the cache
      getinverse <- function() i
      
      # return a list of functions output
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Check if the inverse of the argument matrix has an inverse matrix stored in the cache
# If yes, then return the inverse of the matrix from the cache, else calculate the inverse matrix 
# and store it in the cache

cacheSolve <- function(x, ...) {
       
      ## Return a matrix that is the inverse of 'x'
      
      ## Check if the inverse of the matrix is already cached
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      
      ## If not cached, then calculate the inverse and store in cache
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}

