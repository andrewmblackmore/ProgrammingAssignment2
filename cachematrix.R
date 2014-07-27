## Coursera Data Science Specialization
## Programming in R
## Assignment 2
## Andrew Blackmore

## These two functions create a matrix and calculate the 
## inverse of it using the solve() function. Once calculated
## the value is cached and retreived from the cache if 
## requested again

## makeCacheMatrix creates a set of get and set functions that allow access
## to the values of the original input matrix and also serve to store the
## value of the inverse matrix in a global memory space

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      
      ## Accept the inverse matrix as an input parameter and store it in the 
      ## variable i in the global memory space using the assignment operator <<-
      setinv <- function(inv) i <<- inv
      
      ## Return the value of i, this will be the inverse matrix if it has
      ## been calculated and stored in a global memory space
      getinv <- function() i
      
      ## Return a list of functions allowing access to them from cacheSolve
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve checks to see if the matrix used as the input parameter
## has had its inverse calculated and stored in cache. If no cached 
## value is found, the inverse if calculated and passed to makeCacheMatrix()
## in order to have its value cached

cacheSolve <- function(x, ...) {
      ## Check to see if a cached value exists for the inverse matrix
      i <- x$getinv()
      
      ## If cached value returned, return the value of the inverse matrix and
      ## notify the user that it was pulled from cached data
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }

      ## Retreive the value of X because no cached value was found for the inverse
      data <- x$get()

      ## Use the solve() function to calculate the inverse
      i <- solve(data)

      ## Store the inverse in a global memory space
      x$setinv(i)

      ## Return the inverse
      i
}
