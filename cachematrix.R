## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y){ ##Initilizes matrix as NULL
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x ##gets the value of matrix
  setinverse <- function(inverse) invMatrix <<- inverse ##sets the value of the invertible matrix
  getinverse <- function() invMatrix ##gets the value of invertible matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, setinverse = setinverse, getinverse = getinverse) ##list the functions
}


## Write a short comment describing this function
## This functioncomputes the inverse of the special matrix 
## If the inverse has already been then the retrieved value is returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getinverse() ##getting the values
  if(!is.null(invMatrix)) { ##cheking for NULL
    message("getting cached data") ##if not NULL returning the cache
    return(invMatrix)
  }
  data <- x$getMatrix()
  invMatrix <- solve(data, ...)
  x$setinverse(invMatrix)
  invMatrix
}
