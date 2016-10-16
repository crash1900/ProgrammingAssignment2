## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that can cache its inverse
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # Initialize objects
  inverseMatrix <- NULL
  
  # Define the "behaviours" or functions for objects
  set <- function(y)
  {
    x <<- y
    inverseMatrix <<- NULL
  }

  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  
  # Create a new object by returning a list()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated and the matrix has not changed,
## then fetch the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Get the inverse of the makeCacheMatrix object argument
  inverseMatrix <- x$getinverse()
  
  # Check if the inverse exists, return it if it does
  if (!is.null(inverseMatrix))
  {
    return(inverseMatrix)
  }
  
  # If the inverse does not exist, initialize the getter
  matrixData <- x$get()
  # Solve to get the inverse
  inverseMatrix <- solve(matrixData)
  # Initialize the setinverse property with the inverse matrix
  x$setinverse(inverseMatrix)
  inverseMatrix
}
