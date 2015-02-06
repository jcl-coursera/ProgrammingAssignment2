## This script implements the second programming assignment for the Coursera course
## "R Programming", R.D. Peng, J. Leek and B. Caffo, started Feb. 2, 2015
##
##  The script implements two functions:
##
##    makeCacheMatrix, which creates a matrix object that can store the matrix inverse
##    cacheSolve, which retrieves the inverse of the matrix object, if available. 
##                Otherwise, it calculates the inverse and stores it in the object
##
##  The programs are described in detail in the subsections below
##
## -----------------------------------------------------------------------------------




## function makeCacheMatrix

## This function creates a list containing four different functions:
## a. set(), which sets the value for the matrix, which is assigned to the variable x
## b. get(), which retrieves the value of the matrix
## c. setinv(), which sets the inverse matrix, which is assigned to the variable invM
## d. getinv(), which retrieves the inverse matrix
## 
## Usage:  
## a. Setting the value of the matrix to h:
##    m <- makeCacheMatrix(h), where h is a matrix
##
##    if the function is called without arguments:
##    m <- makeCacheMatrix() 
##    m$set(h)
##
## b. Setting the value of the inverse of m to invh:
##    m$setinv(invh)
##
## c. Getting the values of m:
##    m$get()
##
## d. Getting the value of the inverse of m:
##    m$getinv()
##
## -----------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) { 
  
  invM <- NULL
  set <- function(y){
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinv <- function(invy) invM <<- invy
  getinv <- function() invM
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}



## -----------------------------------------------------------------------------------
## function cacheSolve
##
## Returns a matrix that is the inverse of 'x'
##
## Usage:
##   x is a matrix object created using the function makeCacheMatrix
##    
##   call the function  m <- cacheSolve(x)
##
##   function detects if input is consistent with makeCacheMatrix
## -----------------------------------------------------------------------------------


cacheSolve <- function(x, ...) {
  if (class(x)!="list") {
    message("input is not valid")
    message("input should be created with makeCacheMatrix()")
  }
  else {
    invM <- x$getinv()
    if(!is.null(invM)) {
      message("getting cached data")
      return(invM)
    }
    data <- x$get()
    invM <- solve(data, ...)
    x$setinv(invM)
  }
}
