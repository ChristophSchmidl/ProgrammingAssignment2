# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ## get the inverse of the matrix
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

## -------------------------------- makeCacheMatrix end --------------------------------



# cacheSolve returns the inverse of a matrix
# 1. check if inverse has already been calculated
# 2. inverse already calculated: skip new calculation
# 3. inverse not yet calculated: calculate inverse, set the value in cache by using setinverse
cacheSolve <- function(x, ...) {
  ## Assumption: the matrix is invertible
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("accessing cache...")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
## -------------------------------- cacheSolve end --------------------------------

## How to test this script:

## source('cacheMatrix.R')
## > x = rbind(c(3, -1/3), c(-1/3, 3))
## > m = makeCacheMatrix(x)
## > m$get()
##          [,1]       [,2]
## [1,]  3.0000000 -0.3333333
## [2,] -0.3333333  3.0000000
##
## > cacheSolve(m)
##        [,1]   [,2]
## [1,] 0.3375 0.0375
## [2,] 0.0375 0.3375
##
## > cacheSolve(m)
## accessing cache...
##        [,1]   [,2]
## [1,] 0.3375 0.0375
## [2,] 0.0375 0.3375


