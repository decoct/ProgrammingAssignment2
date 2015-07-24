## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes the matrix to be inverted as its argument and generates
## a list object, comprising 4 functions for use in the next function, cacheSolve

## cacheSolve takes as its input the list object output from makeCacheMatrix and
## outputs the inverse of the matrix used originally in makeCacheMatrix. If the
## calculation has been done previously, it uses the cached result and does not
## recalculate

## Write a short comment describing this function

## makeCacheMatrix takes the matrix to be inverted as its argument and generates
## a list object, comprising 4 functions for use in the next function, cacheSolve

makeCacheMatrix <- function(x = matrix()) {
## Initialise m as NULL
     m <- NULL
## Create set function to input new values for x if needed     
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
## Create get function to get x     
     get <- function() x
## Create setinverse function that assigns the argument to m for use later 
     setinverse <- function(inverse) m <<- inverse
## Create getinverse function that returns the value m 
     getinverse <- function() m
## Return list object comprising the four functions created
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve takes as its input the list object output from makeCacheMatrix and
## outputs the inverse of the matrix used originally in makeCacheMatrix. If the
## calculation has been done previously, it uses the cached result and does not
## recalculate

cacheSolve <- function(x, ...) {
## Run the getinverse function from the list
     m <- x$getinverse()
## If the result is not null, then it has already been calculated, so just
## return the result m and exit the function
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
## If the result IS null, then get the matrix...
     data <- x$get()
## ... calculate the inverse of the matrix...
     m <- solve(data, ...)
## ... store the result for later use if needed ...
     x$setinverse(m)
## ... and return the result
     m
}
