## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a functions to:
##  set/get the values of matrix and set/get the values of inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
     x <<- y 
     inverse <<- NULL
     }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverted matrix of the special "matrix" created with the above function. It first checks to see if the inverted matrix 
## has already been calculated. If so, it gets the inverted matrix from the cache and skips the computation. Otherwise, it calculates the inverted matrix and 
##sets the value of the inverted matrix in the cache via setinverse function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
   inverse <- x$getinverse()
   if(!is.null(inverse)) {
           message("getting cached data")
           return(inverse)
           }
   data <- x$get()
   inverse <- solve(data, ...)
   x$setinverse(inverse)
   inverse
}
