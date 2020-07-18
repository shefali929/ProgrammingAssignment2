## These R functions allow to cache potentially time consuming computations
## This function specifically caches the inverse of a matrix
## Caching the inverse makes it easier for us to call it rather than recomputing it

## The makeCacheMatrix function creates an special object that stores a matrix,
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y){
         x <<- y
         inv <<- NULL
   }
   get <- function() {x}
   setInverse <- function(inverse) {inv <<- inverse}
   getInverse <- function() {inv}
   list(set =set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## The makeCacheMatrix and cacheSolve function work in conjunction
## The cacheSolve function caches the inverse of the special "matrix" which is created in the above function.
##  If the inverse has already been calculated, then the cachesolve retrieves the inverse from the cache.
##  That is indicated by the message "getting cached data"


cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
        if(!is.null(inv)){
                 message("getting cached data")
                 return(inv)    ## Return a matrix that is the inverse of 'x'
      }
      mat <- x$get()
      inv <- solve(mat, ...) ##if the inverse is not calculated then solve() is used to find the inverse
      x$setInverse(inv)
      inv
}

        

