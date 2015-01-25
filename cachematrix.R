## This pair of functions addresses the situation where you don't want to
## compute the inverse of a function more than once. If the inverse of a
## particular matrix has been calculated before it will be pulled from a "cache"
## where the matrix is also stored.

## The function makeCacheMatrix spawns an environment (or "cache") containing a 
## matrix and it's inverse and returns a list of 4 functions that access that
## environment.

##Because the variables X and i are assigned with the <<- operator,
## they are searched for until found up the environment tree. In other words the
## point where the 4 functions are defined upon a particular  call of
## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {  # sets the matrix in the environment
    x <<- y             
    i <<- NULL
  }
  get <- function() x   # gets the matrix
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## This function operates on a cached matrix defined and set by makeCacheMatrix,
## and computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
