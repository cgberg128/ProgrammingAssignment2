## The two functions defined below
## allow the user to cache the inverse of a matrix
## in order to save valuable computing resources

## This function creates a special "matrix" object
## that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #Initialize inv (stands for inverse) to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #Utilize lexical scoping by defining function inside of function
  
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  #Have functions to set and get inverse utilize built-in "solve" function
}


## This function computes the inverse of the special
## "matrix" object that is returned by the makeCacheMatrix function
## If inverse already calculated (and matrix hasn't changed),
## then this function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #If we have already calculated inverse, get cached value
  #and let user know that the value is determined using cached value
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
