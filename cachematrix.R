## These functions implement a basic matrix class to allow caching of 
## matrix inverses

## makeCacheMatrix creates a matrix object

makeCacheMatrix <- function(x = matrix()) {
    
  x_inv <- NULL

  # set method, to set matrix value
  #    Notice:  setting x clears out old x_inv value
  set <- function(y){
    x <<- y
    x_inv <<- NULL
  }

  # get method, to get matrix value
  get <- function() x

  # methods to set and get the inverse of the matrix object stored in x
  setinv <- function(solve) x_inv <<- solve
  getinv <- function() x_inv

  # create the matrix object as a list  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## cacheSolve checks matrix object to see if inverse has already been
##    computed before computing it

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  x_inv <- x$getinv()
  
  # if inverse has already been computed, return it
  if(!is.null(x_inv)){
    message("getting cached inverse")
    return(x_inv)
  }

  # if inverse has not been computed, compute and return it
  matr <- x$get()
  x_inv <- solve(matr)
  x$setinv(x_inv)
  x_inv
  
}
