## makeCacheMatrix() and cacheSolve() work similarly to makeVector() and cachemean():
## makeCacheMatrix() takes a matrix as input, and stores four functions acting on that matrix in a list as output. If there is already a computed inverse, the value is first erased.
## Two of the functions are only applicable if a matrix inverse has been calculated. 
## cacheSolve() calls for the inverse, then computes the inverse matrix if the value returned is empty.
## Note: if the matrix is non-invertible, cacheSolve() will return an error.

## The function makeCacheMatrix() below works identically to the makeVector() example function except that the default value for x is an empty matrix:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(matrixinverse) inv<<-matrixinverse
  getinverse <- function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## The function cacheSolve() below takes a vector object of the type defined in makeCacheMatrix() above and calculates the matrix inverse for the matrix x stored by makeCacheMatrix():

cacheSolve <- function(x) {
  inv<-x$getinverse()
  #If the matrix inverse has been cached, the inverse is returned and the function terminates:
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Otherwise, get retrieves the original matrix:
  data <- x$get()
  #This calculates the inverse of the matrix:
  inv <- solve(x$get())
  #And this caches the inverse:
  x$setinverse(inv)
  inv
  
        ## Return a matrix that is the inverse of 'x'
}
