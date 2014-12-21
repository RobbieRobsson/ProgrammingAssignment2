## makeCacheMatrix creates a special matrix object that contains the original
## values and the cached inverse
## 
## cacheSolve computes or loads the inverse from the cache


makeCacheMatrix <- function(x = matrix()) {
  # the matrix x is always invertible (see instructions)
  
  # m will serve to save the inverse to the cache
  m <- NULL
  
  # set is a function to change the underlying matrix
  # set is not really necessary but included for completness
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # get is a function to get the matrix input
  get <- function(){
    x
  }
  
  # setInverse is a function to save the computed inverse to m
  setInverse <- function(y){
    m <<- y
  }
  
  # getInverse is a function to get the (already computed) matrix
  getInverse <- function(){
    m
  }
  
  # this list serves to address the defined functions
  list(set=set, get=get,
       setInverse=setInverse, 
       getInverse=getInverse)
}




cacheSolve <- function(x, ...) {
  
  # again, m saves the (already computed) matrix
  m <- x$getInverse()
  
  # if the inverse has been computed, load it from the environment and return it
  if(!is.null(m)){
    message("Getting data from cache")
    return(m)
  }
  
  # if the inverse has not yet been computed, compute it, save it and return it
  y <- x$get()
  x$setInverse(solve(y, ...))
  x$getInverse()
}
