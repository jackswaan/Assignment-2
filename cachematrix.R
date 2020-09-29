## The function will make a matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(z){
    x <<- z
    inv <<- NULL
  }
  get <- function(){x} ## to get the matrix
  setInverse <- function(inverse) {inv<<- inverse}
  getInverse <- function() {inv} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## to get the inverse of the matrix from makeCacheMatrix function
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  ## to see if the matrix already been cached
  if (!is.null(inv)){ 
    message("getting cached data")
    return(inv)
  }
  lam <- x$get()
  inv <- solve(lam, ...)
  x$setInverse(inv)
  inv
}