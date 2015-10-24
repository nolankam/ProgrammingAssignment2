## makeCacheMatrix is a function that creates a cache for the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## this function uses solve() to compute the inverse of the matrix x and 
## stores the inverse of x in variable inv. If the function is run again using x as
## input arg, function will detect and get the cache value instead of running solve() again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }else{
    ##change from original sample cacheMean, uses solve() to determine the inverse
    inv <- solve(x$get())
    x$setinverse(inv)
    return (inv)
  }
}
