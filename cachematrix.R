## The makeCacheMatrix function creates a list to set a value to the matrix and get this value back.

makeCacheMatrix <- function(x = matrix()){
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }

## It also set and get the matrix inverse.
  
  get <- function() x
  setinverse <- function(inverse) invers <<- inverse
  getinverse <- function() invers
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
     
}


## The cacheSolve function checks if the matrix inverse has already been computed.
## If so, it skips the computation and gets the result.

cacheSolve <- function(x, ...) {
  invers <- x$getinverse()
  if(!is.null(invers)){
    message("retrieving cached inverse")
  return(invers)
  }
## If not, it computes the inverse and set it to cache.
  retrieve <- x$get()
  invers <- solve(retrieve)
  x$setinverse(invers)
  invers
}


