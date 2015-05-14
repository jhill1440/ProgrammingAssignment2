## Doing long memory intensive calculations is costly so we cache them and use them as needed


## This puts a matrix in cached memory for later retreval.

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


## Write a short comment describing this function
## This checks to see if the matrix is already computed.
## If so it returns the cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
