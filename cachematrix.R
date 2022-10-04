## Cache the inverse of an invertible matrix

## define different functions to return a matrix object as a list

makeCacheMatrix <- function(x = matrix()) {
  m1 <- NULL
  set <- function(y) {
    x <<- y
    m1 <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m1 <<- solve
  getinverse <- function() m1
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cache the inverse of the matrix that's already been used as an object for the makeCacheMatrix function 

cacheSolve <- function(x, ...) {
        m1 <- x$getinverse()
  if(!is.null(m1)) {
    message("getting cached data")
    return(m1)
  }
  data <- x$get()
  m1 <- solve(data, ...)
  x$setinverse(m1)
  m1
        ## Return a matrix that is the inverse of 'x'
}
