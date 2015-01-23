## Following functions save time for computing inverse matrix. 
## It can be done once and then result can be used again and again, instead of repeating the same computatons
## It is assumed that matrix being fed to these functions always has the inverse. 

## This function caches inverse matrix: sets the matrix, gets the matrix, sets and gets inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function looks if the matrix has been already transformed to inverse. If it has - the function returns
## inverse matrix that has been computed before. It it hasn't - the function computes inverse matrix and returns it.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting solved matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
