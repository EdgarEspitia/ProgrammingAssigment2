## Put comments here that give an overall description of what your
## functions do

## This function make a cache matrix
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  minv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    minv <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  setminv <- function(solve) minv <<- solve
  getminv <- function() minv
  list(set = set,
       get = get,
       setmean = setmean,
       getmean = getmean,
       setminv = setminv,
       getminv = getminv)
}


## This function calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
  minv <- x$getminv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  dataMatrix <- x$get()
  minv <- solve(dataMatrix, ...)
  x$setminv(minv)
  minv
}
