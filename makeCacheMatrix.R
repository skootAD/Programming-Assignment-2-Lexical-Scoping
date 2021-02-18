setwd("E:/R Programming/Week 3")

####This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get, setsolve = setsolve,
       getsolve = getsolve)
}

#### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting inversed matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}