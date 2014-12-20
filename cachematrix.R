## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## makes list with results of four functions defined in the body
## set = set the value of the vector
## get = get the value of the vector
## setIverse = set the value of the inverse
## getInverse = get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}


##usage
a <- rbind(c(1, -1/4), c(-1/4, 1))
b <- makeCacheMatrix(a)
cacheSolve(b)
##returns inverted matrix
cacheSolve(b)
## returns cached inverted matrix


