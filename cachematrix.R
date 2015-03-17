##  Together these functions set and cache the inverse of a matrix.
##  If the inverse has already been calculated, the second function gets the inverse
##  from the cache and skips the computation.

##  makeCahceMatrix function does the following:
##    1. Sets up the matrix
##    2. Gets the values of the matrix
##    3. Sets the values of the inverse matrix
##    4. Gets the values of the inverse matrix


makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve function does this:
## Calculates the inverse of the matrix created with the above function...
## ..but first it checks to see if the inverse has already been calculated. 
## If so, it gets the results from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets these values in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
## Return a matrix that is the inverse of 'x'
  m
}


