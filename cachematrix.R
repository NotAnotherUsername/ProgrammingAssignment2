## Functions assess a supplied matrix as a variable, and supply
## the inverse of it. If inverse has already been calculated
## then the cached value is returned, otherwise, the inverse
## is calculated and returned.

## Gets, Sets value of a supplied matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



##  Function looks for the calculated matrix inverse in the
##  cache, if found, returns the cache result, 
##  if not found, calculates the inverse of the matrix 
##  using 'solve()'

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } 
  mtrx <- x$get()
  m <- solve(mtrx, ...)
  x$setinv(m)
  m

}
