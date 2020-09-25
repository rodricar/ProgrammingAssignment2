## Function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse. So function cacheSolve computes
## the inverse of the special "matrix" returned by makeCacheMatrix.
## For this assignment, assume that the matrix supplied is
## always invertible.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix())
{
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

## Computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## Test performed successfully

# mat <- matrix(as.numeric(1:4), 2,2)
# a <- makeCacheMatrix(mat)
# res <- cacheSolve(a)
# res
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5