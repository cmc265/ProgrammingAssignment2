makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix creates a special "matrix" with a list of function to
  ##  - set the value of the vector
  ##  - get the value of the vector
  ##  - set the value of the mean
  ##  - get the value of the mean
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  if(dim(data)[1]!=dim(data)[2]) {
    return("Error: not a square matrix")
  }
  i <- solve(data, ...)
  x$setinv(i)
  i
}
