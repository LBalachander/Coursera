
makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) invM <<- inverse
  
  getInverse <- function() invM
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)  

}

cacheSolve <- function(x, ...) {
  
  ## Return the inverse matrix of x
  invM <- x$getInverse()
  
  if (!is.null(invM)) {
    message("Providing cached data")
    return(invM)
  }
  
  mat <- x$get()
  
  invM <- solve(mat, ...)
  
  x$setInverse(invM)
  
  invM

}