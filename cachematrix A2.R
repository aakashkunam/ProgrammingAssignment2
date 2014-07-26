# This program is aimed at computing inverse of a given matrix
# If data is repeated, the program will display the result
# without computing but by fetching from earlier computations

# This function computes the inverse of a given matrix and returns the result

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixinv <- function(solve) m <<- solve
  getmatrixinv <- function() m
  list(set = set, get = get,
       setmatrixinv = setmatrixinv,
       getmatrixinv = getmatrixinv)
}

# cachematrix function will be called from here after checking
# whether similar data was used earlier or not

cacheSolve <- function(x, ...) {
  m <- x$getmatrixinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrixinv(m)
  m
}

