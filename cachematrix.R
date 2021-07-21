## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  invs <- NULL

  ## getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }

  ## getter/setter for matrix inverse
  getinv <- function() invs
  setinv <- function(inverse) invs <<- inverse

  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  invs <- x$getinv()

  # return cached matrix inverse if it's been already computed
  if (!is.null(invs)) {
    message("cached completed")
    return(invs)
  }

  # compute inverse of matrix
  m <- x$get()
  invs <- solve(m, ...)

  # cache inverse
  x$setinv(invs)

  # return inverse of matrix
  return(invs)
}


# Example usage
 m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
 m2 <- makeCacheMatrix(m)
cacheSolve(m2)
cacheSolve(m2)
