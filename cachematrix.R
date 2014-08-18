## A pair of functions used to compute and cache the inverse of a matrix defined
## in a list object.

## This function creates a matrix object which can cache its inverse.
## Functions:
## - get() returns the matrix
## - set() assigns a new matrix
## - getinverse() returns the cached inverse matrix
## - setinverse() assigns the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of a matrix object created by
## makeCacheMatrix. If the inverse has been calculated, it is returned from cache
## otherwise the inverse of the matrix is calculated, cached (via a call to setinverse)
## and returned.
## Assumes that the matrix is invertable.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Test
## Using invertable 4x4 matrices from yahoo answers:
## https://answers.yahoo.com/question/index?qid=20120114080640AAoM43Q

m1 <- makeCacheMatrix(matrix(c(2,1,0,0,3,0,2,2,1,3,-3,3,5,1,2,1), nrow=4, ncol=4))
cacheSolve(m1)
i1 <- m1$getinverse()
m1$get() %*% i1
cacheSolve(m1) # message says 'getting cached data'
m1$set(matrix(c(1,1,1,1,1,2,1,4,1,1,1,2,1,2,0,3), nrow=4, ncol=4))
cacheSolve(m1) # no message displayed
i1 <- m1$getinverse()
m1$get() %*% i1
cacheSolve(m1) # message says 'getting cached data'
