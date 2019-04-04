## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## Next functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  setM <- function(y) {
    x <<- y
    invM <<- NULL
  }
  getM <- function() x
  
  #setInvM <- function(inv) invM <<- inv
  getInvM <- function() invM
  list(set = setM, get = getM,
       setinv = setInvM,
       getinv = getInvM)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above assuming that the matrix supplied is always invertible.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invM <- x$getinv()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  inv <- inv(data, ...)
  x$setinv(inv)
  inv
}


#test 2x2
M1 <- makeCacheMatrix()
M1$set(matrix(1:4, 2))
M1$get()
M1$setinv()
M1$getinv()

cacheSolve(M1)

M1$getinv()

cacheSolve(M1)

#test 3x3
M2 <- makeCacheMatrix()
M2$set(matrix(c(1,3,5,6,7,8,2,4,5),nrow=3, byrow=TRUE))
M2$get()
M2$setinv()
M2$getinv()
cacheSolve(M2)

M1$getinv()

cacheSolve(M2)
