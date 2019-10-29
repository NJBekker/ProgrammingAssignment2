## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A function that creates a special "matrix" object that can cache its inverse.
## The 'matrix' is actually a list containing a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inversed matrix.
## 4. Get the value of the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## Return a matrix and cache its inverse.
  ##0. Make cache for inversed matrix.
  i <- NULL
  ##1. Set the value of the matrix.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##2. Get the value of the matrix.
  get <- function() x
  ##3. Set the value of the inverse.
  setinverse <- function(inverse) i <<- inverse
  ##4. Get the value of the inverse.
  getinverse <- function() i
  ##5. Return a special "matrix" that is actually a list with the output of functions 1 to 4.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## A function that computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##0. Retrieve the cached value of the inverse of X.
  i <- x$getinverse()
  ##1. Return cached inverse of X if one is already available.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ##2. If cache is empty, get the value of X.
  data <- x$get()
  ##3. Inverse X.
  i <- solve(data, ...)
  ##4. Cache the inverse of X.
  x$setinverse(i)
  ##5. Return the inverse of X.
  i
}
