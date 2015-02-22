## Use makeCachedMatrix to make an object that can store and return a matrix and it's inverse.
##
## An object created by makeCachedMatrix can be passed to cacheSolve which will return the inverse of the matrix,
## but makeCachedMatrix will only calculate the inverse the first time it is called and will use a cached copy
## of the inverse on subsequent calls.

## Usage :
## x <- matrix(1:4, 2, 2)   # Create a 2x2 matrix
## cm <- makeCacheMatrix(x) # Create the wrapper object
## invX <- cacheSolve(cm)   # on this occasion the inverse will be calculated and returned
## invX2 <- cacheSolve(cm)  # on this occasion a cached copy of the inverse will be returned

## makeCacheMatrix takes a square matrix as a parameter (or defaults to an empty matrix)
## Returns a list of functions over x that can be used to set/get the matrix and it's inverse.
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve takes an object created by makeCacheMatrix
## It will return the current stored value of the inverse if it is not null
## If the inverse is null, the function calculates the inverse, stores it, then returns it.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
      message("getting cached inverse")
      return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}