## The following functions allow the caching and returning of the inverse of a matrix,
## to prevent unnecessarily caluclating it repeatedly

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of its inverse
## 4. get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setmean <- function(inverse) i <<- inverse
  getmean <- function() i
  list(set = set, get = get,
       setmean = setinverse,
       getmean = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
