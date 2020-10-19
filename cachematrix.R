## makeCacheMatrix caches the inverse of matrices to save time
## cachesolve computes the makeCacheMatrix to check if check if it has the inverse available and retrieves it
##if it's not avilable it will create one using solve()

## This function used to cache the inverse and to assign the value of i to help retrive cached data

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


## The below function creates new inverse or retreives the existing one from above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matri <- x$get()
  i <- solve(matri, ...)
  x$setinverse(i)
  i
}

