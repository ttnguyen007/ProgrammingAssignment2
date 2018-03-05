## This is the submission to the second programming assignment of R-programming.
## The following 2 functions must be used Together to store matrix, and to compute and cache and its inverse.


## makeCacheMatrix() is used to create an R object and its matrix.

makeCacheMatrix <- function(x = matrix())
  {

    m <- NULL
    set <- function(y)
      {
      x <<- y
      m <<- NULL
      }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to
## in order the retrieve the inverse from the cached value that is stored in makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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
