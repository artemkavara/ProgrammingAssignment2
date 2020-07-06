## 2 functions are created to get cached inverse matrix

## Creates a "matrix" as a list with four elements: set, get, 
## setinverse, getinverse
makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set <- function(y){
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve(x)
      getinverse <- function() m
      list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}


## Calculates inverse matrix unless the cache is empty and returns it

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
