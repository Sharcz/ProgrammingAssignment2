## Johns Hopkins University R Programming Week 3 Peer Graded Assignment:
## Programming Assignment 2: Lexical Scoping (2017-01-21)
##
## This assignment involves creating a special matrix object, via the first
## function, makeCacheMatrix), that can cache its inverse. The second 
## function, cacheSolve, computes the inverse of the matrix. If the inverse
## has already been calculated, then the function will retrieve the inverse
## from the cache.

## Creates a special matrix object that can cache its inverse using the 
## lexical scoping rules in R.

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


## Computes the matrix inverse from the function makeCacheMatrix. Retrieves the 
## inverse from the cache if the inverse has already been calculated from the
## existing matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
              message("getting cached data")
              return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
