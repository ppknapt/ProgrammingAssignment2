## This is a Programming Assigment 2 solution for R Programming course.

## makeCacheMatrix stores a given matrix and returns a list of functions
## that allows to interact with the cache:
##
## set(x)        - replaces cached matrix.
## get()         - gets the matrix.
## setinverse(x) - sets the matrix inverse.
## getinverse()  - gets the matrix inverse.
##
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

## cacheSolve calculates the inverse matrix of a given argument and
## stores it inernally in the argument's cache, so the next call on
## cached matrix will result in cached argument.
##
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
