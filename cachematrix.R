## These two functions work together to solve for and cache the inverse of a matrix. In so doing, 
## the inverse does not need to be recalculated, if the input matrix is unchanged,if is needed again. The 
## Th epreviously solved for inverse matrix is pulled from the cache.

## makeCacheMatrix is a function that creates a special matrix that is a list containing a function to:
## 1) set the value of the matrix; 
## 2) get the value of the matrix;
## 3) set the value of the matrix inverse; and
## 4) get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
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

}
