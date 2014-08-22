## These two functions work together to solve for and cache the inverse of a matrix. In so doing, 
## the inverse does not need to be recalculated, if the input matrix is unchanged, when it is called again.  
## The previously solved for inverse matrix is pulled from the cache.

## makeCacheMatrix is a function that creates a special vector that is a list containing a function to:
## 1) set the matrix; 
## 2) get the matrix;
## 3) set the matrix inverse; and
## 4) get the matrix inverse.

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



## cacheSolve is a function that calculates the inverse of the arguement matrix by using the functions from the special vecor 
## created with the makeCacheMatrix function. However, it first checks to see if the inverse matrix has already been 
## calculated. If so, it gets the inverse natrix from the cache and skips the computation. Otherwise, it solves for 
##the inverse of the matrix and sets it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

}
