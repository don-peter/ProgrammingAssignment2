## The two functions below calculate the inverse of a square invertible matrix,
## and cache it. If the inverse is required again and the matrix has not changed,
## the cached inverse is returned.

## makeCacheMatrix creates a list of functions that set the value of a matrix, get the value of the matrix,
## sets the value of the inverse of the matrix, and gets the value the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
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

## cacheSolve calculates the inverse of a square invertible matrix, or if the inverse has
## been previously calculated and the matrix has not changed, it returns the cached inverse value

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

