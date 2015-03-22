## Both of these functions are designed to save computing power by caching
## the inverse of a matrix rather than re-calculating the solution every time.

## This function creates a special 'matrix' object that can cache its inverse.
## In reality, this 'matrix' is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function () m
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by 'makeCacheMatrix'.
## If the inverse has already been calculated, and the matrix has not changed,
## then this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## if the inverse was not found in the cache, calculate it and then 
        ## use 'setinverse' to add it to the cache.
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
