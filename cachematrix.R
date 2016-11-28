## Coursera R Programming Assignment No2
## 'makeCasheMatrix' and 'casheSolve'

## Function 'makeCasheMatrix'
## special matrix for caching its inverse
## returning calculation produced by function 'cacheSolve'

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

## Function "casheSolve"
## calculates a matrix that is the inverse of 'x'
## returned by function "makecasheMatrix"

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
