## The frst of these two functions creates an inverse of a given matrix and stores it in a cache for future  use
## The second recalls the cached inverse matrix or creates a new one if one is not already stored

## This function creates an inverse of a given matrix 'x' and caches it for future recall

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the cached inverse matrix of 'x' or produces a new inverse matrix if no inverse has been cached

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