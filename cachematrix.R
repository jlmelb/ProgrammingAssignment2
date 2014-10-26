
## makeCacheMatrix function allows user to set a matrix object (and get)
## also sets and allows for retrieval of inverse matrix         

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(z) {
                x <<- z
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## user can then call cacheSolve function to compute the inverse of the matrix object
## it will also return the inverse if already calculated before due to the 
## calculated inverse being set by makeCacheMatrix

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
