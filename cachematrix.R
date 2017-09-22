## This function creates a matrix that gets the value of the matrix and its inverse
#
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if the inverse of a matrix already exists. If so,
## it returns the inverse w/o calculation. Else, it performs the calculation for its inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#
# test functions
#
a <- matrix(c(1:4),2,2)
a2 <- makeCacheMatrix(a)
cacheSolve(a2)