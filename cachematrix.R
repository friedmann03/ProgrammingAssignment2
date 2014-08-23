## define makeCacheMatrix and cacheSolve which can return inverse of matrix
## run makeCacheMatrix on original matrix and resulting object passed to cacheSolve

## makeCacheMatrix generates object that is list of functions to set and hold cached matrices

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                            ## set assigns matrix to x and initializes m for inverse to null in cache
                x <<- y                 
                m <<- NULL
        }
        get <- function() x                             ## get returns original matrix
        setinverse <- function(inverse) m <<- inverse   ## setinverse assigns inverse to m in cache
        getinverse <- function() m                      ## getinverse returns stored inverse matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns inverse of matrix x from cache, if available, or computation

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                             ## retrieve inverse of x from cache
        if(!is.null(m)) {                               ## check if inverse available from cache & if so print
                message("getting cached data")
                return(m)
        }
        data <- x$get()                                 ## assign matrix to data
        m <- solve(data, ...)                           ## inverses matrix & assigns to m
        x$setinverse(m)                                 ## passes inverse to m in cache
        m                                               ## print inverse matrix
}
