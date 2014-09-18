## caching the inverse of a matrix rather than compute it repeatedly


## makeCacheMatrix function creates a special "matrix" object
## that catches its inverse if available in cache and skips computation
## Otherwise, it calculates the inverse of the data and sets the value
## of inverse in the cache via the setinv function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i<<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve function calculates inverse of an invertible matrix 
## if and only if inverse matrix is not calculated yet
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)){
                message("getting catched data")
                return(i)
        }
        data <- x$get()
        if(det(data) == 0) message("non invertible input matrix")
        else {
                i <- solve(data)
                x$setinv(i)
        }
        i
}
