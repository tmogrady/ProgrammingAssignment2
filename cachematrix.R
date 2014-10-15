## Inverts a square invertible matrix and caches the inverse

## Creates a list of functions to get and set the vector and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Retrieves the cached inverse of the matrix, or calculates it using "solve"

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()   ##looks for a cached value
        if(!is.null(inv)){      ##if there is a cached value, retrieves it
                message("getting cached data")
                return(inv)
        }
        data <- x$get()         ##puts the matrix into the object "data"
        inv <- solve(data, ...) ##inverts the matrix
        x$setinverse(inv)
        inv      ## Returns a matrix that is the inverse of 'x'
}
