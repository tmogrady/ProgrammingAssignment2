## Inverts a square invertible matrix and caches the inverse

## Creates a list of functions to get and set the vector and its inverse
makeCacheMatrix <- function(x = matrix()) {     ## begins the function declaration for makeCacheMatrix
        inv <- NULL                             ## sets inv to NULL (because the inverse hasn't been calculated yet)
        set <- function(y){                     ## $set function allows the user to change the input matrix after running cacheMatrix
                x <<- y                                 ## replaces matrix x with matrix y
                inv <<- NULL                            ## resets inv to NULL
        }
        get <- function() x                     ## $get function takes no arguments and returns x (the input matrix)
        setinverse <- function(solve) inv <<- solve     ## $setinverse function takes solve as an argument and assigns it to inv, one level up
        getinverse <- function() inv                    ## $getinverse function takes no arguments and returns inv (the inverse of the matrix)
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    ## puts all of the functions into a list
}

## Retrieves the cached inverse of the matrix, or calculates it using "solve"
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()                   ## runs the $getinverse function and assigns the returned value to inv
        if(!is.null(inv)){                      ## checks to see if the value is NULL. If it isn't...
                message("getting cached data")  ## ...prints the message...
                return(inv)                     ## ...and returns the cached value
        }
        data <- x$get()                         ## runs $get to get the matrix and puts it into the object data
        inv <- solve(data, ...)                 ## inverts the matrix
        x$setinverse(inv)                       ## runs $setinverse using the inverted matrix as the argument
        inv                                     ## Returns a matrix that is the inverse of x
}
