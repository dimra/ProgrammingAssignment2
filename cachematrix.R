?help## The functions in this file are used to calculate the inverse of a matrix
## Caching is used if the inverse has already been calculated

## This is a Matrix object that returns a list of four functions
## Setter/getter for Matrix, setter/getter for its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Uses the object defined above to get the inverse of a provided matrix, using cache if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    # check if inv already exists, if so, simply return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # otherwise, calculate the inverse on the matrix provided
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
