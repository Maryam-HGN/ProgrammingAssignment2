## functions calculate the invert of matrix and cache it in other calls.

##creates a special 'matrix', which:
#set the value of the matrix
#get the value of the matrix
#set the value of the invert
#get the value of the invert

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) i <<- invert
    getinvert <- function() i
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}

##calculates the invert of the matrix created with 'makeCacheMatrix' function.
#if the invert has already been calculated, gets it from the cache
#otherwise,calculates the invert and sets its value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinvert()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinvert(i)
    i
}

