## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is the analogous version of makeVector; it creates a 'special' matrix, which is really a list containing a function to
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is the analaogous version of cachemean; basically, it replaces the 'mean' function with the 'solve' function, and
## also uses the right names for the functions defined in makeCacheMatrix to get and set the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        ## we assume the provided matrix is invertible
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
