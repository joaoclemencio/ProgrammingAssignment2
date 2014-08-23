## These two functions work together to create a structure of a matrix and its own inverse
## In this way the inverse of the matrix is calculated only once

## This function constructs the structure (a list) which holds the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    # x contains the matrix to be inverted
    
    i <- NULL # i will contain the inverted matrix
    
    # set allows for reconstruction of the basic list variables, x and i
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # get returns the inputted matrix
    get <- function() {
        x
    }
    
    # setinverse assigns a calculated matrix (inverse) to the i variable
    setinverse <- function(inverse) {
        i <<- inverse
    }
    
    # getinverse returns the inverted matrix
    getinverse <- function() {
        i
    }
    
    # the function returns the following list
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )

}


## This function calculates the inverse of a matrix created via the makeCacheMatrix function and caches it within this structure

cacheSolve <- function(x, ...) {
    
    # To avoid unnecessary calculations, first we check if x (a makeCacheMatrix list) already has a cached inverse
    i <- x$getinverse()
    
    # If i is not null, the function returns that value and the function ends
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # If not, the inverse is calculated
    # First we get the original matrix and assign it to the variable 'data'
    data <- x$get()
    
    # Then we calculate the inverse of 'data' and assign it to 'i'
    i <- solve(data, ...)
    
    # Then we assign the inverted matrix to the original makeCacheMatrix list via its 'setinverse' function
    x$setinverse(i)
    
    # Finally we return the inverted matrix 'i' (which could also be achieved by x$getinverse)
    i
    
}
