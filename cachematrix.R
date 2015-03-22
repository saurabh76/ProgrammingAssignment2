## Programming Assignment 2 : Dated 22/03/2015 
## Function to create a special "Matrix" Object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
## Assignment operator to store the values in x and m        
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    set_inverse <- function(solve) m <<- solve
    get_inverse <- function() m
    list(set = set, get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse)
    
}

## Compute the inverse of the special "Matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix hasnt changed), then 
## cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
    
        if(!is.null(m)) {
## write another condition using is.identical function to check if the cached
## matrix is the same as the new matrix            
        message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
    }    

