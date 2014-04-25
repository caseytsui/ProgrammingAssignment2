# R Programming Assignment 2
# By Casey Tsui
# Format: R
# Last updated: 4/24/14


# Author's Note: This code stores a matrix in cache and later, if the same 
# matrix is called, the cacheSolve function checks to see if it is stored in
# cache and returns the cached value if identical. If it isn't, then it 
# returns the inverse matrix and stores the new matrix into cache


# FUNCTIONS ###################################################################


makeCacheMatrix <- function(x = matrix()) {
        # This function creates a special "matrix" object that can cache its
        # inverse
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmat <- function(solve) m <<- solve
        getinvmat <- function() m
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}


cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of x using a special 'matrix'
        # object from the makeCacheMatrix function as input
        m <- x$getinvmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmat(m)
        m
}


