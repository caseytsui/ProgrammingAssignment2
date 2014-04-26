# R Programming Assignment 2
# By Casey Tsui
# Format: R version 3.0.3 (2014-03-06)R
# Last updated: 4/25/14


# Author's Note: This code stores a matrix in cache and later, if the same 
# matrix is called, the cacheSolve function checks to see if it is stored in
# cache and returns the cached value if identical. If it isn't, then it 
# returns the inverse matrix and stores the new matrix into cache


# FUNCTIONS ###################################################################

## This function creates a list object that can cache the inverse of the
## matrix that is used as the x argument
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setinvmat <- function(invmat) {
                m <<- invmat
        }
        getinvmat <- function() {
                m
        }
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}

## Using the returned list object of the makeCacheMatrix function, this
## return an inverse of the original matrix that was used as input for
## the makeCacheMatrix function
cacheSolve <- function(x, ...) {
        # Pull up the cached value of the inverse matrix, if it exists
        m <- x$getinvmat()
        # If a cache exists, then the function returns a message and cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # If a cache does not exist, then the function calculates the inverse,
        # then stores the inverse matrix into x's cache
        data <- x$get()
        m <- solve(data, ...)  # Calculate inverse matrix
        x$setinvmat(m)         # Save result to x's cache
        m                      # Return m
}


