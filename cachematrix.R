## This file contains a function makeCacheMatrix that creates 
## a cached matrix, and a function cacheSolve that solves 
## (i.e. calculates the inverse of a square and invertible matrix),
## The inverse is calculated only if it is not already available in the cache.   

## Function call: first call makeCacheMatrix with a square, invertible matrix 
## as the argument, then call cacheSolve with the output of makeCacheMatrix 
## as the argument.
## Course: R Programming - Coursera.
## Sept. 2014

## NOTE 1: This code was developed through modest tweaks to 
## the code provided by the teaching staff as a guide in 
## the programming assignment. 

## NOTE 2: The cacheSolve function has been 
## modified to test whether the matrix is square and its 
## determinant is non-zero, as only then it is invertible.


## the function makeCacheMatrix creates a special "matrix", i.e a list 
## containing a function to set and get the value of the matrix, and to 
## set and get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function cacheSolve calculates the inverse of the special "matrix" 
## created with the function makeCacheMatrix. It first checks to see if 
## the inverse has already been calculated and, if so, it gets the inverse 
## from the cache and skips the calculation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverse in the cache with the setinv function.

## The function provided as a guide has been augmented to test for matrix 
## squareness and invertibility. 
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
	  # is the matrix square and, if so, is it invertible?
	  if (nrow(data)!=ncol(data)) {
		message("matrix is non-square, so not invertible")
	  } else if (det(data) == 0) {
		message("matrix determinant is zero, so matrix not invertible")
	  } else {
		    m <- solve(data, ...)	
 	          x$setinv(m)
        	    m
	  }
}
