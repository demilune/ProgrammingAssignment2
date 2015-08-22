## This file contains a pair of functions which create a matrix 
## which can cache its inverse.

## makeCacheMatrix creates a special "matrix," which is really 
## a list containing functions to 
##   1.  set the value of the matrix
##   2.  get the value of the matrix
##   3.  set the value of the inverse
##   4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y 
		inv <<- NULL		
        }
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve calculates the inverse of the special "matrix"
## created with the above makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## Get the value of the cached inverse of the 
        ## special matrix x
	inv <- x$getinv()

        ## If the inverse has been calculated and cached,
        ## then return the cached inverse matrix. 
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)	
	}

        ## If the inverse hasn't been calculated yet,
        ## compute the inverse and cache it.
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
