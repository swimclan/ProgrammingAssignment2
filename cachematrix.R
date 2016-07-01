## Put comments here that give an overall description of what your
## functions do

## Function that creates the list object of get and set functions to store cached data in the closure

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) i <<- inv
	getinverse <- function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Function that executes the inverse matrix calculation, invoking cache if the solution didnt change

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		if (!is.null(i)) {
			message('Getting inverse from cache...')
			return(i)
		}
		data <- x$get()
		i <- solve(data, ...)
		x$setinverse(i)
		i
}
