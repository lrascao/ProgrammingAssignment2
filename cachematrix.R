## Matrix Cache
## 	abstraction that speed up calculation of the matrix inverse
##
##	eg.
##		# create cache matrix object, provide the matrix to invert
##		cm <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
##
##		# calculate the matrix inverse the first time, subsequent invocations will return cached result
##		matrixInverse <- cacheSolve(cm)

## Given a matrix return a cache matrix object to be fed to the cacheSolve method
## 
## args:
##	matrix to be inverted
makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	set <- function(y) {
	    x <<- y
	    inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## Given a list built using makeCacheMatrix return the inverse of the underlying matrix
##
## args:
##	list object resulted from makeCacheMatrix

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if (!is.null(inv)) {
	    message("getting cached data")
	    return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}
