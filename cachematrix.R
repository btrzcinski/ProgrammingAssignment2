## This function library provides a way to create and access 
## matrices using cached inverse values. In this way, the inverse
## does not have to be recomputed unless the value of the matrix
## changes.

#### makeCacheMatrix
## Creates a cache-enabled matrix, which allows the cacheSolve function
## to operate on the matrix.
##
## Arguments:
## x: source matrix to be returned as a cache-enabled matrix.
##    (Default: matrix())
##
## Returns:
## A cache-enabled matrix, which is a list of functions:
## (set, get, setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
	# All new matrix values result in a cache miss (NULL).
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x

	# The *inverse functions should generally only be called by cacheSolve.
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


#### cacheSolve
## Uses the solve() function to get the inverse of matrix 'x'. If a cached value
## is available, that value is retrieved instead. The cached value is only 
## retrieved on a per-variable basis (two calls to identically-valued variables  
## will each result in a recomputation of their inverses), and variable value 
## history is not kept indefinitely (so changing a value once and changing it  
## back will invalidate the cache).
##
## Arguments:
## x: the cache-enabled matrix created with makeCacheMatrix. The matrix must be
##    invertible.
## ...: additional parameters to pass to solve(). Parameters are not cached, so 
##      changing parameters from previous calls will not result in a cache miss.
##
## Returns:
## The inverse of matrix 'x'.

cacheSolve <- function(x, ...) {
	# Check for a cache hit, and if so return it
	inv <- x$getinverse()
	if(!is.null(inv)) {
		return(inv)
	}

	# Cache miss, so recompute the inverse, cache it, then return it
	data <- x$get()	
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
