## A pair of functions that cache and calculate the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	# Creates a special "matrix" object that can cache its inverse.
	#
	# Args:
	# 	x: The original matrix. Optional parameter.
	#
	# Returns:
	# 	An object that contains:
	# 	* set() - set the value of the vector
	# 	* get() - get the value of the vector
	# 	* setsolve() - set the value of the mean
	# 	* getsolve() - get the value of the mean
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
	# Computes the inverse of the special "matrix" returned by 
	# makeCacheMatrix function. If the inverse has already been calculated 
	# (and the matrix has not changed), then the cachesolve should retrieve 
	# the inverse from the cache.
	#
	# Args:
	# 	x: The original "matrix" object.
	# 	...: Additional solve() parameters. Optional parametes. See ?solve
	#
	# Returns:
	# 	A matrix that is the inverse of 'x'.
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}
