## The functions makeCacheMatrix and cacheSolve cache the inverse of a matrix

# makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv=getinv)
}


# cacheSolve is a function that computes the inverse of matrix x
# If the inverse has already been calculated and the matrix has not
# changed, the function retrieves the inverse from the cache.
# The input matrix is assumed to be invertible.

cacheSolve <- function(x, ...) {
# Check if matrix has changed
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
# The second argument of solve if missing is taken to be the
# identity, and solve returns the inverse of x 
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}