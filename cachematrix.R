## These two functions when used in conjunction,
## will be able to cache an inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	inverseMatrix <- NULL
	set <- function(y) {
		x <<- y
		inverseMatrix <<- NULL
	}

	get <- function() x
	setInverse <- function(inverse) inverseMatrix <<- inverse
	getInverse <- function() inverseMatrix

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned above.
cacheSolve <- function(x, ...) {

## If inverse has already been cached, return inverse of matrix
	inverseMatrix <- x$getInverse()
	if(!is.null(inverseMatrix)) {
		message("Getting cached data")
		return(inverseMatrix)
	}

## If inverse not already cached, it will be here
	matrix <- x$get()
	inverseMatrix <- solve(matrix, ...)
	x$setInverse(inverseMatrix)
	return(inverseMatrix)

}
