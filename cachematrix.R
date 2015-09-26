## The first function (makeCacheMatrix) creates a list of
## four functions, which set the value of the matrix ("setmatrix),
## get the value of the matrix ("getmatrix"), then set the value of
## the inverse ("setinverse") using the solve command, and then
## gets the value of the inverse ("getinverse").  The second function
## (cacheSolve) calculates the inverse of the matrix created in makeCacheMatrix.
## If the inverse has already been calculated it gets the inverse from the
## cache, otherwise it calculates it

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	setmatrix <- function(y) {
		x <<- y
		m <<- NULL
	}
	getmatrix <- function () x
	setinverse <- function(solve) m <<- solve  #solve is the command which calculates the inverse of a matrix
	getinverse <- function() m
	list (set = setmatrix, get = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  #... allows passing of other arguments to the function
        m <- x$getinverse()
        if(!is.null(m)) {           # checks to see if the inverse has already been calculated
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data,...)  # "solve" calculates the inverse of the matrix
        x$setinverse(m)
        m
}
