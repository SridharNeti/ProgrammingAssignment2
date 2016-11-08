## Put comments here that give an overall description of what your
## functions do

## this function cache's matrix and set to environment

makeCacheMatrix <- function(x = matrix()) {
	mtrx <- NULL
	set <- function(y){
		x <<- y
		mtrx <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) mtrx <<- solve
	getinverse <- function() mtrx
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## this function checks the environment if inverse already exists if so returns that otherwise 
## matrix is inversed and set to cache

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
	m
}
