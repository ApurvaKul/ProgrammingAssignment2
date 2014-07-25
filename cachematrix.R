## The program is about the caching the inverse of the Matrix.
##The program defined the two functions MakeCacheMatrix to assign a cache matrix 
## and cacheSolve function to inverse the given matrix and store into cachematrix
## Assumption: The input matrix is square inversible matrix

## Function Name: makeCacheMatrix
##Desciption: The function creates the special cache matrix. assign the value 0 to it
## There are few more functions set, get , setInverse and getInverse which set the initial matrix, get the initial matrix,
## cache the Inverse matrix and get the cached Inverse matrix respectively

makeCacheMatrix <- function(x = matrix()) {
	m <<- NULL 							# set the matrix
	
	set <- function(y){
		x <<- y
		m <<- NULL						# assigning NULL value to cached matrix
	}
	
	get <- function() x					# get the matrix

	setInverse <- function(Inverse) m <<- Inverse 	# set the Inverse to cached matrix

	getInverse <- function() m				# Get the Inverse matrix

 	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)


}


## Function Name: cacheSolve
## The function computes the inverse of the matrix.
## If already computed directly get the computed inverse matrix from cache

cacheSolve <- function(x, ...) {
     	m <- getInverse()

	if (!is.null(m))						# Check whether the matrix already computed
	{
		message("Getting cache Inverse Data")
		return (m)						# Return Inverse matrix from cache
	}
	
	set(x)							# If m is NULL, need to compute Inverse, set the matrix to x

	data <- get()						# Get the matrix

	m <- solve(data)					# Compute to inverse the matrix

	setInverse(m)						# Set inverse matrix to cache

	m							# Return a matrix that is the inverse of 'x'
}
