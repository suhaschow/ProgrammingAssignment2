#
#
# It's a function that returns a list of functions
# It stores a Non Invertible Matrix and cached value of its inverse
# This function contains the following functions
#  *	setMatrix 
#  *  getMatrix 
#  *  cacheInverse
#  *  getInverse
makeCacheMatrix <- function(inputmatrix = numeric()) {
  	cache <- NULL

	# Stores a Matrix
  	setMatrix <- function(input){
	  inputmatrix <<- input
  	  cache <<- NULL
	}

	# Returns the stored Matrix/ Getter method
	getMatrix <- function() inputmatrix

	# Caches the inverse Matrix
	cacheInverse <- function(solveinverse){
		cache <<- solveinverse
	}

	# Gets the cached Inverse Matrix
	getInverse <- function() cache
	
	# Returns list of defined functions
	list(setMatrix = setMatrix, getMatrix = getMatrix,
   		cacheInverse = cacheInverse, getInverse = getInverse)
}

#	This function solves the inverse of matrix created by makeCacheMatrix 
cacheSolve <- function(y, ...) {

	# Retrieves the cached inverse
    	inverse <- y$getInverse()
    	if(!is.null(inverse)){
	      message("getting cached data")
      	return(inverse)
    	}
	
	# First checking if the matrix is Invertible
    	data <- y$getMatrix()
	determinant <- det(data)
	if(determinant == 0){
		message <- message("It is a non Invertible Matrix")
		return
	}

	# Solving inverse of the matrix
    	inverse <- solve(data, ...)
    	y$cacheInverse(inverse)
    	inverse
}
