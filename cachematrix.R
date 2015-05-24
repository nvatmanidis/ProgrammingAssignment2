## Functions that cache the inverse of a matrix
##
## Usage example:
##
## > source('cachematrix.R')
## > a <- makeCacheMatrix(matrix(c(4, 0, 0, 4), c(2, 2)))
## > cacheSolve(a)
## [,1] [,2]
## [1,]  0.25  0.0
## [2,]  0.0  0.25

##The first function, makeVector creates a special "vector", which is really a list containing a function to


makeCacheMatrix <- function(x = numeric()) {

# initially sets the value of the vector to NULL
m <- NULL

# store a matrix
setMatrix <- function(v) {
  x <<- v

# since the matrix is assigned a new value, flush the cache
m <<- NULL

}

# returns the stored matrix
getMatrix <- function() {
  x
}

# cache the given argument 
cacheInverse <- function(solve) {
  m <<- solve
}

# get the cached value
getInverse <- function() {
  m
}

# return a list. Each named element of the list is a function
list(setMatrix = setMatrix, 
     getMatrix = getMatrix, 
     cacheInverse = cacheInverse, 
     getInverse = getInverse)
}

# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix

cacheSolve <- function(y, ...) {

  # get the cached value
  inverse <- y$getInverse()
  
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  d <- y$getMatrix()
  inverse <- solve(d)
  y$cacheInverse(inverse)
  
  # return the inverse
  inverse
}
