## Code calculates the inverse of a Matrix and stored & retrieves it from caache
## for future computations. Pulling it from cache - rather than computing it
## each time is advantageous from a time computational perspective

#Prepares cached matrix for future reference (actual matrix is calculated later)

makeCacheMatrix <- function(x = matrix()) {
  #x is a square invertible matrix
  #i is the inverse of matrix x
  i <- NULL
  set <- function(y){
    # double arrow assignment operator "<<"- allows
    # you to modify variables at the parent level
    # rather than the single arrow assignment operator
    # "<-" which us just useful for the current level
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
  

# Calculates the inverse of a matrix, pulling it from the cache, if it was
# previously calculated. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
      #get inverse from cache and skip any computations
      message("get cached data")
      return(i)
  }
  #OR runs the calculation if the inverse is not cached
  data <- x$get()
  i <- solve(data, ...)
  
  #set the inverse in cache for future reference
  x$setinverse(i)
  i
}
