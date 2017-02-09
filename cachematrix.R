# The function gets a matrix and calculates its inverse with the CaseColve function
#after making it a special object by the cacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
  inv_ = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv_ <<- inverse 
  getinv = function() inv_
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The inverse of the matrix is calculates below

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_ = x$getinv()
  # checks if the inverse has already been calculated
  if (!is.null(inv_)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv_)
  }
  # if the above does not hold then calculate the inverse 
  matrix.data = x$get()
  #use the solve function to find the inverse of a square matrix
  inv_ = solve(matrix.data, ...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv_)
  return(inv_)
}
