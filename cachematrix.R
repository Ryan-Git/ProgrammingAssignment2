## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of 
## a matrix rather than compute it repeatedly.
## This pair of functions is write to cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInversion <- function(inversion) inv <<- inversion
  
  getInversion <- function() inv
  
  list(set = set, get = get, 
       setInversion = setInversion,
       getInversion = getInversion)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInversion()
  
  if (!is.null(inv)){
    ## It's already cached.
    return(inv)
  }
  
  data <- x$get() #get row matrix
  inv <- solve(data) #calculate inversion
  x$setInversion(inv) #cache the inversion
  
  inv
}
