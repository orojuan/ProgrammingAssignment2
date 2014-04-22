###These functions are intended to calculate and cache the inverse of a matrix###

#Function to create a special object that stores a matrix and caches its inverse
#(Assumption: Supplied matrix is always invertible).
makeCacheMatrix <- function(mat=matrix()) {
  s <- NULL #Set cache for inverse matrix 
  #Method to change matrix to be stored
  set <- function(new) {
    mat <<- new
    s <<- NULL
  }
  #Method to get stored matrix
  get <- function() mat
  #Method to cache inverse of stored matrix
  setinv <- function(inv) s <<- inv
  #Method to get cached inverse matrix
  getinv <- function() s
  list(set = set, get = get,setinv = setinv,getinv = getinv) #Return list of methods
}

#Function to calculate inverse of a matrix. If it has already been calculated, then fn will
#retrieve cached value
cacheSolve <- function(mat, ...) {
  s <- mat$getinv() #Get cached value for inverse matrix
  #Return inverse matrix if cached
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- mat$get() #Get stored matrix
  s <- solve(data, ...) #Calculate inverse of stored matrix
  mat$setinv(s) #Cache inverse matrix
  return(s) #Return inverse matrix
}