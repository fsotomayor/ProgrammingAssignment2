#this function creates an object with 4 functions inside
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #print the content of matrix "x"
  get <- function() x
  #allows object to calculate the inverse of matrix "x"
  setInv <- function(solve) m <<- solve
  #NULL if the inverse of matrix "x" has not been calculated yet
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## x is an object defined by function makeCacheMatrix. This object has 4 functions inside.
#Using the function setInv is possible to calculate the of matrix "m"
cacheSolve <- function(x, ...) {
  #"m" is NULL if the inverse has not been calculated yet
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #the matriz is assigned to varible "data"
  data <- x$get()
  #calculating the inverse of matrix "data"
  m <- solve(data)
  x$setInv(m)
  #return the inverse matrix
  m
}
