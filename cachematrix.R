## Put comments here that give an overall description of what your
## functions do
##  The solution comprises two functions
##  makeCacheMatrix takes a matrix as input argument
##  and returns a new object in form of a list that comprises
##  the original input matrix as well as methods allowing to compute its inverse
##  cacheSolve() takes as input argumetn the list created by makeCacheMatrix
##  computes and returns its inverse
##
##  The matrix input to makeCacheMatrix is assumed to be invertible


## Write a short comment describing this function
# makeCacheMatrix (x) 
#   input Argument x  : a matrix (assumed to be invertible)
#   Return            : a new object in form of a list including the methods to get/set the inverse
#                     of the matrix supplied as an input argument
##  

makeCacheMatrix <- function(x = matrix()) {


  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##For checking/debugging: allows to retrieve the original matrix from the output list object
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function

## cacheSolve(x,...)
## accepts as input argument a list including the methods defined in function makeCacheMatrix
## if an inverse matrix has been previously calculated and cached, it is returned without recomputing
## if not, the inverse is recalculated and cached
## displays the inverse matrix
## as a variant, the last line can be replaced by 
## return(inv)
##  allowing the output object to be used in further calculations

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("Getting Result from cache")
    return (inv)
  }
  message("Caching Result")
  data <- x$get()
  inv <- solve(data) %*% data
  x$setinv(inv)
  inv
  ## VARIANT
  ## return(inv)
}
