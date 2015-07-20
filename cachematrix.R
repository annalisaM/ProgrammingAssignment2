##  makeCahceMatrix creates a list that consists of functions.
##  There are two 'setters' and two 'getters' as follows:
##  'set' stores the value of the matrix and 'setinversion' its inversion.
##  'get' returns the value of the matrix and 'getinversion' its inversion.
##  For an explanation about the inversion of matrices, see e.g.
##  http://www.purplemath.com/modules/mtrxinvr.htm


makeCacheMatrix <- function(x = matrix()) {
  inversion <- NULL
  set <- function(y) {
    x <<- y
    inversion <<- NULL
  }
  get <- function () x 
    setinversion <- function(inverse) inversion <<- inverse
    getinversion <- function() inversion
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
}


##  CacheSolve does the inversion, thus its output is the inversion
##  of the vector/matrix that was sent to it in the function call
##  NB! In this Coursera assignement, the assumption is that the 
##  matrix is invertible.

cacheSolve <- function(x, ...) {
        inversion <- x$getinversion()
        if(!is.null(inversion)) {
          message("getting cached data")
          return(inversion)
        }
        data <- x$get()
        #  solve(a, b, ...): 
        #  a and b are the vectors or matrices.
        #  if b is missing, solve will return the inverse of a.
        inversion <- solve(data, ...)
        x$setinversion(inversion)
        inversion
}

