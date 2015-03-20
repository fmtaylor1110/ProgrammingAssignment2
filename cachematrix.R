
## makeCacheMatrix creates and object that will contain four functions that will be used to solve and cache a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get simply returns whatever its input is
  get <- function() x
  ## setmatrix computes the inverse of an imput matrix and stores the value in m in the environment it was defined it
  setmatrix <- function(solve) m <<- solve
  ## getmatrix returns the value of the cached inverted matrix
  getmatrix <- function() m
  #this is the list of functions that will be passed through cacheSolve
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
## getmatrix pulls the value of m which has been assigned to the local environment.  
  ##If cachesolve has been called before it will the inverse of the matrix being passed through
  ##If cachesolve has not been called it woull be null as defined in makeCacheMatrix
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    ##IF m is Null (meaning cacheSolve has not been called before) this pulls the non inverted matrix from where it was stored in 'x'
    mat <- x$get()
    #And inverts it
    m <- solve(mat, ...)
    #and caches the inverted matrix in m where it can be pulled without having to compute the inverse again by cacheSolve
    x$setmatrix(m)
    m
  }
}
