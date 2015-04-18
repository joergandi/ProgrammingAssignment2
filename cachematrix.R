## Compute matrix inverse with caching; assumes matrix always invertible (square, full rank)

## builds a data structure containing matrix data, inverse, and access functions to both
## usage example:
### N<-100
### M<-100
### a<-matrix( rnorm(N*M,mean=0,sd=1), N, M) 
### a_cached<-makeCacheMatrix(a)

makeCacheMatrix <- function(x = matrix()) {
  #i: internal storage for inverse
  i <- NULL
  #set source matrix
  set <- function(y) {
    #x: internal storage for inverse, init to NULL
    ##NOTE "<<-" operator to lookup scope of target variable in parent environments (caller)
    x <<- y
    i <<- NULL
  }
  #get source matrix
  get <- function() x
  #set inverse of source matrix, computed by caller
  setinv <- function(inv) i <<- inv
  #get inverse of source matrix, or NULL if not yet cached
  getinv <- function() i
  #return data structure with matrix data, inverse data cache, and access functions for both
  #note: returns function handles 
  l<-list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  return(l)
}


## Computes matrix inverse (assumes matrix invertible, square fullrank) with caching
## usage example:
### N<-100
### M<-100
### a<-matrix( rnorm(N*M,mean=0,sd=1), N, M) 
### a_cached<-makeCacheMatrix(a)
### a_inv<-cacheSolve(a_cached)

cacheSolve <- function(x, ...) {
  # check if cache datastructure has been provided as input; if not, print warning and fallback to uncached op
  if (!(("getinv" %in% names(x)) &
        ("setinv" %in% names(x)) & 
        ("get" %in% names(x))    & 
        ("set" %in% names(x)) ) ) {
    message("Warning: input not of makeCacheMatrix list type; fallback to uncached inverse")
    # invert matrix
    i <- solve(x, ...)
    return(i)
  } 
  #check if cache contains inverse
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #get source matrix to invert from datastructure
  data <- x$get()
  #invert matrix
  i <- solve(data, ...)
  #cache inverse in datastructure
  x$setinv(i)
  #return inverse
  return(i)
}
