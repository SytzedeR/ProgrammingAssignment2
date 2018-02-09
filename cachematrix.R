## Matrix inversion with storing the results of the inversion 
## in a cache.

##     makeCacheMatrix:
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse matrix
##     get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y) {
            x<<-y
            m<<-NULL
      }
      get <-function() x
      setinv<-function(solve) m<<-solve
      getinv<-function() m
      list(set=set, get=get, setinv=setinv, getinv=getinv)
      
}

## cacheSolve:
## Calculate the inverse of the matrix, or if already calculated retrieve
## the inverse matrix from cache.

cacheSolve <- function(x, ...) {
      m<-x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data<-x$get()
      m<-solve(data, ...)
      x$setinv(m)
      m
}
