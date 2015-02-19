## makeCache Matrix creates a special matrix that can cache its inverse and returns a list

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setSolve<-function(solve) m<<-solve
  getSolve<-function() m
  
  list(set=set, get=get,getSolve=getSolve, setSolve=setSolve)
  
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve  retrieves the 
## inverse from the cache.

cacheSolve <- function(x = matrix(),...){
    m <-x$getSolve()
  if(!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-solve(data,...)
  x$setSolve(m)
  m
}


