
makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setSolve<-function(solve) m<<-solve
  getSolve<-function() m
  
  list(set=set, get=get,getSolve=getSolve, setsolve=setSolve)
  
}


cacheSolve <- function(x = matrix(),...){
    m <-x$getSolve()
  if(!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-solve(data,...)
  ##x$setSolve(m)
  m
}

