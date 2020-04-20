#This function creates a special matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrixInverse<-function(solve) m<<- solve
  getmatrixInverse<-function() m
  list(set=set, get=get,
       setmatrixInverse=setmatrixInverse,
       getmatrixInverse=getmatrixInverse)
}


## Returns the inverse of the x matrix 

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrixInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data) %*%  data 
  x$setmatrixInverse(m)
  m
}
