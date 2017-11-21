#############################################################
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of 
## a matrix rather than compute it repeatedly
##
## Two functions are consturcted to complete the inversion:
##
## 1. makeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  im<-NULL
  set<-function(y){
    x<<-y
    im<<-NULL
  }
  get<-function()x
  setinver<-function(inver)im<<-inver
  getinver<-function()im
  list(set=set,get=get,setinver=setinver,getinver=getinver)
}

############################################################
## 2. cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If 
## the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
##
cacheSolve <- function(x, ...) {
        im<-x$getinver()
        if(!is.null(im)){
          message("getting cached data")
          return(im)
        }
        data<-x$get()
        im<-solve(data)
        x$setinver(im)
        im
}
