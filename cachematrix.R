## Coursera > R programming > Week 3 > Programming Assignment 2: Lexical Scoping.
## Written here are a pair of functions that calculate the inverse of a matrix and cache it.

## makeCacheMatrix: This function creates a special "matrix" object... 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setInv<-function(inverse)inv<<-inverse
  getInv<-function()inv
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned...
## by makeCacheMatrix above. If the inverse has already been calculated... 
## (and the matrix has not changed), then the cachesolve... 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv<-x$getInv()
  if(!is.null(inv)){
    message("Getting cached results")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setInv(inv)
  inv
}
