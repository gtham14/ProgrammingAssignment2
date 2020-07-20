## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<-y
    inv<- NULL
  }
  get<- function(){x}
  
  
  getInverse<- function(){inv}
  setInverse<- function(inverse){
    inv<<- inverse
  }
  
  list(get=get,set=set,getInverse=getInverse,setInverse=setInverse)
  

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getInverse()
  if(!is.null(inv))
  {
    message("inverse already exists")
    return(inv)
  }
  
  mat<- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}
