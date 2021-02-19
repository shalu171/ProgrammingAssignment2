##There are two function makeCacheMatrix.makeCacheMatrix
##makeCacheMatrix consists of set, get, setinv, getinv
##library(Mass) is used to calculate inverse of non squared as well as square matrices
library(MASS)
makeCacheMatrix <-function(x = matrix()){
  inv <-NULL     #initializing inverse as NULL
  set <-function(y){
    x <<- y
    inv <<- NULL
  }
  get<-function()x   #function to get matrix x   
  set <- function(inverse){inv <<- inverse}
  getInverse <- function() { 
          inver<-ginv(x)
          inver%*%x     #function to obtain inverse of the matrix
  }
  
  list(set = set, get = get, setinverse = setinverse, getInverse = getInverse)
  
}
##Write a short comment describing this function 
##This is used to get the cache data

cachsolve <-function(x,....)#gets Cache data() 
{
  inv <- x$getInverse()
  if(!is.null(inv))  #checking whether inverse is Null
    {
           message("getting cached data")
           return(inv)   #returns inverse value
  }
  data <-x$get()
  inv <-solve(data,...)   #calculates inverse value
  x$setinverse(inv)
  inv  ##Return a matrix that is the inverse of 'x'
}

  