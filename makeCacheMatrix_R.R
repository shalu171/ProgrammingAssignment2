1. makeCacheMatrix <-function(x = matrix(){
2. inv <- NULL
3. set <- function(y){
4.        x <<- y
5.        fnv <<-NULL
6.     }
7. get <- function() {X}
8. setInverse <- function(inverse) (inv <<- inverse)
9. getInverse <- function() {inv}
10. list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
11. }
12. 
13. cacheSolve <-function(x, ...){
14.    inv <-x$getInverse()
15.  if(! is.null(inv)){
16.    message("getting cached data")
17.    return(inv)
18.  }
19.  mat <- x$get()
20.  inv <- solve(mat, ...)
21   x$setinverse(inv)
22.  inv 
23.   }
