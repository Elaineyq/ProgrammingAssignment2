makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
   x <<- y 
   inv <<- NULL
   }
 get <- function() x
 setinverse <- function(inverse) inv <<- inverse
 getinverse <- function() 
   {
   inver <- ginv(x)
   inver%*%x
   }
 list(set = set, get = get, 
      setinverse = setinverse, 
      getinverse = getinverse)
}      


cacheSolve <- function(x, ...) {
 inv <- x$getinverse()
 if(!is.null(inv)) { 
   message("getting cached data") 
   return(inv)
 }
 mat <- x$get()
 inv <- solve(mat, ...)
 x$setinverse(inv) 
 inv 
}

f <- makeCacheMatrix(matrix(1:8,2,4))
f$get() 
