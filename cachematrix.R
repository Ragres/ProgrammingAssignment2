## These functions make an special object that is a matrix but it's inside
## the cache of R and solve the matrix also inside of the R cache


## This function make an objet that's a matrix inside the r cache
## and return it as a list with several "methods" kinda a OOP view


makeCacheMatrix <- function(x = matrix()) {
      m<- NULL
      set<- function(y){
        x <<-y
        m <<-NULL
      }
      get <- function() x
      setinverse<-function(solve) m<<-solve
      getinverse<-function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## this function read the methods of the special object and use them to 
## calculate the inverse of the matrix and store it in the cache
## if it finds that de inverse is already in the cache it prints it out 
## and don't calculate it.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}


##little example
a=matrix(c(1,2,3,4),nrow=2,ncol=2)
z=makeCacheMatrix(a)
cacheSolve(z)
