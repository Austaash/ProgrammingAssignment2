##This function basically stores the matrix in cache along with the other things mentioned below.
#storing the matrix in cache
{
  m <- NULL                      
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x                        #get the value of the matrix
  setInverse <- function(solve) m<<- solve   #set the value of the inverse
  getInverse <- function() m                 #get the value of the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#m1<-makeCacheMatrix(x) 

##This function calculates the inverse of the matrix from the cache.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cacheSolve will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  m <- x$getInverse()                     #check the cache
  if(!is.null(m)) {
    message("obtaining data from cache")
    return(m)                              #return the value from cache
  }
  data <- x$get()
  m <- solve(data, ...)                   #solve() calculates the inverse of the matrix
  x$setInverse(m)                         #storing the inverse into cache
  m
}
## one should make sure that the return value of the makeCacheMatrix is passed to cacheSolve()
## and not the matrix as it is.