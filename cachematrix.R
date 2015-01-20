## the first function below is used to create special matrices that can store their inverse
## so that the second function can retrieve the value of the inverse without computing it again.



## makeCacheMatrix creates a "special matrix" that can cache the value of its inverse 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      
      get <- function() x
      
      setinverse <- function(inv) i <<- inv
      
      
      getinverse <- function() i
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## cacheSolve takes a "special" matrix as argument and 
##either returns its inverse stored in the cache 
##or computes the inverse and stores the result in the cache memory  

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      if(!is.invertible(data)){
            return(NaN)
      }
      i <- solve(data, ...)
      x$setinverse(i)
      i
}

##is.invertible evaluates whether or not a matrix is invertible. Returns true if it is.

is.invertible <- function(m){
      if(nrow(m)==ncol(m)){
            if(!is.null(det(m))){
                  return(TRUE)         
            }
            else return(FALSE)
      }
      else return(FALSE)
}