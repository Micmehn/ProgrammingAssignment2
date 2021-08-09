## Put comments here that give an overall description of what your
## functions do

## Makes a Matrix, wich contains its inverse and so safes it in the cache 

makeCacheMatrix <- function(x = matrix()) { 
   inv <- NULL                             
   set <- function(y) {                    
      x <<- y                             
      inv <<- NULL                        
   }
   get <- function() x                     
   
   setinv <- function(inverse) inv <<- inverse  
   getinv <- function() inv                    
   list(set = set, get = get, setinv = setinv, getinv = getinv)   
   
}


## Gives the inverse of Matrix x, but search for it in the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
         message("cached data available")
         return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
