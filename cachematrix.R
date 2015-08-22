## These two functions are designed to speed up calculstions that involve a matrix and its inverse
## by storing the inverse of the matrix on its first calculation and simply returning this instead
## of recalculating its value each time. If the matrix stored in this object is changed, the next call
## will recalculate the inverse

## makeCacheMatrix creates an object that can store a matrix and the corresponding inverse matrix. 
## This will reject any inputs that are not matrices

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   setmatrix <- function(matr)
   {
      if(is.matrix(matr)) ## checks passed value is a matrix before setting
      {
         x <<- matr ## caches matrix 
         inv <<- NULL ## sets inverse to NULL, so inverse will be calculated on next call of cacheSolve
      }
      else
      {
         warning("Passed argument is not a matrix")
      }
   }
   
   getmatrix <- function() 
   {
      return(x)
   }
   
   setinv <- function(inverse)
   {
      if(is.matrix(inverse)) ## checks passed value is a matrix before setting
      {
         inv <<- inverse
      }
      else
      {
         warning("Passed argument is not a matrix")
      }
   }
   
   getinv <- function() 
   {
      return(inv)
      
   }   
   list(setmatrix = setmatrix, getmatrix = getmatrix, 
        setinv = setinv, getinv = getinv)
   
}


## cacheSolve takes a matrix and returns its inverse. It first checks to see if the inverse has alreasy been
## calculated, if so it simply returns this. If not it will calculate the inverse, store the value in the cache
## then return the value

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv() ## gets value of inverse in cache
   if(!is.null(inv))  ## checks if inverse value is set
   {
      return(inv) ## if inverse has been calculated returns the cached value
   }
   else
   {
      inv <- x$setinv(solve(x$getmatrix(), ...)) ## calculates inverse, stores to cache and passes to variable for return
      return(inv)
   }
   
}
