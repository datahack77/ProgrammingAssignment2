##The R functions below can be used to cache inverses of matrices and avoid unnessary
##time-consuming computations.  The cache mechanism is implemented by taking advantage
##of R's scoping rules and the <<- operator that allows variable assignments to be made
##outside of a function's environment and inside the function's parent (enclosing)
##environment.  Note that these functions assume that the input matirx m is invertible!


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

     inv <- NULL     
     set <- function(y) { #make assignments in the makeCacheMatrix environment. 
          x <<- y  
          inv <<- NULL
     }     
     get <- function() x
     setInv <- function(t) inv <<- t #make assignment in the makeCacheMatrix environment
     getInv <- function() inv
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
     
     
}#end function makeCacheMatrix


#This function computes the inverse of the special "matrix" returned by
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve will retrieve
#the inverse from the cache.

cacheSolve <- function(x, ...) {      

     inv <- x$getInv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...) #compute the matrix inverse
     x$setInv(inv)
     inv   # Return a matrix that is the inverse of 'x'
     
}#end function cacheSolve

