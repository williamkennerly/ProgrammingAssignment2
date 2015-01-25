#  Project 3 for R Programming at Coursera (January 25, 2015)

#These two functions work together to make a special matrix-like object that also includes a cached inverse of that matrix.
#First you define some typical R square invertible matrix in whatever symbol you want.  
#Then create the new special matrix-like object as a result of makeCacheMatrix(x)
#Then when you want to know the inverse of x, call cacheSolve(myMatrixObject)
#the first time it will calculate the inverse and store it inside the special 
#the second time it will not recalculate it, instead it will return the cached version and print a notifcation to screen


#Then create the new special matrix-like object as a result of makeCacheMatrix(x)
#  call this as:    myMatrixObject <- makeCacheMatrix(x)
#  where x is a regular R matrix (square, invertible)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL      #initialize inverse matrix to NULL
  set <- function(y) {    # set the parent environment's matrix to a new value (not actually used here)
    x <<- y
    inv <<- NULL       #will also reset the cache flag if called (i.e. if matrix x changes)
  }
  get <- function() x    #just get the matrix to be inverted
  setinverse <- function(inversedummy) inv <<- inversedummy    #set the inverse of the matrix x as the formal argument, change in parent environment
  getinverse <- function() inv   #simply get the inverse matrix 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)   #this function returns this list of four functions to be part of the matrix "object"
}

#Now can efficiently calculate or retrieve the inverse of the matrix by calling this as
#  cacheSolve(myMatrixObject)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()   #get the current inverse matrix 
    if(!is.null(inv)) {     #if it is NOT a NA value, then don't recalculate it, and return it
      message("getting cached data")
      return(inv)
    }
    data <- x$get()   #otherwise if inverse is not cached... copy the original matrix to 'data'
    inv <- solve(data,...)    #here's where you actually invert the matrix
    x$setinverse(inv)     #now set the inverse matrix in the parent environment
    inv    #Now return the inverse matrix
  
}
