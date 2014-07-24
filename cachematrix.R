## functions to calculate the inverse of a matrix, 
## or retreive it from cache if previously calculated


## takes a matrix as it's argument, 
## has private variables x and m
## has public methods to get and set the matrix and its inverse
##which are accessible though the list functionality e.g x$get()

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInvMatrix <- function(invMatrix) m <<- invMatrix
     getInvMatrix <- function() m
     list(set = set, get = get,
          setInvMatrix= setInvMatrix,
          getInvMatrix = getInvMatrix)

}


## function to calculate the inverse of a matrix, or recall it from cache
## if previously calculated

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m<-x$getInvMatrix()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     data<-x$get()
     m<-solve(data)
     x$setInvMatrix(m)
     m
}
