## Caching the Inverse of a Matrix: This is assignment 2 
## which aims to teach the user about Lexical Scoping. 
## This script is made of two functions;
## [I] "makeCacheMatrix" function: Creates special list which contains a function to
##		 1:set the value of the matrix
##		 2:get the value of the matrix
##		 3:set the value of the inverse matrix
##		 4:set the value of the inverse matrix
## [2] "cacheSolve" function: Calculates the inverse 
##  	matrix of our "special matrix" from above, but 
##		it first checks to see if the inverse matrix has 
##		been resolved. If so it calls the value from the cache
## Inverse function is a function where if it is multiplied by 
## original matrix the resulting matrix is the identity matrix. 
## See: http://www.mathsisfun.com/algebra/matrix-inverse.html

##[1] the first function: creates special list.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL				#sets m to NULL
  set <- function(y) {	#caches newly set matrix and caches m back to Null
    x <<- y
    m <<- NULL
  }
  get <- function() x #This is a simple function with no variables that returns the users input
  setinversematrix <- function(solve) m <<- solve #
  getinversematrix <- function()
  list(set = set, get = get, 				#list of functions to get and set matrices
       setinversematrix = setinversematrix,	#that can be called upon by the user
       getinversematrix = getinversematrix)
}


##[2] the second function Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinversematrix()
  if(!is.null(m)) { #checks if m value is not "null". If true then it returns cached m
    message("getting cached data")
    return(m)			
  }
  data <- x$get()		#calls upon the users input matrix for the makeCacheMatrix function
  m <- solve(data, ...) #calculates the inverse matrix using "data" and assigns it to m. How does it know if m is cahed or not?
  x$setinversematrix(m) #caches m
  m						#calls m for the display.
}
