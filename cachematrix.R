## Assignment 2 for R Programming on Coursera
## Written by Peter Dirsa
##
## This assignment is the implementation of a special "matrix" object 
## with the makeCacheMatrix function.  This Object can then be inverted
## by the cacheSolve function.  If the matrix has not changed then the 
## cached solution is returned after the first run.  This will save time, 
## making repeated calls to this Matrix object more efficent.
##
## The logic for these functions is bases on the examples listed in the 
## assignment for caching vectors on the Assignment 2 page:
## https://class.coursera.org/rprog-006/human_grading/view/courses/972578/assessments/3/submissions
##
##  Example of use:
##
##  source ("cachematrix.R")
##  oldMatrix <- rbind(c(1, -0.25), c(-0.25, 1))
##  newMatrix <- makeCacheMatrix(oldMatrix)
##  invMatrix <- cacheSolve(newMatrix)
##  inv2Matrix <- cacheSolve(newMatrix)
##



## This function creates a special "matrix" object 
## that can cache its inverse. It is used in the cacheSolve function. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setsolution <- function(solution) m <<- solution
  getsolution <- function() m
  list(set = set, get = get,
       setsolution = setsolution,
       getsolution = getsolution)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## if the inverse has already been calculated (and the matrix has not changed)
## Note this function can only accept makeCacheMatrix objects.  Passing a base style matrix
## will result in an error

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolution()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolution(m)
  m
  
}
