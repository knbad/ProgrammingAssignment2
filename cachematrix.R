## Matrix inversion is usually a costly computation. Caching the inverse of 
## a matrix rather than computing it repeatedly may save valuable computation time.
## This program takes the data in the form of a vector, converts it
## into a square matrix, gets its inverse and caches it. If same matrix 
## is again given, it takes the inverse from cached data rather than 
## re-computing  it.

# makeCacheMatrix
# Creates a special "matrix" object that can cache its inverse.
# The object does not calculate the inverse, just saves it inside.
# Takes the data in the form of a numeric vector x and converts 
# it into a square matrix. Saves the matrix to variable m and its
# inverse to variable s.
# Returned object ( a list) contains methods:
# setmat: sets matrix and resets cached inverse
# getmat: returns matrix
# setSolve: saves solve value
# getSolve: returns cached inverse value.


makeCacheMatrix <- function(x = numeric()) {
    n<-sqrt(length(x))                # sets the dimension of the matrix
    m <- matrix(x, n, n)              # creates matrix
    s <- NULL                         # creates placeholder for inverse matrix
    setmat <- function(y) {           
      m <<- y
      s <<- NULL
    }
    getmat <- function() m
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(setmat = setmat, getmat = getmat,
         setinv = setinv,
         getinv = getinv)
}


#cachesolve
# Function to get the inversed matrix from a special object created by makeCacheMatrix.
# Takes the object of that type as an argument 'x', checks if the inverse value is already
# cached, and if it is returns the cached value; if not, this function calculates the
# inverse for the matrix saved in the 'x', saves it into 'x' cache using method 'setSolve'
# and returns the result.



cachesolve <- function(m, ...) {
  s <- m$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)                       # returns the cached value if available.
  }
  data <- m$getmat()                # takes the data from m matrix and 
  s <- solve(data, ...)             #calculates it inverse, if not already catched.
  m$setinv(s)
  s
}



