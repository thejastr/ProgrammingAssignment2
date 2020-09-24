### R Programming Week 3 Programming Assignment 2
### lexical scoping

### Matrix inversion is usually a costly computation and there may be some benefit to 
### caching the inverse of a matrix rather than compute it repeatedly. 
### Your assignment is to write a pair of functions that cache the inverse of a matrix.

### Note: The matrix supplied is always invertible.

### This program contains two functions that are used to createa special object
### that is able to store a matrix and caches it inverse


### The first function This function creates a special "matrix" object that can 
### cache its inverse. 
### Here we create a function that can 
### - set the value of the matrix
### - get the value of the matrix
### - set the value of the inverse
### - get the value of the inverse
### with help of the  <<- operator we can assign a value to an object in an environment
###  that is different from the current (here: working) environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<- function(y) {
    x <<- y
    m <<- NULL
  }
  get        <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(    set        = set
           , get        = get
           , setinverse = setinverse
           , getinverse = getinverse)
}


### This function computes the inverse - using the solve function - of the special 
### "matrix" created by makeCacheMatrix above. If the inverse has already been 
### calculated and is used again on the same matrix, then the inverse is retrieved 
### from the cache.In the output window you can see the note "getting cached data".

cacheSolve <- function(x,...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}