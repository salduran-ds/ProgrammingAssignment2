## Put comments here that give an overall description of what your
## Write a short comment describing this function
## This function returns an object that contains 2 matrices and 4 methods.
## The first matrix is a conventional matrix and the second is its inverse.
## The 4 methods (get,set, get_inv, set_inv) are functions to manipulate the
## two matrices, two for setting the values and two for fetching the values.
## The first matrix can be passed as an argument or can be set via the 'set' method.
## The second matrix (inverse) can only be set by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  m_inv<-NULL
  set <-function(y){
    x<<-y
    m_inv<<-NULL
  }
  get <-function() x
  set_inv <-function(inv) m_inv<<-inv
  get_inv <- function() m_inv
  list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## this function takes an object created by makeCacheMatrix()
## It checks for whether the object contains its inverse, if it does, then the function simply returns it.
## If it doesn't, then it will get the matrix of 'x', compute the inverse, store it in 'x'
## and return the inverse back to the caller.
cacheSolve <- function(x, ...) {       
  m_inv<-x$get_inv()
  if(!is.null(m_inv)){
    message("cache-hit")
    return(m_inv)
  }
  matrix<-x$get()
  m_inv<-solve(matrix)
  x$set_inv(m_inv)
  m_inv
}
