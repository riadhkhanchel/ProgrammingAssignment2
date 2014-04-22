## Put comments here that give an overall description of what your
## functions do

## These functions cache the inverse of a matrix rather than 
## computing repeatedly

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
  {
  # set the list elements
  # inverse of the matrix is set to NULL
    inverse <- NULL
  #set the value of the matrix
    set <- function (y)
      {
        x <<- y
        m <<- NULL
      }
  #get the value of the matrix 
  get <- function()x

  # set the inverse value
  setInv <- function (inv) inverse <<-inv
  
  # get the inverse
  
  getInv <- function() inverse
  
  #group the 4 elments into a single list
  
  list (set = set, get = get, setInv = setInv, getInv = getInv)
  }

######################################################################################
######################################################################################
## Write a short comment describing this function
## This function computes the inverse of the "special" matrix returned 
## by makeCacheMatrix function. If the inverse has already been calculated 
## (and the matrix has not changed), then CacheSolve retrieves the inverse 
## from the cache. 
######################################################################################
######################################################################################
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  ## Sart by checking if  the inverse was already computed
  inv <-x$getInv()
  if(!is.null(inv))
  {
    message("getting cched data")
    return(inv)
  }
  ## The inverse was not computed, we are going to compute now
  data <-x$get()
  inv <- solve(data,...)
  x$setInv(inv)
  inv
}
