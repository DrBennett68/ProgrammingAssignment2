# This set of funtions allows the user to create a matrix-like oject that works like
# a normal matrix, but also has the capability of calculating its inverse(if square)
# and then caching the result for future reference. 

##########################
# Execution Instructions
##########################
#   Initialize a cacheMatrix object
# > cm <- makeCacheMatrix( matrix(c(1,2,20,30), nrow = 2, ncol = 2))

#   Show the summary of functions for the cacheMatrix object
# > summary(cm)
#   Expected results
#           Length Class  Mode    
#SetMatrix  1      -none- function
#GetMatrix  1      -none- function
#SetInverse 1      -none- function
#GetInverse 1      -none- function

#   Calculate the Inverse the first time (noncached)
# > cacheSolve(cm)
#   Expected results
#[,1] [,2]
#[1,] -3.0  2.0
#[2,]  0.2 -0.1

#   Calculate the Inverse the second time (cached)
# > cacheSolve(cm)
#   Expected results
#getting cached data
#[,1] [,2]
#[1,] -3.0  2.0
#[2,]  0.2 -0.1

####################################################################################
# Author: Jim Bennet
# Created: 10/13/2014
# Purpose:
#    makeCacheMatrix is a factory function that allows the user to create a matrix-like
#    object.  It has the following sub-functions:
#         SetMatrix:   Sets the matrix provided by the user in the parameter agument
#         GetMatrix:   Gets the matrix
#         SetInverse:  stores the Inverse of the matrix in a cache internal variable
#         GetInverse:  Gets the inverse of the matrix either from cache if it exists or
#                      calculates it on the fly if the cache does not exist
# Revisions: (none)
makeCacheMatrix <- function(x = matrix()) 
{
     
     ######################
     # private variables
     ######################
     inverseMatrix <- NULL
     
     
     ################
     #Sub-Functions (Assessors)
     ################
     #Sets the Matrix from the parameter argument
     SetMatrix <- function(y)
     {
          x <- y
          x$SetInverse(NULL) # we have a new matrix so destroy the existing cache
     }
     
     #Gets the matrix
     GetMatrix <- function()
     {
          x
     }
     
     #Set the inverse matrix
     SetInverse <- function(inverse)
     {
          inverseMatrix <<- inverse
     }
     
     #Get the inverse of matrix
     GetInverse <- function()
     {
          inverseMatrix
     }
     
     #This is for the summary function
     list(SetMatrix = SetMatrix, 
          GetMatrix = GetMatrix,
          SetInverse = SetInverse,
          GetInverse = GetInverse) 
}

####################################################################################
# Author: Jim Bennet
# Created: 10/13/2014
# Purpose:
#    cacheSolve is a function that
#    gets the inverse of a matrix created from the makeCacheMatrix function 
#    from cache if it exists or calculates it, caches the result and returns the result
# Revisions: (none)
cacheSolve <- function(x, ...) 
{
     ######################
     # private variables
     ######################
     returnValue <- x$GetInverse()
     
     #Check the cache first to see if we have already calculated it
     if(!is.null(returnValue))
     {
          message("getting cached data")
     }
     else
     {
          # we need to solve the inverse of the matrix
          baseMatrix <- x$GetMatrix()
          
          #check to see if the matrix is square
          if(ncol(baseMatrix) == nrow(baseMatrix))
          {
               # we have a square matrix so calculate the inverse
               returnValue <- solve(baseMatrix)
               x$SetInverse(returnValue)
          }
          else 
          {     
               message("Unable to calculate inverse of matrix")
               returnValue <- NULL
          }     
     }
     returnValue
}
