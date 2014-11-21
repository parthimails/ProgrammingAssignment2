## There are two functions 'makeCacheMatrix' and 'cacheSolve' - these two functions are created
## for understanding the cache in R. As described in the assignment description we may need to 
## store values in objects during complex calculations / functions. makeCacheMatrix function has
## four functions, getmatrix, setmatrix, getinverse and setinverse and this functions returns 
## four functions returned by makeCacheMatix. cacheSolve uses these objects if the value is availble
## for calculating the inverse of the matrix, if not it calculates the inverse and set the value
## in makeCacheMatrix.


## makeCacheMatrix function has four functions, getmatrix, setmatrix, getinverse and setinverse
## and this functions returns four functions in the list - it gets a matrix as input.


makeCacheMatrix <- function(x = matrix()) #getting matrix as the input
  {
    inverse <- NULL                       #making the inverse of the matrix as NULL
    
    getmatrix <- function ()              #Function which returns the input matrix
    {
      x
    }
    
    setmatrix <- function(y)              #Function which sets the new value for the matrix
    {
      x <<- y
      inverse <<- NULL
    }
    
    setinverse <- function(xinverse)      #If the inverse is not calculated - cachematrix calculates the inverse and calls
    {                                     #this function to set the inverse value for the matrix 
      inverse <<- xinverse
    }
    
    getinverse <- function()              #If the inverse is already calcualted this function is called by cachematrix 
    {                                     #to get the matrix inverse
      inverse
    }
    
    list(a=getmatrix,b=setinverse,c=getinverse, d=setmatrix) #list of functions returned
  }


## cacheSolve is the main function and has the base logic, it checks with makeCacheMatrix if the value is already cached
## or not, if the value is cached already it uses it otherwise it calculates the value and sets the value
## in makeCacheMatrix. This function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  
  inverse <- x$c()                      #Gets the inverse from the object returned by makeCachematrix
  
  if(!is.null(inverse))                 #Checks if the matrix is null or not
  {
    message(" inverse calculated for this matrix and its cached - Retreiving inverse from makeCacheMatrix")
    
    return(inverse)                     #if not null it returns the inverse, which means the inverse value is already calculated
  }                                     #and it returns the value
  
  matrix <- as.matrix(x$a())            #if the inverse is NULL get the new matrix
  
  inverse <- solve(matrix)              #Calculate the inverse of the matrix by calling solve function
  
  x$b(inverse)                          #Set the inverse in cache by calling setinverse function 
  
  inverse                               #Returns the inverse of the matrix
}
