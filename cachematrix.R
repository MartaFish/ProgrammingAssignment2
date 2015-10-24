## This file is saved in ProgrammingAssignment2 in Git
## create a matrix object and then cache its inverse

## The first function creates a matrix
makeCacheMatrix <- function(x = matrix()) {    
  mtx <- NULL                   #set value of the matrix to NULL
  set <- function(y){          
    x <<- y
    mtx <<- NULL          #define a function to reset the matrix
  }
  get <- function() x           #define a function to retrieve the matrix
  setinverse <- function(inverse) mtx <<- inverse   #define a function to reset the inverse matrix
  getinverse <- function() mtx  #define a function to retrieve the inverse matrix
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse) #initialize a list that allows you to access all the functions
}


## the second function uses the solve function to create the inverse matrix 
## and caches that matrix

cacheSolve <- function(x, ...){              
  inv <- x$getinverse()               #define the inverse
  if(!is.null(inv)){                  #if the inverse already has a value
    message("getting cached data")   #tell the user we are using the cached matrix,
    return(inv)                 #return that cached matrix, and exit the function
  }
  data <- x$get()            #get the matrix from the makeCacheMatrix function
  inv <- solve(data, ...)    #calculate the matrix inverse
  x$setinverse(inv)          #sets the inverse matrix using the command from makeCacheMatrix
  inv                        #print the inverse matrix
}