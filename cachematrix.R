## These functions basically work in pairs as  the cacheSolve returns the inverse of the matrix 
## and it only takes the input of matrix  by the makeCacheMatrix and claculaes the inverse of it 
##


## This makeCacheMatrix function first takes x as an argument which is a matrix in this case. Then we set the object "Inverted" to NUll as we will use it to store the inverse of our matrix.
## Set function basically resets the input again as this "<<-" operator assigns the new value of its argument "y" to x in the environment of the function, "makeCacheMatrix"
## it also sets the value of inverted to null to clear out any previous values
## to be noted that the get function does not take x as argument and simply returns the x value from the parent environment
## the function setinverse now takes its argument and assigns to the environment of make CacheMatrix
##get inverse function again returns the value of the object "inverted"
##lastly the list contains all these functions. why? because it will be easier to extract the functions by name

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y){
    x <<- y
    inverted <<- NULL
  }
get <- function() x
 setinverse <- function(solveit)  inverted <<- solveit
 getinverse <- function() inverted
 list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function takes x as input of makeCacheMatrix then next step is to get back x from the get inverse function
## since x was assigned with the double operator, thus caheSolve function is also able to use the x object 
## it checks if the object "inverted" already has a value assigned to it. 
##If it has alreay been calculared , it will return the  object inverted  with the message 
##if not then, the function retrieves the get function which in previous function returned x value and here the matrix
## it then calculates the inverse using the solve function
# it then returns input to the parent environment through set inverse function

cacheSolve <- function(x, ...) {
  inverted <- x$getinverse()
  if(!is.null(inverted)) {
        message("getting cached data")
    return(inverted)
  }
  data <- x$get()
  inverted <- solve(data)
 x$setinverse(data)
 inverted
  
  
}


##Example :

## a <- matrix (1:4,2,2)
## m <- makeCacheMatrix(a)
##m$get()
##        [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## CacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## cacheSolve(m)
## getting cached data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
