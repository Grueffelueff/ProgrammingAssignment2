## These are 2 functions, which are able to calculate the inverse of a given matrix
## (provided that it exists - what we will assume) but rather than calculating it every
## time we ask again, the once calculated value is stored and can be retrieved if needed
##later on. The programm will let you know, when it uses the retrieved value.

## The makeCacheMatrix function creates a list of 4 functions given a matrix x.
makeCacheMatrix <- function(x = matrix()) {
  
  n <- NULL ## At first the variable n (which will be the inverse Matrix) is set to NULL
            ## we dont know the inverse of the matrix at this point
  
            ##Then there are created 4 functions
  
  set <- function(y) {  ##the set-function is for, whenever you later want to change
    x <<- y             ##the matrix x to a new matrix y 
    n <<- NULL          ##the inverse(n) must of course be set to NULL again
  }
  
  get <- function() x   ##the get-function just returns you the matrix x
  
  setinverse <- function(inverse) n <<- inverse ##the setinverse-function takes a value
                                                ##(hopefully the calculated inverse)
                                                ##and assigns it to n
  
  getinverse <- function() n                    ##the getinverse-function returns you the
                                                ## value of n
  
  ##a list of these 4 functionss is then created and returned
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##The cacheSolve-function gets a list of 4 functions of the type the 
##makeCacheMatrix-function created. 
##it then either calculates the inverse of the matrix or returns the previously calculated
##and stored inverse
cacheSolve <- function(x, ...) {
  
  n <- x$getinverse() ##n is retrieved
  
  if (is.null(n) == FALSE){ ## This expression being FALSE means, that n has already been
                            ##assigned a value, meaning the inverse has already been
                            ##calculated
    message("I remember doing that before, let me get the cached data...")
    return(n)               ##the n is being returned and the function exited
  }
  
  data <- x$get()           ##when n is NULL then the matrix is assigned to data
  
  n <- solve(data,...)      ##and n is given the value of the inverse matrix
  
  x$setinverse(n)           ##the inverse is now stored in the value n
                            ## remember that setinverse uses the superassigner <<-
  
  n                         ##the inverse is given out without the message
}
