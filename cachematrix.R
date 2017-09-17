#The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse

#1.set the matrix
#2.get the matrix
#3.set set the inverse of the matrix
#4.get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

# makeCacheMatrix:
# This function creates a special "matrix" object that can cache its inverse.
# To facilitate this caching, you first create a special matrix that will help us with this by using the
# makeCacheMatrix function.  The input into this function is simply a variable of type matrix.

# Initially set to NULL
# Changes when the user sets the value
    inv <- NULL

    # set function 
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
      
     # get function 
      get <- function() x
      
      # set the inverse
       setInverse <- function(inverse) inv <<- inverse
     
      # Get the inverse
      getInverse <- function() inv

      # Encapsulate into a list   
      list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Get the current state of the inverse and see if it
    ## has been computed yet
        
    inv <- x$getInverse()

    ## If it has...
    if(!is.null(inv)) {
    	# Simply return the computed inverse		
            message("getting cached matrix")
            return(inv)
    }

    # If it hasn't...
    # Get the matrix itself
    data <- x$get()

    # Find the inverse
    inv <- solve(data, ...)

    # Cache this result in the object
    x$setInverse(inv)

    # Return this new result
    inv    
}

##Testing My Functions
x <-matrix(c(4,2,7,6),2,2)
my_matrix <- makeCacheMatrix(x)
my_matrix$get()
     [,1] [,2]
[1,]    4    7
[2,]    2    6
my_matrix$getInverse()
NULL
cacheSolve(my_matrix)
     [,1] [,2]
[1,]  0.6 -0.7
[2,] -0.2  0.4
cacheSolve(my_matrix)
 getting cached matrix
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
my_matrix$getInverse()
     [,1] [,2]
[1,]  0.6 -0.7
[2,] -0.2  0.4

 


ls(environment(funs$set))
[1] "get"        "getInverse" "inv"        "set"        "setInverse" "x"
# As you see not only are the four functions in this environment, but also the x and inv objects (a consequence of using <<-). 
