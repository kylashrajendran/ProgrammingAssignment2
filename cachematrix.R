## Two functions for caching of a matrix inverse
## Stores the inverse to save re-calculation overhead

## makeCacheMatrix: function list to manage cached matrix object

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL # initialise the inverse to NULL
    
    # Define a function to set the value of the matrix
    set <- function(m) {
        x <<- m # updates matrix to m
        inv <-- NULL # resets inv to NULL (since new matrix added)
    }
    
    # Define a function to get the value of the matrix
    get <- function() x
    
    # Define a function that assigns a value to the inverse
    setinv <- function(minv) inv <<- minv
    
    # Define a function to return the inverse
    getinv <- function() inv
    
    # Return a list of the above functions
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve: calculates the matrix inverse if required, and caches it.
## Otherwise, returns the cached matrix

cacheSolve <- function(x, ...){
    # check if the inverse is currently defined
    inv <- x$getinv()
    if(!is.null(inv)){ # inverse already cached, so return
        message("Returning cached inverse")
        return(inv)
    }
    # We reach this point only if the inverse has not been cached (to Null)
    # So we now calculate the inverse and cache it
    X <- x$get() # get the value of the matrix to invert
    inv <- solve(X,...) # calculate the inverse
    x$setinv(inv) # cache the inverse
    inv # return the inverse
}

## testmatrix: Uses 2d rotation matrices to test whether the above functions work

testmatrix <- function(theta){
    matrix(data=c(cos(theta),sin(theta),-sin(theta),+
                      cos(theta)),nrow=2,ncol=2,byrow=TRUE)
}