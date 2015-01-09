#############################################################################
# In this example the <<- operator is introduced, which can be used to assign 
# a value to an object in an environment that is different from the current 
# environment. Below are two functions that are used to create a special 
# "matrix" object and cache its inverse.
#############################################################################

# The first function, makeCacheMatrix, creates a special "matrix" object that 
# can cache its inverse, and is really a list containing a function to:

# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

# The matrix will be passed as argument "x".
# Variable "i" will be used to remember if the inverse matrix has been 
# calculated and cached. 
# "x" and "i" will be persistent using <<- operator, and "i" will be 
# resetted to NULL when creating a new special matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(n) {
        x <<- n
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


# The second function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.
# We'll assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    I<-diag(dim(data)[1])
    i <- solve(data, I,...)
    x$setinverse(i)
    i
}
