## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL                              
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                            
        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                      #get the value of the invertible matrix                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
    if(!is.null(inv)) {                  #if inverse matrix is not NULL
        message("getting cached data")   #Type message: Getting Cached Data
        return(inv)                      #return the invertible matrix
    }
    data <- x$get()                      #get the original Matrix Data 
    inv <- solve(data, ...)               #use solve function to inverse the matrix
    x$setinverse(inv)                     #set the invertible matrix 
    inv
        ## Return a matrix that is the inverse of 'x'
}
