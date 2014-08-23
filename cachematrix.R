## Programming Assignment 2 - to write a pair of functions that cache 
## the inverse of a matrix
        
## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
        
makeCacheMatrix <- function(x = matrix()){       ## Function argument x is a matrix 
        inv <- matrix()                          ## inv is intialized as a matrix
        get <- function(){x}                     ## get() function to return the original matrix
        setinv <- function(solve){inv <<- solve} ## setinv() function to set the result in inv, in this case inverse matrix
        getinv <- function(){inv}                ## getinv() function to fetch the value in stored in inv
        list(get = get,
             setinv = setinv,
             getinv = getinv)
}
        
        
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
        
cacheSolve <- function(x, ...) {                 ## x is the matrix which is passed as function argument
        inv <- x$getinv()                        ## call getinv() to the fetch the inverted matrix, if available
        if(length(inv)>1) {                      ## length(inv)>1 means the inverted matrix is available
                message("getting cached data")
                return(inv)                      ## fetch the cached inverted matrix instead of computing it again 
        }
        data <- x$get()                          ## called when the inverted matrix is not available. Original matrix will be assigned to data
        inv <- solve(data, ...)                  ## solve() calculates the inverted matrix
        x$setinv(inv)                            ## calls setinv() to cache inverted matrix for future use for the same original matrix
        inv                                      ## return inverted matrix
}
