

## MakeCacheMatrix is a function that stores a list of functions. 
## You can use the listed functions in this function by using '$'
## The first function in the list is 'set'. This function is able to change the matrix stored in the main function.
## The second function, get, returns the matrix x stored in the main function.
## The third function 'setinverse' is similar to 'set' en 'getinverse' is similar to 'get'.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                                x <<- y 
                                m <<- NULL
                        }
                get <- function() x
                setinverse <- function(inverse) m <<-inverse
                getinverse <- function () m
                list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
                 }

## CacheSolve has the object where makeCacheMatrix is stored as input. 
## Step 1: check if m is null. 
## Step 2: If m is not null, then m is returned. This ends the function. 
## Step 3: If m is null, the matrix stored with makeCacheMatrix is stored in 'data'. 
## Step 4: The inverse of the matrix in 'data' is calculated with the 'solve' function. 
## Step 5: x$setinverse(m) stores the result in the object that contains the makeCacheMatrix function.  
## Step 6: The result is shown. 


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <-  solve(data,...)
        x$setinverse(m)
        m
}

