#makeCacheMatrix creates a list containing a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix
# 4. get the value fo the matrix


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, then it gets the result and 
# skips the computation. If not, then it computes the inverse, sets the value in
# the cache via setinverse function.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                
                x <<- y
                
                inv <<- NULL
                
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                
                message("getting cached data.")
                
                return(inv)
                
        }
        
        data <- x$get()
        
        inv <- solve(data)
        
        x$setinverse(inv)
        
        inv
        
}

# perform test to call the function with a matrix, compute the inverse, 
# retrieve the inverse from the cache list, change the call matrix to the inverse,
# compute the inverse on that and return the original function

a <- matrix(c(1,2,3,4,5), 1,1)
a1 <- makeCacheMatrix(a)
cacheSolve(a1)
        
#RESULTS !
# cacheSolve(a1)
#[,1]
#[1,]    1

#cacheSolve(a1)
# getting cached data
# [,1]
#[1,]

