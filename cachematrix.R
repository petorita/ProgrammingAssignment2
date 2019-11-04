## Put comments here that give an overall description of what your
## functions do

# caching the inverse of a matrix rather than compute it repeatedly
#### if it already calculated then just get the inverse
#### it it is not calculate it yet, it calculate it. 


## Write a short comment describing this function


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
# It creates 4 subfunctions:
##### set: set the value of the matrix
##### get: get the value of the matrix
##### setinverse. set the value of the inverse matrix
##### getinverse: get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    }




## Write a short comment describing this function

#The following function calculates the mean of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the matrix in the cache via 
#the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
   
    input_matrix <- x$get()
    
    inv <- solve(input_matrix)
    x$setinverse(inv)
    inv
}
