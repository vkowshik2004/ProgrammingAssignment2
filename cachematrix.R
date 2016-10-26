rm(list = ls())
## Put comments here that give an overall description of what your 
## functions do 

## Write a short comment describing this function - this function creates a list containing functions to get the matrix, set the matrix, 
##get the inverse of the matrix & set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
} 

## Write a short comment describing this function - this function calculates the inverse of the list created using the above function. It first checks to see if the inverse
## already exists before computing it. If it does then it fetches it from the cache memory and skips the computation. Else it computes the same.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

a <- matrix(c(1,2,3,4),2,2)
a
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

cachedMatrix <- makeCacheMatrix(x = a)
unCachedMatrix <- cacheSolve(cachedMatrix)
unCachedMatrix