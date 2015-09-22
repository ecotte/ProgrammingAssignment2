## Programming Assignment 2: Lexical Scoping
## 
## Excercise to make 2 functions.
## First a factory to make a structure with a mtrix
## and the cache of the inverse.
## Second a function to get the cache with the inverse from the structure
## or calculate it in case we don't have it on cache

## Factory to create a structure of a matrix with the data
## and the cache of the inverse.
## Arguments: x - Matrix type
makeCacheMatrix <- function(x = matrix()) {
    solv <- NULL
    set <- function(y) {
        x <<- y
        solv <<- NULL
    } 
    get <- function(){
        x
    }
    setsolve <- function(solve){
        solv <<- solve
    }
    getsolve <- function(){
        solv
    } 
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function to get the cache of the structure or
## calculate it in case it doesn't exist
## Arguments: x - Matrix Structure with cache Inverse
##            ... - Other parameters for the solve function
cacheSolve <- function(x,...) {
    ## Return a matrix that is the inverse of 'x'
    solv <- x$getsolve()
    if(!is.null(solv)) {
        message("getting cached data")
        return(solv)
    }
    data <- x$get()
    solv <- solve(data,...)
    x$setsolve(solv)
    solv
}



## Not part of the Assigment, just testing something different in R

## Factory to create a structure of a matrix with the data
## and the cache of the inverse, when called the inverse it will
## check if it has it on cache and if not, it will calculate
## the result first.
## Arguments: x - Matrix type
makeCacheMatrixOptional <- function(x = matrix()) {
    solv <- NULL
    
    set <- function(y) {
        x <<- y
        solv <<- NULL
    } 
    get <- function(){
        x
    }
    setsolve <- function(solve){
        solv <<- solve
    }
    getsolve <- function(...){
        if (!is.null(solv)){
            message("getting cached data")
            return(solv)
        }
        solv <<- solve(x,...)
        solv
    } 
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}