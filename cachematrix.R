## "makeCacheMatrix", for a given matrix creates a list 
## consisting of four objects:
## 1. a function which set the value of the matrix
## 2. a function which get the value of the matrix
## 3. a function which set the inverse of the matrix
## 4. a function which get the inverse of the matrix


makeCacheMatrix <- function(x=matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## "cacheSolve" return a matrix that is the inverse of the matrix
## given by the "get" function from the list given by "makeCacheMatrix",
## but it first check if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the inverse 
## in the cache via the "setinv" function.


cacheSolve <- function(x, ...) {
        
        i <- x$getinv()
        
        if(!is.null(i)){
        message("getting cached data")
        return(i)
        }
        
        data <- x$get()
        i<- solve(data, ...)
        x$setinv(i)
        
        i
}