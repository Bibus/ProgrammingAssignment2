## the function makeCacheMatrixThese functions allow to put 
## the inverse of a matrix in the cache.
## If the inverse is in the cache the function cacheSolve returns
## it. Otherwise it calculates the inverse and sets in in the cache

## This function creates a vector which is a list of functions to
## 1. set a matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse

makeCacheMatrix <- function(M = matrix()) {
        I <- NULL
        set <- function(y) {
                M <<- y
                I <<- NULL
        }
        get <- function() M
        setinverse <- function(Inv) I <<- Inv
        getinverse <- function() I
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## If the inverse is in the cache this function returns it. Otherwise
## it calculates the inverse, stores it in the cache and returns it

cacheSolve <- function(M, ...){
		## returns a matrix that is the inverse of M
        I <- M$getinverse()
        if(!is.null(I)){
                message("getting inverse from cache")
                return(I)
        }
        matr <- M$get()
        I <- solve(matr,...)
        M$setinverse(I)
        I
}
