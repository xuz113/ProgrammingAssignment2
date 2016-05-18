## MakeCacheMatrix(x) is used to generate special "matrix" and return a list contaning 4 functions
## set: to set the matrix
## get: to retrieve the matrix
## setMinv: to set the inverse of the matrix
## getMinv: to retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        MInv <- NULL
        set <- function(y) {
                x <<- y
                MInv <<- NULL
        }
        get <- function() x
        setMinv <- function(MinvFunc) MInv <<- MinvFunc 
        getMinv <- function() MInv 
        list(set = set, get = get,
             setMinv = setMinv,
             getMinv = getMinv)
}


## cacheSolve is a function for the cache or calcualte the inverse of matrix, based on whether the inverse of matrix exists or not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MInv <- x$getMinv()
        if(!is.null(MInv)) {
                message("getting cached data")
                return(MInv)
        }
        data <- x$get()
        MInv <- solve(data, ...)
        x$setMinv(MInv)
        MInv
}

## below shows some example about how to use those functions

## let us set a matrix first ( square matrix only so that inverse of matrix exists)

## MT <- matrix(c(1,2,3,4), 2, 2)
## MT
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## let us generate the special matrix here
## MTMake <- makeCacheMatrix(MT)
## MTMake
## $set
## function (y) 
## {
##     x <<- y
##     MInv <<- NULL
## }
## <environment: 0x114cc9430>

## $get
## function () 
## x
## <environment: 0x114cc9430>

## $setMinv
## function (MinvFunc) 
## MInv <<- MinvFunc
## <environment: 0x114cc9430>
## 
## $getMinv
## function () 
## MInv
## <environment: 0x114cc9430>
## now let us use CacheSolve to get the inverse of matrix
## MInv <- cacheSolve(MTMake)
## MInv
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## if we call CacheSolve again, the function will not calculate, but cache (show the msg, "getting cached data") the inverse of matrix since it already exists
## MInv <- cacheSolve(MTMake)
## getting cached data
## MInv
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## to calculate another matrix (MT1 here), use set function from makeCacheMatrix, this will reset the inverse to NULL and new matrix
## MT1 <- matrix(c(-2, 1, 1.5, -0.5), 2, 2)
## MTMake$set(MT1)
## cacheSolve(MTMake)
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## one can see the inverse is for the new matrix calculated again
