## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly,
##  the below functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        ## Create a new variable and define the cache as m      
        m <- NULL
        
        set <- function(y) {
                ## assign the input matrix y to the variable x in the
                ## parent environment
                x <<- y
                ## re-initialize m in the parent environment to null
                m <<- NULL
        }
        ## return the matrix x
        get <- function() x
        ## set the cache m equal
        ## to the inverse of the matrix x
        setinverse <- function(inverse) m <<- inverse
        ## return the cached inverse of x
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse matrix of the special "matrix"
## created with the above function. However, it first checks to see if the 
## inverse matrix has already been calculated. If so, it gets the inverse matrix
## from the cache and skips the computation. Otherwise, it calculates the 
## inverse matrix of the data and sets the value of the inverse matrix in the 
## cache via the setinverse matrix function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## checks if m has an existing value. If TRUE, then return that value.
        if(!is.null(m)) {
                message("getting cached data")
                ## If there's no existing value, this function will calculate 
                ## it below
                return(m)
        }
        ## Calls get() function from x, which won't have an m value
        data <- x$get()
        ## Computes inverse matrix from the retrieved values via x$get()
        m <- solve(data, ...)
        ## With the new inverse matrix, setinverse() will be called to update
        ## m in makeCacheMatrix()
        x$setinverse(m)
        ## Prints the new inverse matrix
        m
}



## sample output verified based on inputs from forum
## thank all members of the forum
## > a<-makeCacheMatrix(matrix)
## > a<-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
## > a$get()   # Returns original matrix
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(a)  # Computes, caches, and returns    matrix inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > a$getinverse()  # Returns matrix inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(a) # Returns cached matrix inverse using previously computed matrix inverse
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##  # Modify existing matrix
## > a$set(matrix(c(0,5,99,66),nrow=2,ncol=2))
## > cacheSolve(a)  # Modify existing matrix
## [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0
## > a$get()  # Returns matrix
## [,1] [,2]
## [1,]    0   99
## [2,]    5   66
## > a$getinverse()  # Returns matrix inverse
## [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0
## To validate your code is to multiply your matrix by its inverse and show 
## that the result is indeed the Identity matrix.we can see that the diagonal
## is 1 and we have 0 everywhere else.
## > a$get()%*% cacheSolve(a)
## getting cached data
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
 