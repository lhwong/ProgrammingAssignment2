## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.


##  This function creates a special "matrix" object that can cache its inverse.

##  It creates a list containing a function to
##
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ##  <<- operator is used to assign a value to an object in an environment that 
        ##  is different from the current environment. 
        ##  If the matrix has been changed, clear the cache by setting it to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## checks to see if the inverse has already been calculated. 
        ## If so, it gets the inverse from the cache and skips the computation.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## otherwise, calculates the inverse of the data and sets the value of the inverse in the cache via 
        ## the setinverse function.
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}


test <- function() {
        m <- rbind(c(1, -0.25), c(-0.25, 1));
        x <- makeCacheMatrix(m)        
        i <- cacheSolve(x)
        print(i)
        ## get "getting cached data" message indicates that the value is returned from cache
        j <- cacheSolve(x)
        print(j)
}

testSpeed <- function() {
        #matrix(c(19,8,11,2,18,17,15,19,10), nrow = 3)
        set.seed(10000)
        m <- matrix(rnorm(1000000), nrow=1000, ncol=1000) 
        x <- makeCacheMatrix(m)
        
        start <- Sys.time()
        i <- cacheSolve(x)
        timeDiff <- Sys.time() - start
        print(timeDiff) 
        
        start <- Sys.time()
        j <- cacheSolve(x)
        timeDiff <- Sys.time() - start
        print(timeDiff)
} 