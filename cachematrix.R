## Create a cache for the inverse matrix that can save our computing resource
## on the repeating activity and increase the overall efficiency


## To initiate a matrix for cache and provide functions to set and get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # set initial NULL value to m
    set <- function(y) { # initial a new matrix
        x <<- y # copy input matrix to x
        m <<- NULL # set initial NULL value to m
    }
    get <- function() x # get the original matrix
    setinverse <- function(solve) m <<- solve # save inverse matrix from cacheSolve
    getinverse <- function() m # retrive inverse matrix
    list(set = set, get = get, # create a list of all functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## If the cache is existed, then get it from cache. If not, comput the 
## inverse matrix and save it the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse() # get the cached inverse matrix
    if(!is.null(m)) { # if m exist, we got cache
        message("getting cached data")
        return(m) # return inverse matrix
    }
    data <- x$get() # cache not exist, get matrix
    m <- solve(data) # compute the inverse matrix
    x$setinverse(m) # put inverse matrix to the cache
    m
}
