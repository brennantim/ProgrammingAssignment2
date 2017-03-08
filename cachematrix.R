## creates a list containing a function that 1) sets value of matrix
## 2) get value of matrix, 3) set inverse, 4) get value of inverse

makeCacheMatrix <- function(x = matrix()) {
        
        invervse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse1) inverse <<- inverse1
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)        
}


## returns inverse of a matrix.  Because computation can be time
## consuming, it first checks to see if the inverse has already
## been computed.  If it has, no computation occurs.  If it has
## not, the inverse is computed and cached via setinverse function

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data.")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
