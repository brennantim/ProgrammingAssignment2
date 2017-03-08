## creates a list containing a function that 1) sets value of matrix
## 2) get value of matrix, 3) set inverse, 4) get value of inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)        
}


## returns inverse of a matrix.  Because computation can be time
## consuming, it first checks to see if the inverse has already
## been computed.  If it has, no computation occurs.  If it has
## not, the inverse is computed and cached via setinverse function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
