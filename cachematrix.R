makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
                print(x)
        }
        setinverse <- function(cal) {
                inverse <<- cal
        }
        getinverse <- function() {
                print(inverse)
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# if the inverse has already been calculated, then the catchsolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
        
        ## return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(is.null(inverse)) {
                data <- x$get()
                inverse <- solve(data, ...)
                x$setinverse(inverse)
        } else { 
                message("getting cached inverse")
                return(inverse())
        }
        print(inverse)
}

## check
M <- makeCacheMatrix(matrix(rnorm(1:100), 10, 10))
M$get()
M$getinverse()
M$set(matrix(rnorm(1:36), 6, 6))
M$get()

cacheSolve(M)
M$getinverse()

