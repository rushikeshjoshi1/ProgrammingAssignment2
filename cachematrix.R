
## This function creates a special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        M = NULL
        Set = function(y){
                x <<- y
                M <<- NULL
        }
        Get = function() x
        setinv = function(solve) M <<- solve
        getinv = function() M
        list(Set = Set, Get = Get, setinv = setinv, getinv = getinv)

}


## This computes the inverse of the special matrix returne by above function
## If the inverse is already calculated it retrives it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        M = x$getinv()
        if(!is.null(M)){
                message("getting cached data\n")
                return(M)
        }
        data = x$Get()
        M = solve(data,...)
        x$setinv(M)

        M
}
