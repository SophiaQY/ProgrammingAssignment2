
## The first function, makeCacheMatrix creates a special "matrix", which 

##set the value of the matrix
##get the value of the matrix
##set the inverse of the matrix
##get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- function(y){
                x<<- y
                inv<<- NULL
        }
        get<- function(){x}
        setinv<- function(inverse) {inv<<- inverse}
        getinv<- function() {inv}
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}


## The following function computes the inverse of 
## the special "matrix" returned by the above function.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix 
## and sets the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        inv<- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat<- x$get()
        inv<- solve(mat, ...)
        x$setinv(inv)
        inv
}

