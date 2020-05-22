## Course-2 Week-3 Assignment
## Caching matrix inverse

## Makes a matrix and caches the inverse

makeCacheMatrix <- function(x = matrix()) {
  
    inv=NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get <- function() x
    setinv <- function(solve) inv<<-solve
    getinv <- function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## checks if inverse is cached and if not then computes it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv <- solve(x,...)
    x$setinv(inv)
    inv
}
