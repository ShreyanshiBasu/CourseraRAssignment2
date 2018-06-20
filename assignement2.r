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

#OUTPUT:

#> source("assignement2.r")
#> ls()
#[1] "add2"            "cacheSolve"      "df"              "makeCacheMatrix"
#[5] "myfunction"      "pollutantmean"   "second"          "sub"            
#[9] "sub1"            "x"               "y"              
#> x = rbind(c(1,-2),c(-2,1))
#> m = makeCacheMatrix(x)
#> m$get()
 #    [,1] [,2]
#[1,]    1   -2
#[2,]   -2    1
#> cacheSolve(m)
 #          [,1]       [,2]
#[1,] -0.3333333 -0.6666667
#[2,] -0.6666667 -0.3333333
#> cacheSolve(m)
#getting cached data.
 #          [,1]       [,2]
#[1,] -0.3333333 -0.6666667
#[2,] -0.6666667 -0.3333333
