makeCacheMatrix <- function(x = numeric()) {
        im <- NULL # im is the inverse matrix in cache
        set <- function(y) {
                x <<- y
                im <<- NULL # set removes the inverse matrix from cache (if present)
        }
        get <- function() x
        setinverse <- function(solve) im <<- solve # put inverse matrix in cache
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...) # calculates the inverse matrix
        x$setinverse(im)       # calls setinverse with the calculated inverse matrix
        im
}
