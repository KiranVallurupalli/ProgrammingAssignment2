

## This function ("makeCacheMatrix") creates a special "matrix" object that can cache its inverse.
##
## makeCacheMatrix creates a special "matrix", which is to
##
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the matrix inverse
##    get the value of the matrix inverse
##


makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x

        ## Set the inverse matrix

        setinverse <- function(inverse) mi <<- inverse


        ## Get the inverse matrix

        getinverse <- function() mi


        ## set the list

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



##
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##
## it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the mean in the cache via the setinverse function.
##
##

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'

        mi <- x$getinverse()

        ##
        ## Is inverse of matrix is already computed?  Check it with is.null function.  If it was then get cached data and return the value
        ##
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }


        data <- x$get()

        ##
        ## use the "solve" function to compute the inverse and assign it to variable "mi".
        ##

        mi <- solve(data, ...)

        ## set the inverse
        ##
        x$setinverse(mi)

        ## return inverse of matrix
        mi
}
