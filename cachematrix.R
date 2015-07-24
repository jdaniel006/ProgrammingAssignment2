## Put comments here that give an overall description of what your
## functions do

## The objective here is to cache the inverse of a matrix that may be used
## often, therefore saving some time (and some CO2 emissions to the planet ;-) )
##
## Usage: first create an object to store the matrix whose inverse will be
## cached, using makeCacheMatrix
## Then, use cacheSolve function with the newly created object as parameter. The
## function will return the inverse of the matrix and inform whether it had
## already been cached. No need to keep track of it by oneself!
##
## Example:
## mymatrixcached <- makeCacheMatrix(mymatrix)
## myinversematrix <- cacheSolve(mymatrixcached)


## Write a short comment describing this function
## Function name: makeCacheMatrix
## Arguments: one valid matrix object (i.e, an invertible matrix)
## Return value: a list of functions, which provide the functionality
##      set(m): caches the m matrix object and clears its cached inverse matrix,
##           if exists
##      get(): retrieves the matrix stored with set()
##      setInverse(invm): caches invm as the inverse matrix
##      getInverse(): retrieves the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        InverseMatrix <- NULL ## to create an empty object
        ## set: see above. Each time it is called, the inverse matrix is cleared
        set <- function(y) {
                x <<- y
                InverseMatrix <<- NULL
        }
        
        ## get: see above
        get <- function() x
        
        ## idem. I have changed the name of the argument to avoid confusion,
        ## although R has different name spaces for functions and vars
        setInverse <- function(solveJD) InverseMatrix <<- solveJD
        
        # idem
        getInverse <- function() InverseMatrix
        
        # return value, a list of functions AND their environment
        # This means that x and InverseMatrix are kept within the object
        # and shared amongst its functions
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Function name: cacheSolve
## Arguments: mandatoryly one makeCacheMatrix object (i.e, an invertible matrix)
##              optionally: extra args for solve() function
##              (see ?solve for more info)
## Return value: the inverse matrix of the matrix stored in the first
##              argument
##              If that inverse matrix had alrady been computed, it returns the
##              cached results and informs with a message.
##              Otherwise, the function computes the inverse matrix, returns
##              the value and stores it in the argument for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## First, we check if the inverse matrix is already cached
        InverseMatrix <- x$getInverse()
        if(!is.null(InverseMatrix)) { ## if it exists...
                message("getting cached data") ## ... say it is cached and...
                return(InverseMatrix) ## ... return the cached data and exit
        }
        ## Otherwise, get the matrix whose inverse is to be computed
        TempMatrix <- x$get() ## We use a temporary object
        ## resolve the inverse 
        InverseMatrix <- solve(TempMatrix, ...)
        ## store the result in the cache
        x$setInverse(InverseMatrix)
        ## return the inverse matrix
        InverseMatrix
}
