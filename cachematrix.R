## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function basically creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inver<- NULL  ## First set our variable matrix as NULL
        set<- function(y){   ## make function set(**)
                x <<- y
                inver <<- NULL
        }
        get <- function() x  ## make function get()
        setinverse <- function(inverse) inver <<- inverse ## make function setinverse()
        getinverse <- function() inver   ##make function getinverse()
        list(set = set, get= get, setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        inver <- x$getinverse()  ## we use the function we created above
        if(!is.null(inver)){     ## make sure that there is no NA value in there
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...) 
        ## Computing the inverse of a square matrix can be done with the solve function in R.
        x$setinverse(inver) ## we use the function we created above
        inver
        ## Return a matrix that is the inverse of 'x'
}
        ## In conclusion, I just learned how the teacher writes the function
        ## for the mean of the long vector, and simply change to inverse case
