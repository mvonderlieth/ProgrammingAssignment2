## methods to solve a matrix caching it for future reference.


## object to hold matrix and it's inverse
makeCacheMatrix <- function(x = matrix())
{
    inversem = NULL
    set = function(y)
    {
        x <<- y
        inversem <<- NULL
    }
    get = function()
    {
        x
    }
    setinversem = function(im)
    {
        inversem <<- im
    }
    getinversem = function()
    {
        inversem
    }
    list(set = set, get=get, setinversem=setinversem, getinversem=getinversem)
}


## Method to take a makeCacheMatrix object and calc it's inverse and store it for later reference.
cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    inversem = x$getinversem()
    if (is.null(inversem))
    {
        message("setting cached data")
        mat = x$get()
        inversem = solve(mat,...)
        x$setinversem(inversem)
    }
    else
    {
        message("getting cached data")
    }
    inversem
}

