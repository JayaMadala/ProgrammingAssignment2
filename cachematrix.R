
## The makeCacheMatrix function creates a special "matrix", which is really a matrix containing a function to
#set the value of the matrix
#get the value of the matrix
#set the inverse of the matrix
#get the inverse of the matrix

makeCacheMatrix <- function(x = matrix(),nrow=sqrt(length(x)),ncol=sqrt(length(x))) {
        m <- NULL
        ## Creates a new variable in the parent environment of makeCacheMatrix
        set <- function(y,nrow=sqrt(length(y)),ncol=sqrt(lenght(y))) {
                ## x defined as an input from makeCacheMatrix function
                x <<- y
                ## Anticipating that NULL will be updated in the use of setinverse function
                m <<- NULL
                
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        ## This sets m to inverse in the parent env (makeVector())
        getinverse <- function() m
        ## R will look for the value of m in getinverse()
        ## R won't find one within getinverse, so it'll look to the parent environment makeCacheMatrix for m val
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        ## Allows you to access these functions outside of makeCacheMatrix environment
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## Enters this loop when we m already has inverse 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()    # Returns original matrix
cacheSolve(amatrix)  # Computes, caches, and returns    matrix inverse
amatrix$getinverse()   # Returns matrix inverse
cacheSolve(amatrix)  # Returns cached matrix inverse using previously computed matrix inverse

amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getinverse()  # Returns matrix inverse