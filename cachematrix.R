# Example
makeVector <- function(x = numeric()) {
        # sets x equal to an empty numeric vector
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get, setmean = setmean      , getmean = getmean)
        
}
        # This function creates a special "vector", which is really a list of:
                #1. set the value of the vector
                #2. get the value of the vector
                #3. set the value of the mean
                #4. get the value of the mean

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

# Practice
a <- makeVector()       # initialize
a                       # shows that a is a list of functions
class(a)                # a is a list
class(a$set)            # set (within a) is a function
a$set(c(1, 2, 3, 4, 5, 6, 7)) # set the function
a$get()                 # returns the vector that was previously "set"
cachemean(a)
cachemean(a)


tf <- matrix(1:4, 2, 2)         # Creates a matrix
tf
tf1 <- solve(tf)                # solve(tf) calculates the inverse of the matrix tf
tf1
tf %*% tf1                      # %*% is matrix multiplication


# Part 1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # sets x equal to an empty matrix
        I <- NULL
        # Set the inverse equal to NULL
        
        set <- function(y){
                x <<- y
                # set function assigns the argument to x
                I <<- NULL
                # Once the set function is called, Inverse is re-set to NULL (this is important if you redefine the matrix, x)
        }
        get <- function() x
        # get function returns the matrix
        
        setInverse <- function(solve) I <<- solve
        # setInverse overrides the previous value of I and assigns the argument to Inverse (which is supposed to be the inverse of matrix x)
        
        getInverse <- function() I
        # getInverse returns the Inverse
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        # creates a list of the functions
        
}

# Part 2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        # Retrives the most recent value for the inverse
        
        if(!is.null(I)){
                message("getting cached data")
                return(I)
                # If the value of Inverse is NOT null (was previously calculated), cacheSolve returns that value        
        }
        # If the value of Inverse is NULL, then you retrive matrix x and calculate the inverse with the solve() function
        message("newly calculating data")
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        # Sets Inverse to the newly calculated value   
        I #Returns the new Inverse value
}

# Test functions
M <- makeCacheMatrix()
V <- makeVector()
class(M)
class(V)
class(M$set)
class(V$set)
M$set(matrix(1:4, 2, 2))
M$get()
M$getInverse()
M$setInverse(9)
cacheSolve(M)
M$setInverse(NULL)
cacheSolve(M)
M$getInverse()
M$set(matrix(1:9, 3, 3))
M$getInverse()
cacheSolve(M)
M_I <- M$getInverse()
M1 <- M$get()
M1 %*% M_I

# Test with invertible matrices: reference https://class.coursera.org/rprog-003/forum/search?q=lapack#11-state-query=lapack
install.packages("magic")
library(magic)
        # Install a package that makes invertible, square matrices

A <- magic(5)
A

B <- makeCacheMatrix(A)
names(B)
B$get()
B$getInverse()
cacheSolve(B)
B$getInverse()
B$set(A)

C <- cacheSolve(B)
        # This sets matrix C to the B, which is the inverse of matrix A
B$set(C)
B$getInverse()
cacheSolve(B)
