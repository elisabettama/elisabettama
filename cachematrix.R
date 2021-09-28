##My goal is to cache the inverse of a matrix to avoid computing it repeatedly 
##I'll write 2 functions that cache the inverse of a matrix.

##the function below allows sets the basis to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##the function below computes the inverse of the matrix;
##if the inverse was calculated already, it will just retrieve it from the cache
##and tell me displaying the message "getting cached data" 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

#now I test them to see if the code actually made sense
#I create a squared matrix
pleasework <- matrix(c(0,1,10,100,1000,9,4,50,27),3,3)
pleasework
##and this is what I get
####     [,1] [,2] [,3]
##[1,]    0  100    4
##[2,]    1 1000   50
##[3,]   10    9   27
#since it's a squared matrix i can use solve() to get the inverse
solve(pleasework)
####            [,1]         [,2]          [,3]
##[1,]  3.61913850 -0.363140676  0.1363140676
##[2,]  0.06447655 -0.005452563  0.0005452563
##[3,] -1.36191385  0.136314068 -0.0136314068

#I use my function to get cached its inverse and compute it 
#and I check if the result is the same that I got from using solve()
pleasework_i <- makeCacheMatrix(pleasework)
cacheSolve(pleasework_i)
####            [,1]         [,2]          [,3]
##[1,]  3.61913850 -0.363140676  0.1363140676
##[2,]  0.06447655 -0.005452563  0.0005452563
##[3,] -1.36191385  0.136314068 -0.0136314068

#the result is the same.
