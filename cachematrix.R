# R programming
# Assignment #2
# January 19, 2017
# Regina Cer
# JHU Data Science Course
# Reference: https://en.wikipedia.org/wiki/Invertible_matrix

# creates a special 'matrix' object that can cache its inverse
# x<-c(1,3,5,9)
#> matrix(x,2,2)
# [,1] [,2]
# [1,]    1    5
# [2,]    3    9

# how to run
#> b<-makeCacheMatrix()
#> b$set(matrix(7:10,2,2))
#> cacheSolve(b)
#[,1] [,2]
#[1,]   -5  4.5
#[2,]    4 -3.5

makeCacheMatrix <- function(z = matrix()) {
        n<-NULL
        set<-function(y){
                z<<-y
                n<<-NULL
        }
        get<-function() z
        setMatrix<-function(solve) n<<- solve
        getMatrix<-function() n
        list(set=set, get=get,
             setMatrix=setMatrix,
             getMatrix=getMatrix)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. 
# If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the 
# inverse from the cache.
cacheSolve <- function(z=matrix(), ...) {
        n<-z$getMatrix()
        if(!is.null(n)){
                message("Getting cached data for inverse matrix...")
                return(n)
        }
        matrix<-z$get()
        n<-solve(matrix, ...)
        z$setMatrix(n)
        n
}