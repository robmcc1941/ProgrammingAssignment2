# makeCacheMatrix(amtrx)
#
# creates a special copy of a matrix with storage for the inverse of the matrix
# for caching them in an external environment
# (this assumes the matrix is an invertible square matrix)
# it returns a list containing 4 functions which can 
#    set the matrix
#    get the matrix
#    set the inverse of the matrix
#    get the inverse of the matrix
#    the inverse is initialized as NULL
#    if the matrix is re-set, the inverse is also re-set to NULL
#
# (But note that you could set an inverse matrix value that was not
#  actually the inverse of the stored matrix.
#  The assignment did not require us to prevent this or to test input data
#  for validity.)

makeCacheMatrix <- function(amtrx = matrix()) {
# initiate the inverse matrix to NULL
        mtrxinverse <- NULL  
		
# set(y) stores matrix y in the external environment
# and resets the inverse matrix to NULL
        set <- function(y) { 
                amtrx <<- y
                mtrxinverse <<- NULL
	        }
		
# get() simply returns the matrix stored
# in the external environment
				
        get <- function() amtrx
		
# setMinverse(inverse) stores its argument
# which should be the inverse matrix of
# the stored matrix
							  
        setMinverse <- function(inverse) mtrxinverse <<- inverse
		
# getMinverse() returns the value stored in the 
# cache that should be the inverse matrix
# of the stored matrix
        getMinverse <- function() mtrxinverse
		
# return a list of these 4 functions	
# so they can be applied  to the cached data
# by name
        list(set = set, get = get,
             setMinverse = setMinverse, getMinverse = getMinverse)
}

#cacheSolve(amtrx)
#
#produces the inverse matrix of the matrix stored in an object outside the current
#environment which has been created using makeCacheMatrix(amtrx).
# If the inverse has not yet been calculated, it is calculated using solve()
# and the result is cached in the external environmemt object
# and returned.
# If the inverse has previously been calculated,
# it reminds us with a message and returns the previously calculated inverse matrix
# that was stored in the cache 

cacheSolve <- function(amtrx, ...) {
# getMinverse() retrieves the inverse matrix stored in the external
# environemnt object amtrx

        mtrxinverse <- amtrx$getMinverse()
# if the value returned is not NULL
# the inverse of the stored matrix has already
# been calculated and it is returned
# a message is printed to indicate that the value
# had been previously cached

        if(!is.null(mtrxinverse)) {
                message("getting cached data")
                return(mtrxinverse)
        }
		
# otherwise the inverse has not previously been
# calculated, so we retrieve the stored matrix
# and calculate its inverse and store it in the cache
# the newly calculate inverse is returned

        data <- amtrx$get()
        mtrxinverse <- solve(data, ...)        
        amtrx$setMinverse(mtrxinverse)
}

# below are some sample matrices I used to test my functions
# these are all square invertible matrices
# note that if you pass a matrix to makeCacheMatrix(x)
# that is not a square matrix or is not an invertible square matrix
# you will get an error message, there is no data checking performed by
# any of this code

m1 <- matrix(c(-3, 16.5, -0.156, 13.6, 2.45, -32.7, -1, 2.78,
              2, -0.123, 5, 9.1, -23.1, -7.07, 3.333, 1.11), 4 ,4)
              
m2 <- matrix(c(4,5,3,7.1,2,3,6,2, 21),3,3)

m3 <- matrix(c(4,3,3,2),2,2)

m4 <- matrix(c(1,3,2,4), 2, 2)