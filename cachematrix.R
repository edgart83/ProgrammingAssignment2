## makeCacheMatrix creates a list of functions 
## to set/get the value of the input matrix data and 
## to set/get the value of the inverse matrix

		
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL				# place holder for the inverse matrix
        set <- function(y) {			# establishes the value of the matrix
                x <<- y				# x now contains the matrix 
                m <<- NULL
        }
        get <- function() x				# get the matrix
        setinvmtx <- function(solve) m <<- solve	# populate (m) with the inv matrix 
        getinvmtx <- function() m			# get the inverse matrix (m)
        list(set = set, get = get,
             setinvmtx = setinvmtx,
             getinvmtx = getinvmtx)
}



## This function takes a matrix preprocessed by makeCacheMatrix as an input 
## and returns the inverse matrix. If the 
## solution previously exist it is returned and if not 
## it is computed, stored and returned 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinvmtx()
        if(!is.null(m)) {						## if place holder not m is empty 
									## then solution exists
                message("getting cached data")
                return(m)						## inverse matrix is returned
        }
	## Need to compute inverse matrix as solution does not exists
        data <- x$get()							## get the matrix
        m <- solve(data, ...)						## Compute inverse matrix
        x$setinvmtx(m)							## Store result in (m)
        m								## return the value of the invertse matrix
}
