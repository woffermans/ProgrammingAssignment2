##**************************************************************##
##                                                              ##
##                                                              ##
##                                                              ##
##  Functions to cache the inverse matrix                       ##
##                                                              ##
##                                                              ##
##                                                              ##
##  Coursera@Offermans.Rompen.nl  07/02/2017                    ##
##                                                              ##
##                                                              ##
##**************************************************************##


## Calculating the inverse of a matrix can be computationally 
## demanding. Certainly more demanding than looking up the
## inverse matrix in a table in memory (cache)
## The following two functions enable to cache the inverse
## matrix in memory and to calculate the inverse if the inverse
## matrix cannot be found in cache.

## function to create a list that can cache inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y){
		x<<-y
		i<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) i<<-inverse
	getinverse<-function() i
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## function to look up the inverse of a matrix in memory and to
## calculate the inverse if it cannot be found.

cacheSolve <- function(x, ...) {
	i<-x$getinverse()
	if(!is.null(i)){	## check if inverse of matrix x has been calculated
		message("getting cache value")
		return(i)	## return cache value if yes and stop execution
	}
				## calculate inverse if not
	mat<-x$get()		## first get matrix to invert
	i<-solve(mat,...)	## invert the matrix
	x$setinverse(i)		## set inverse matrix into list (cache)
	i			## Return a matrix that is the inverse of 'x'
}
