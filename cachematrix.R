# makeCacheMatrix is a function that returns a list of functions. 
# It is meant to store a matrix and the cached value of the inverse of that matrix
# makeCacheMatrix contains four functions:
# 	setMatrix	sets the value of the matrix
#	getMatrix	gets the value of the matrix
#	cacheInverse	sets the cached value (inverse of the matrix)
#	getInverse	gets the cached value (inverse of th matrix)


#makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
	cache<-NULL #initially nothing is cached so it is set no NULL
	setMatrix<-function(y){
		x<<-y #the matrix is assigned the new value 'y'
		cache<<-NULL #clear the cache
	} #stores the matrix

	getMatrix<-function(){
		x
	} #returns stored matrix

	cacheInverse<-function(solve){
		cache<<-solve
	} #cache's the argument

	getInverse<-function(){
		cache
	}#gets the cached value

#finally we return a list, where each element of the list is a function.

list(setMatrix=setMatrix, getMatrix=getMatrix, cacheInverse=cacheInverse, getInverse=getInverse)	
}





## cacheSolve function calculates the inverse of the "special" matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
	#get the cached value
 	inverse<-x$getInverse()
	#if a cached value exists, then return it
	if(!is.null(inverse)){
	message("getting cached data")
	return(inverse)
	}
	#if there is no cached value, then, get the matrix, caculate the inverse and store it in the cache
	data<-x$getMatrix
	inverse<-solve(data)
	y$cacheInverse(inverse)

	#return the inverse
	inverse
}
