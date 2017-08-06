#1, makeCacheMatrix
makeCacheMatrix<-function(x=numeric()){
	m<-NULL
	setMatrix<-function(y){
		x<<-y
		m<<-NULL
	}
	getMatrix<-function(){
		x
	}
	cacheInverse<-function(z){
		m<<-z
	}
	getInverse<-function(){
		m
	}
	list(setMatrix=setMatrix,getMatrix=getMatrix,cacheInverse=cacheInverse,getInverse=getInverse)
}

#2, cacheSolve
cacheSolve<-function(f,...){
	inverse<-f$getInverse()
	if(!is.null(inverse)){
		message("inverse isn't NULL")
		return(inverse)
	}
	d<-f$getMatrix()
	inverse<-solve(d)
	f$cacheInverse(inverse)
	inverse
}