##this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function (x=matrix())#make the argument, assuming invertible
{
  inv<-NULL #define variable
  set <-function(y) #set the value of the matrix using another function
    {
    x<<- matrix
    inv<<-NULL #the double arrow assignment operator allows for two levels of 
  #parameters. One in the parent function and the other in the inside function 
  #- manages variables at different levels
}
get<-function(){x} #get the value of the matrix
setInverse<-function(inverse){inv<<-inverse} #set the value of the inverse
getInverse<-function(){inv} #get the value of the inverse
list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

##This function computes the inverse of the special"matrix" returned by the "makeCacheMatrix" above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve shoudl retrieve the inverse from the cache.

cacheSolve<-function(x,...){
  inv<-x$getInverse() #returns a matrix that is an inverse of x and assigns 
  #it to inv
  if(!is.null(inv)){
    message("getting cached data") #if it's retrieving data, will display this
    return(inv) #return the inverse value
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setInverse(inv)
  inv
}

Source("makeCacheMatrix.R") #the name of my file
pmatrix<-makeCacheMatrix(matrix(1:16, nrow=4, ncol=4))
#now get the matrix
pmatrix$get()
pmatrix$getInverse()
cacheSolve(pmatrix)