makeCacheMatrix<-function(x = matrix()){
  #the function can creates a list that can cache
  #the maxrix's invers,with four functions as follows.
  #set() is a function that can set the value of
  #the matrix.
  #get() is a function that can get the value of
  #the matrix.
  #setinv() is a function that can set the value
  #of the inverse of the matrix.
  #getinv() is a function that can get the value
  #of the inverse of the maxrix.
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinv<-function(inv) i<<-inv
  getinv<-function() i
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}

cacheSolve<-function(x,...){
  #this function can compute the inverse of a matrix
  #which is workd by function"makeCacheMatrix".If the 
  #inverse of matrix has been calculated ,then it will
  #get the inverse straightly from the "getinv" and
  #give you a message to show that;
  #otherwise it will calculate the inverse by using 
  #solve()
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinv(i)
  i
}