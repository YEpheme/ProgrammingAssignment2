makeCacheMatrix<-function(x = matrix()){
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