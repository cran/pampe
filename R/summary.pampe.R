summary.pampe <-
function(object, ... ){
  
  pampe.object <- object
  
  if (class(pampe.object) != "pampe"){
    stop("Wrong object class")
  } 
  
  print(summary(pampe.object$ols))
  
}
