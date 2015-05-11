summary.pampe <-
function(object, ... ){
  
  pampe.object <- object
  
  if (class(pampe.object) != "pampe"){
    stop("Wrong object class")
  } 
  
  time.tr <- (length(pampe.object$model$fitted.values)+1):(length(pampe.object$counterfactual[,2]))
  
  

  cat("Selected controls:\n ")
  
  cat(paste(pampe.object$controls[-length(pampe.object$controls)],",", sep=""))
  cat(paste(" and ", pampe.object$controls[length(pampe.object$controls)],".\n",sep=""))
   
  cat("\n \nTime-average estimated treatment effect:\n ")
  
  cat(mean(pampe.object$counterfactual[time.tr,1]-pampe.object$counterfactual[time.tr,2]))
  
  cat("\n\nOptimal model estimation results:\n\n")
  
  printCoefmat(summary(pampe.object$model)$coefficients)
  
  cat("\nResidual standard error: ")
  cat(round(summary(pampe.object$model)$sigma,4))
  cat(" on ")
  cat(pampe.object$model$df.residual)
  cat(" degrees of freedom")
  cat("\nMultiple R-squared: ")
  cat(round(summary(pampe.object$model)$r.squared,3))
  cat(",     Adjusted R-squared: ")
  cat(round(summary(pampe.object$model)$adj.r.squared,3))
  cat("\nF-statistic: ")
  cat(round(summary(pampe.object$model)$fstatistic[1],2))
  cat(" on ")
  cat(round(summary(pampe.object$model)$fstatistic[2]))
  cat(" and ")
  cat(round(summary(pampe.object$model)$fstatistic[3]))
  cat(" DF, p-value: ")
  cat(pf(summary(pampe.object$model)$fstatistic[1], summary(pampe.object$model)$fstatistic[2], summary(pampe.object$model)$fstatistic[3], lower.tail=F))
  
  res.table <- cbind(pampe.object$counterfactual, pampe.object$counterfactual[,1] - pampe.object$counterfactual[,2])
  colnames(res.table) <- c("Actual", "Counterfactual", "Estim. Tr. Effect")
  
  cat("\n \nActual and counterfactual results:\n\n")
  
  print(res.table)
  
}


