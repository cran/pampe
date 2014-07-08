robustness <-
function(pampe.object, time.pretr, time.tr, treated, data){

##First create a matrix to save results
robust.pred <- matrix(NA,
                      ncol=length(pampe.object$controls),
                      nrow=length(c(time.pretr,time.tr)))
##Iteratively remove each one of the ctrls in model
for (i in 1:length(pampe.object$controls)){
  #Each time remove one of the controls
  fmla <- as.formula(paste(treated, " ~ ",
                           paste(pampe.object$controls[-i],
                                 collapse= "+")))
  robust.temp <- lm(fmla, data=data[time.pretr,])
  #Save the results
  robust.pred[,i] <- predict(robust.temp, data)[c(time.pretr,time.tr)]
}
##Name rows and matrix of the results
robust.pred <- cbind(pampe.object$counterfactual[,1],pampe.object$counterfactual[,2], robust.pred)
colnames(robust.pred) <- c("Actual", "Predict w/ all", paste("w/o", pampe.object$controls))
rownames(robust.pred) <- rownames(data)[c(time.pretr,time.tr)]

class(robust.pred) <- "robust-pampe"
return(robust.pred)
}
