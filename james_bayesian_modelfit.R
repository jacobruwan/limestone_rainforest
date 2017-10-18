# setwd(temp)

################################################################################
#### Save results to csv files
# write.table(x=result$summary.fixed,file="results.csv",row.names=FALSE,col.names=FALSE,sep=",")

################################################################################
#### Plot and save CPO/PIT
dev.new()
pdf("pit1.pdf")
qqnorm(result$cpo$pit)
qqline(result$cpo$pit)
dev.off()

dev.new()
pdf("pit2.pdf")
hist(result$cpo$pit)
dev.off()

################################################################################
#### Calculate posterior mean prediction (to compare to observed data)
y.pred <- result$summary.fitted.values$mean
lb <- min(na.omit(y))-1
ub <- max(na.omit(y))+1

################################################################################
#### Residual plots colour coded by 'local_provenance'
dev.new()
pdf("res_plots.pdf")
par(mfrow=c(2,2))
plot(y,y.pred,xlab="Observed data",ylab="Predicted data",xlim=c(lb,ub),ylim=c(lb,ub))
points(c(lb,ub),c(lb,ub),type="l")

ind <- order(y.pred)
plot(1:length(y.pred),y.pred[ind],type="l",ylim=c(lb,ub),xlab="Index",ylab="Observed and predicted data")
points(1:length(y.pred),y[ind])

plot(y.pred,y - y.pred,xlab="Predicted data",ylab="Residuals")
points(c(lb,ub),c(0,0),type="l")
dev.off()

################################################################################
#### Residual plots versus all variables in data.full
dev.new()
pdf("res_other1.pdf")
par(mfrow=c(4,4))
for (iii in 1:16){
  plot(data.frame(as.numeric(data.full[,iii]),y-y.pred),xlab=paste(names(data.full)[iii]))
  points(c(min(as.numeric(data.full[,iii])),max(as.numeric(data.full[,iii]))),c(0,0),type="l")
}
dev.off()

dev.new()
pdf("res_other2.pdf")
par(mfrow=c(4,4))
for (iii in 17:32){
  plot(data.frame(as.numeric(data.full[,iii]),y-y.pred),xlab=paste(names(data.full)[iii]))
  points(c(min(as.numeric(data.full[,iii])),max(as.numeric(data.full[,iii]))),c(0,0),type="l")
}
dev.off()

dev.new()
pdf("res_other3.pdf")
par(mfrow=c(4,4))
for (iii in 33:48){
  plot(data.frame(as.numeric(data.full[,iii]),y-y.pred),xlab=paste(names(data.full)[iii]))
  points(c(min(as.numeric(data.full[,iii])),max(as.numeric(data.full[,iii]))),c(0,0),type="l")
}
dev.off()

dev.new()
pdf("res_other4.pdf")
par(mfrow=c(4,4))
for (iii in 49:64){
  plot(data.frame(as.numeric(data.full[,iii]),y-y.pred),xlab=paste(names(data.full)[iii]))
  points(c(min(as.numeric(data.full[,iii])),max(as.numeric(data.full[,iii]))),c(0,0),type="l")
}
dev.off()

dev.new()
pdf("res_other5.pdf")
par(mfrow=c(4,4))
for (iii in 65:80){
  plot(data.frame(as.numeric(data.full[,iii]),y-y.pred),xlab=paste(names(data.full)[iii]))
  points(c(min(as.numeric(data.full[,iii])),max(as.numeric(data.full[,iii]))),c(0,0),type="l")
}
dev.off()

dev.new()
pdf("res_other5.pdf")
par(mfrow=c(4,4))
for (iii in 81:96){
  plot(data.frame(as.numeric(data.full[,iii]),y-y.pred),xlab=paste(names(data.full)[iii]))
  points(c(min(as.numeric(data.full[,iii])),max(as.numeric(data.full[,iii]))),c(0,0),type="l")
}
dev.off()

################################################################################
#### Save output summary
s <- summary(result)
capture.output(s, file = "summary.txt")

################################################################################
