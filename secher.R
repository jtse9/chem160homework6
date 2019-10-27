secher<-read.table("secher.txt",header=T)
model<-lm(bwt~bpd, data=secher)
sum.model<-summary(model)
R2<-sum.model$r.squared
f<-sum.model$fstatistic 
p.value<-pf(f[1],f[2],f[3],lower.tail=F)
output<-sprintf("R2 = %f and p-value=%f", R2, p.value)
cat(output)
intercept<-model$coefficients[1]
slope<-model$coefficients[2] 
output<-sprintf("slope=%f  intercept=%f",slope, intercept)
cat(output)
png("graph1.png")
plot(bwt~bpd, data=secher)
abline(model)
dev.off()

model2<-lm(bwt~ad, data=secher)
sum.model2<-summary(model2)
R22<-sum.model2$r.squared
f2<-sum.model2$fstatistic 
p.value2<-pf(f[1],f[2],f[3],lower.tail=F)
output2<-sprintf("R2_2 = %f and p-value_2=%f", R22, p.value2)
cat(output2)
intercept2<-model2$coefficients[1]
slope2<-model2$coefficients[2] 
output2<-sprintf("slope_2=%f2  intercept_2=%f2",slope2, intercept2)
cat(output2)
png("graph2.png")
plot(bwt~ad, data=secher)
abline(model2)
dev.off()
