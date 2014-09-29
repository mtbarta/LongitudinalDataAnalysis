#download the data from http://tigger.uic.edu/~hedeker/long.html
dat <-read.table("~/Downloads/RIESBY.DAT.txt", quote="\"",na.strings=c("."))
#calls in an empty line at the end.
dat<- dat[1:396,1:6]
names(dat) <- c("ID", "HamD", "Intcpt", "Week", "Endog", "EndWeek")
dat$HamD <- as.numeric(dat$HamD)

library('nlme')

#non orthogonal model
model <- lme(HamD~Week+I(Week^2),data=dat, random=~1+Week+I(Week^2)|ID,method="ML",na.action=na.omit)

model
anova(model)
getVarCov(model)

#spaghetti plot
library(ggplot2)
scatter <- ggplot(data=dat,aes(x=Week,y=HamD, group=ID,colour=ID))
scatter + geom_line()

#prediction plot
pred <- predict(model)
pred.trimmed <- pred[!is.na(dat$HamD)]
pred.week <- dat$Week[!is.na(dat$HamD)]
pred.data <- data.frame(week=pred.week,ID=names(pred.trimmed),score=pred.trimmed)
pred.plot <- ggplot(data=pred.data,aes(x=week,y=score,group=ID,colour=ID))
pred.plot+geom_line()

#plot one observation
scatter <- ggplot(data=dat[1:6,],aes(x=Week,y=HamD, group=ID,colour=ID))
scatter + geom_line()
pred.plot <- ggplot(data=pred.data[1:6,],aes(x=week,y=score,group=ID,colour=ID))
pred.plot + geom_line()

#orthogonal polynomials
# 1.) poly(dat$Week[1:6],2) creates the polynomials for Weeks 0-5, but NOT the intercept.
# 2.) add the intercept term with cbind. rep(.4082,6) replicates .4082 6 times.
edited.poly <- data.frame(cbind(rep(.4082,6),poly(dat$Week[1:6],2)))
# we have 66 subjects, so replicate the above data frame 66 times by replicating the indexes.
expanded.poly <- edited.poly[rep(1:6,66),]
#rename columns
colnames(expanded.poly) <- c("int","linT","quadT")
#add in response and subject variables to the data frame.
poly.dat <- data.frame(HamD=dat$HamD,ID=dat$ID,expanded.poly)
#call the model
orthogonal.model <- lme(HamD~0+int+linT+quadT,data=poly.dat, random=~0+int+linT+quadT|ID, method="ML",na.action=na.omit)

orthogonal.model
anova(orthogonal.model)
getVarCov(orthogonal.model)
