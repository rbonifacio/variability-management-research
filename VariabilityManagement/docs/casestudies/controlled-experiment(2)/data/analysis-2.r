> exp1.dat = read.table(file="Documents/workspace/VariabilityManagement/docs/casestudies/controlled-experiment(2)/data/fase01-so.data" ,header=TRUE)
> attach(exp1.dat)
> Replic = factor(Replic)
> Student = factor(Student)
> SPL = factor(SPL)
> Technique = factor(Technique)
ResponseMinutes = Response / (1000 * 60)

exp1.aov <-aov(ResponseMinutes~Replic+Student:Replic+SPL+Technique)
exp1.lm <-lm(ResponseMinutes~Replic+Student:Replic+SPL+Technique)
 
library(MASS)
boxcox(exp1.aov,lambda = seq(-3, 5, 1/10))
exp12.aov<-aov(ResponseMinutes^(-1)~Rep+Student:Rep+CR+Technique)
exp12.lm<-lm(ResponseMinutes^(-1)~Rep+Student:Rep+CR+Technique)

plot(predict(exp12.aov),residuals(exp12.aov),xlab="estimated values", ylab="residuals",axes=F)
axis(1, at = c(20, 30, 40, 50,60, 70, 80, 90, 100, 110,120,130,140,150), pos=0)
axis(2, at = c(-30,-25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40))

qqnorm(residuals(exp1.aov), xlab="", ylab="")

-- necessario importar o pacote car
Non-constant Variance Score Test 
ncv.test(exp1.lm)

TukeyNADD.QL.REP<-function(objeto1)
{
y1<-NULL
y2<-NULL
y1<- fitted(objeto1)
y2<- y1^2
objeto2<- aov(y2 ~ objeto1[13]$model[,2] +
objeto1[13]$model[,3]:objeto1[13]$model[,2]
+ objeto1[13]$model[,4]+ objeto1[13]$model[,5])
ynew <- resid(objeto1)
xnew <- resid(objeto2)
objeto3 <- lm(ynew ~ xnew)
M <- anova(objeto3)
MSN <- M[1,3]
MSErr <- M[2,2]/(objeto1[8]$df.residual-1)
F0 <- MSN/MSErr
p.val <- 1 - pf(F0, 1,objeto1[8]$df.residual-1)
p.val
}



exp2.dat = read.table(file="Documents/workspace/VariabilityManagement/docs/casestudies/controlled-experiment/data/fase02-tabular.data",header=TRUE)
attach(exp2.dat)
Replica= factor(Replic)
Student = factor(Student)
ChangeSet = factor(ChangeSet)
Technique = factor(Technique)

exp2.aov <- aov(TotalTime~Replica+Student:Replica+ChangeSet+Technique)
exp2.lm <- lm (TotalTime~Replica+Student:Replica+ChangeSet+Technique)

library(MASS)
boxcox(exp2.aov,lambda = seq(-3, 5, 1/10))


> exp22.aov <- aov(TotalTime^(0.4)~Replica+Student:Replica+ChangeSet+Technique)
> exp22.lm <- lm(TotalTime^(0.4)~Replica+Student:Replica+ChangeSet+Technique)

plot(predict(exp2.aov),residuals(exp2.aov),xlab="estimated values", ylab="residuals",axes=F)
axis(1, at = c(10, 20, 30, 40, 50,60, 70, 80, 90, 100), pos=0)
axis(2, at = c(-30,-25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40))



OCP = (ChangedScenarios)/(ChangedScenarios + AddedScenarios)