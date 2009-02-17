exp1.dat = read.table(file="Documents/workspace/VariabilityManagement/docs/casestudies/controlled-experiment/data/fase01-tabular.data",header=TRUE)
Replicacao = factor(Replicacao)
Aluno = factor(Aluno)
Dominio = factor(Dominio)
Tecnica = factor(Tecnica)

exp1.aov <- aov(Resposta~Replicacao+Aluno:Replicacao+Dominio+Tecnica)
exp1.lm <- lm(Resposta~Replicacao+Aluno:Replicacao+Dominio+Tecnica)

> library(MASS)
> boxcox(exp1.aov,lambda = seq(-3, 5, 1/10))
> exp12.aov<-aov(Resposta^(-1)~Replicacao+Aluno:Replicacao+Dominio+Tecnica)
> exp12.lm<-lm(Resposta^(-1)~Replicacao+Aluno:Replicacao+Dominio+Tecnica)


plot(predict(exp1.aov),residuals(exp1.aov),xlab="estimated values", ylab="residuals",axes=F)
axis(1, at = c(50,60, 70, 80, 90, 100), pos=0)
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

