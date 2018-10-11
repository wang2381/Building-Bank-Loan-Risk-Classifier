getwd()
setwd("/Users/MURASAKIMIKU/526project")
data0<-read.csv("german_credit.csv")
head(data0)
str(data0)
#### 1: creditability: main response
#### factor 2: 4 level; 4: 5 level; 5: 11 level;
####        7: 5 level; 8: 5 level; 
####        10: 4 level; 11: 3 level; 
####        13: 4 level; 15: 3 level; 16: 3 level;
####        18: 4 level; 
####        20: 2 level; 21: 2 level
#### conti  3(duration of credit): 4-72;  6(credit amount): 250-18424; 9(installment): 1-4; 12(duration in current address): 1-4
14(age): 19-75; 17(Number of existing credits at this bank): 1-4;
19(number of depedent): 1-2

######## 13 categorical variable, 7 continuous variable.

### Deep dive into the important variable selected from logistic regression
k<-21;
unique(data0[,k])
length(unique(data0[,k]))
range(data0[,k])

plot(data0[,2],data0[,1])
plot(data0)

rawname<-names(data0)
names(data0)<-c("Y",
 "X1","X2","X3","X4","X5","X6","X7","X8","X9","X10",
 "X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")
### Fitting
mod0<-glm(cbind(Y,1-Y)~1, family = binomial, data=data0)
mod1<-glm(cbind(Y,1-Y)~., family = binomial, data=data0)
class(mod1)
summary(mod1)
mod2<-step(mod1, direction = "both") ## drop  5 variable, 4,11,13,17,18
rawname[(c(4,11,13,17,18)+1)] ## List of dropped variable
pchisq(deviance(mod2),df.residual(mod2),lower=F) ## suggest mod2 is a good fit
summary(mod2)

library(rsq)
rsq(mod2, adj=FALSE) ## 0.254

### Overdispersion
est.phi<-function(glmobj){
  sum(residuals(glmobj, type="pearson")^2/df.residual(glmobj))}
est.phi(mod2) ### 1.03, no dispersion issue

drop1(mod2, test="Chisq")
mod3<-update(mod2, .~.-X16)
summary(mod3)
pchisq(deviance(mod3),df.residual(mod3),lower=F)
est.phi(mod3)

drop1(mod3, test="Chisq")
mod4<-update(mod3, .~.-X19)
summary(mod4)
pchisq(deviance(mod4),df.residual(mod4),lower=F)
est.phi(mod4)

drop1(mod4, test="Chisq")
mod5<-update(mod4, .~.-X12)
summary(mod5)
pchisq(deviance(mod5),df.residual(mod5),lower=F)
est.phi(mod5)

drop1(mod5, test="Chisq")
mod6<-update(mod5, .~.-X15)
summary(mod6)
pchisq(deviance(mod6),df.residual(mod6),lower=F)
est.phi(mod6)

drop1(mod6, test="Chisq") ## All significant, remaining 11 covariate

diagnostic_mod<-function(glmmod){
par(mfrow=c(2,2))
plot(glmmod,1)
plot(glmmod,2)
plot(glmmod,3)
plot(glmmod,4)
}
diagnostic_mod(mod1)
diagnostic_mod(mod2)
diagnostic_mod(mod3)
diagnostic_mod(mod4)
diagnostic_mod(mod5)
diagnostic_mod(mod6)

#### new variable set
summary(mod6)
data1<-data0[,c(1,1+c(1,2,3,5,6,7,8,9,10,14,20))]


##indepedent study:pairs comparison between factor (X1,X2,X3,X5,X6,X7,X8,X9,X10,X14,X20)
##factor:X1: 4 level; X3: 5 level; X6: 5 level; X7: 5 level; 
X9: 4 level; X10: 3 level; X14: 3 level; X20: 2 level
##conti:X2(duration of credit): 4-72;  X5(credit amount): 250-18424; X8(installment): 1-4; 
######## 8 categorical variable, 3 continuous variable.
##totally 28 pairs
1&3,6,7,9,10,14,20: 
xtabs(Y~X1+X3,data1)->pair1
 
xtabs(Y~X1+X6,data1)->pair2
 
xtabs(Y~X1+X7,data1)->pair3
 
xtabs(Y~X1+X9,data1)->pair4
 
xtabs(Y~X1+X10,data1)->pair5
 

xtabs(Y~X1+X14,data1)->pair6
 
xtabs(Y~X1+X20,data1)->pair7
 

3&6,7,9,10,14,20: 
xtabs(Y~X3+X6,data1)->pair8
 

xtabs(Y~X3+X7,data1)->pair9
 

xtabs(Y~X3+X9,data1)->pair10
 

xtabs(Y~X3+X10,data1)->pair11
 

xtabs(Y~X3+X14,data1)->pair12
 

xtabs(Y~X3+X20,data1)->pair13
 


6&7,9,10,14,20:
xtabs(Y~X6+X7,data1)->pair14
 
  
xtabs(Y~X6+X9,data1)->pair15
 
xtabs(Y~X6+X10,data1)->pair16
 
xtabs(Y~X6+X14,data1)->pair17
 
xtabs(Y~X6+X20,data1)->pair18
 


7&9,10,14,20: 
xtabs(Y~X7+X9,data1)->pair19
 
xtabs(Y~X7+X10,data1)->pair20
 
xtabs(Y~X7+X14,data1)->pair21
 
xtabs(Y~X7+X20,data1)->pair22
 


9&10,14,20: 
xtabs(Y~X9+X10,data1)->pair23
 
xtabs(Y~X9+X14,data1)->pair24
 
xtabs(Y~X9+X20,data1)->pair25
 



10&14,20: 
xtabs(Y~X10+X14,data1)->pair26
 
xtabs(Y~X10+X20,data1)->pair27
 



14&20:
xtabs(Y~X14+X20,data1)->pair28
 


fisher.test for indepedent:
pair 13,18,22,23,25,26,28: ind
pair 7,11,27: not ind


Pearson's chi-sq test for indepedent:
pair 468,10,15,17,20,21,24: ind
pair 12359,12,14,16,19: not ind


summary:
ind:pair 468,10,13,15,17,18,20,21,22,23,24,25,26,28
not ind:123579,11,12,14,16,19,27

we can set x1367,10 to one group, and 9,10,14 to another.










