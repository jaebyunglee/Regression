rm(list=ls())
set.seed(2019)
n = 200; p = 50
b.vec = 1/(1:p)
x.mat = matrix(rnorm(n*p),n,p)
y.vec = x.mat%*%b.vec+rnorm(n)
y.vec = (y.vec>0)+0

xy.df = data.frame(y.vec,x.mat)
glm1 = glm(y.vec~.,data=xy.df,family = "binomial")
glm2 = glm(y.vec~1,data=xy.df,family = "binomial")

# likelihood ratio test
# H0 : 모수는 u_0이다. vs H1 : 모수는 u_1이다.
# p-value 가 작으면 H0 기각
# LR test에서 H0 은 NULL모델 따라서 p-value가 작으면 모든 모수는 0이다 기각, 모형이 유의미

#1
anova(glm2,glm1)
#2
dev = glm2$deviance - glm1$deviance
df = glm2$df.residual - glm1$df.residual
1-pchisq(dev,df)
#3
dev = glm1$null.deviance - glm1$deviance
df = glm1$df.null - glm1$df.residual
1-pchisq(dev,df)
#4
library(rms)
lrtest(glm2,glm1)
#5
lrm = lrm(y.vec~.,data=xy.df)
lrm
