credit=read.csv("C:\\Users\\dell\\Desktop\\dataset\\creditcardProject.csv")
mean(credit$Class)
median(credit$Class)
sd(credit$Class)
var(credit$Class)
library(moments)
skewness(credit$Class)
head(credit$Class)
tail(credit$Class)
kurtosis(credit$Class)
table(credit$Class)

normalize=function(x){
  return(x-min(x)/max(x)-min(x))
}

credit_n=as.data.frame(lapply(credit[,1:30], normalize))

train=credit_n[1:284000,]
test=credit_n[284001:284807,]

train_cr=credit[1:284000,]
test_cr=credit[284001:284807,]

?glm
#logistic regression--bcoz we are having 2 outcomes in dependent variable(exited)
matrix=glm(formula = Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12, data = credit, family = binomial)
summary(matrix)
?predict
####prediction  of data
p1=predict(matrix,credit,type="response")
p1   

##finding the percentage of finding a probability
p=exp(credit$Class)/(1+exp(credit$Class))   #convert log value....   ###sigmoid function....

###if probability value >0.5 than the chance of getting v engine 1 otherwise 0
pr=ifelse(p1>0.5,1,0)
pr

##create a confusion matricx
tab=table(predict=pr,actual=credit$Class)
tab
##acc
acc=(284261+263)/(284261+229+54+263)
acc#99.64%




#knn
library(class)
knn=knn(train,test,cl=train_cr$Class,k=3)
a=table(knn,test_cr$Class)
a
accuracy=sum(diag(a)/sum(a))
accuracy#100%




#kmeans
View(credit)
new = credit[-31]
View(new)
m = kmeans(new,2)
m
plot(credit[c("V1","V2")],col=m$cluster) ##kmeans model.
plot(credit[c("V1","V2")],col=credit$Class)  ##original data
c=table(credit$Class,m$cluster)
c
accuracy = sum(diag(c))/sum(c)
accuracy#46%








