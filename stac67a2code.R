copier=read.table("copier.txt", header=F)
colnames(copier)=c("y","x")
copier
attach(copier)
dotchart(x, xlab="number of copiers serviced")
xbar=mean(x)
ybar=mean(y)
ssx=sum((x-xbar)^2)
spxy=sum((y-ybar)*(x-xbar))
b1=spxy/ssx
b1
b0=ybar-b1*xbar
b0

yhat=b0+b1*x
residual=y-yhat
stem(residual)
plot(yhat,residual)
plot(x,residual)

  email=read.table("email.txt",header=T)
  attach(email)
  boxplot(emails)


rankofres=rank(residual)
n=length(y)
SSE=sum(residual^2)
MSE=SSE/(n-2)
zscore=qnorm((rankofres-0.375)/(n+0.25))
expres=zscore*sqrt(MSE)
plot(expres,residual,xlab="expected values",ylab="residuals")

plot(residual,ylab="residuals")

e2=residual^2
spe2x=sum((e2-mean(e2))*(x-mean(x)))
ssx=sum((x-mean(x))^2)
b1e2=spe2x/ssx 
SSE=sum(e2)
SSRsr=b1e2^2*ssx
chi2BP=(SSRsr/2)/((SSE/n)^2)
chi2BP

copier1=read.table("copier1.txt",header=F)
attach(copier1)
plot(V3,residual,xlab="x2",ylab="residuals")
plot(V4,residual,xlab="x3",ylab="residuals")


electr=read.table("electr.txt",header=F)
electr
attach(electr)
plot(V1,V2,xlab="x",ylab="residuals")

drug=read.table("drug.txt",header=F)
drug
attach(drug)
plot(V1,V2,xlab="x",ylab="residuals")

x=V1
e=V2
e2=e^2
spe2x=sum((e2-mean(e2))*(x-mean(x)))
ssx=sum((x-mean(x))^2)
b1e2=spe2x/ssx
SSE=sum(e2)
SSRsr=b1e2^2*ssx
chi2BP=(SSRsr/2)/(SSE/length(e))^2
chi2BP

SSE=sum(resi^2)
n=length(x)
MSE=SSE/(n-2)
s2b0=MSE*((1/n)+xbar^2/sum((x-xbar)^2))
sb0=sqrt(s2b0)
t=qt((1-0.05/4), n-2)
CI0=c(b0-t*sb0, b0+t*sb0)
CI0

s2b1=MSE/sum((x-xbar)^2)
sb1=sqrt(s2b1)
CI1=c(b1-t*sb1, b1+t*sb1)
CI1

SP2=2*qf(1-0.1,2,n-2)
SP=sqrt(SP2)
SP

BP=qt(1-0.1/(2*2),n-2)
BP

yhat1=b0+b1*4
s2pred=(1+1/n+(4-xbar)^2/sum((x-xbar)^2))*MSE
spred=sqrt(s2pred)
CIy1=c(yhat1-BP*spred,yhat1+BP*spred)
CIy1

yhat2=b0+b1*7
s2pred2=(1+1/n+(7-xbar)^2/sum((x-xbar)^2))*MSE
spred2=sqrt(s2pred2)
CIy2=c(yhat2-BP*spred2,yhat2+BP*spred2)
CIy2

typo=read.table("typo.txt",header=F)



typo=read.table("typo.txt",header=F)
attach(typo)
x=V1
y=V2
spxy=sum(x*y)
sxx=sum(x^2)
b1=spxy/sxx
b1


plot(x,y,xlab="number of galleys for a manuscript",ylab="total cost")
abline(a=0,b=b1)

n=12
yhat=b1*x
residual=y-yhat
SSE=sum(residual^2)
MSE=SSE/(n-1)
s2b1=MSE/sum(x^2)
sb1=sqrt(s2b1)
t=(17.5-18.0283)/sb1
t
tsta=2.718
y1=b1*10
s2pred=(1+10^2/sum(x^2))*MSE
spred=sqrt(s2pred)
CIy1=c(y1-tsta*spred,y1+tsta*spred)
CIy1

flavor=read.table("flavor.txt", header=F)
attach(flavor)
n=length(x)
X=V1
Y=V2

UnitVec <- rep(1, n)
X <- cbind(UnitVec, x)
colnames(X) <- c("1", "x")
Y <- matrix(y, ncol = 1)
YY <- t(Y) %*% Y
YY
XX <- t(X) %*% X
XX
XY <- t(X) %*% Y
XY
b <- solve(XX) %*% XY
b
XXIN=solve(XX)
XXIN
yhat <- X %*% b
e <- Y - yhat
e
J <- matrix(1, nrow = n, ncol = n)
SSR <- t(b) %*% t(X) %*% Y - (t(Y) %*% J %*% Y)/n
SSR
SSE <- t(Y) %*% Y - t(b) %*% t(X) %*% Y
SSE
MSE <- sum(e^2)/3
s2b <- MSE * solve(t(X) %*% X)
s2b

Xh <- -6
Xh <- cbind(1,Xh)
Yhhat <- Xh%*%b
Yhhat

s2Yhhat <- MSE*Xh%*%solve(t(X)%*%X)%*%t(Xh)
s2Yhhat
H <- X %*% solve(t(X) %*% X) %*% t(X)
H
I <- matrix(0, nrow = 5, ncol = 5)
diag(I) <- 1
s2e <- (I - H) * MSE
s2e

brand=read.table("brand.txt",header=F)
colnames(brand)=c("y","x1","x2")
pairs(brand)
cor(brand)

attach(brand)
n <- length(x1)
UnitVec <- rep(1, n)
x <- cbind(UnitVec, x1, x2)
XX <- t(x) %*% X
XY <- t(x) %*% y
XXINV <- solve(xx)
b=XXINV%*%XY
b

yhat <- x%*%b
e <- y-yhat
e
boxplot(e)

plot(e~yhat)
plot(e~x1)
plot(e~x2)
x1x2=x1*x2
plot(e~x1x2)
qqnorm(e)
reg=lm(y~x1+x2,brand)
bptest(reg)
SSPE=57
SSE=t(y) %*% y - t(b) %*% t(x) %*% y
SSE
F = ((SSE - SSPE)/(8 - 3))/(SSPE/(16 - 8)) 
F
p=3
MSE=sum(e^2)/(n-p)
J=matrix(1, nrow = n, ncol = n)
SST=t(y) %*% y - (t(y) %*% J %*% y)/n
SSR=t(b) %*% t(x) %*% y - (t(y) %*% J %*% y)/n
MSR=SSR/(p-1)
F1=MSR/MSE
F1
pv=1-pf(f,2,n-2-1)
pv
p=dim(x)[[2]]
B=qt(1-0.01/(2*2), n-p)
s2b=MSE * solve(t(x) %*% x)
s2b
sb=sqrt(diag(s2b))
sb
CIb1=c(4.425 - B * 0.3011197, 4.425 + B * 0.3011197)
CIb1
CIb2=c(4.375 - B * 0.6733241, 4.375 + B * 0.6733241)
CIb2
R2=SSR/SST
R2
r=cor(y,yhat)
R21=r^2
R21

xh=cbind(1,x1=5,x2=4)
yhhat=xh%*%b
t=qt(1-0.01/2,n-p)
s2yhhat=MSE*xh%*%solve(t(x)%*%x)%*%t(xh)
syhhat=sqrt(s2yhhat)
CIyh=c(yhhat-t*syhhat, yhhat+t*syhhat)
CIyh

s2pred=MSE+s2yhhat
spred=sqrt(s2pred)
CIpr=c(yhhat-t*spred, yhhat+t*spred)
CIpr

ATP=read.table("ATP.txt", header=T)
head(ATP)
attach(ATP)
qqnorm(atp.youngest)
qqline(atp.youngest)
qqnorm(data=ATP)
qqnorm(atp.oldest)
qqline(atp.oldest)
friday=read.table("friday.txt", header=T)
friday
attach(friday)
t.test(F6,F13,paired=T,alternative="less")
t.test(F6,F13,alternative="less")

uremia=read.table("uremia.txt", header=T)
uremia
attach(uremia)
boxplot(morphine~group,data=uremia)
uremia.aov=aov(morphine~group)
summary(uremia.aov)
TukeyHSD(uremia.aov)

y3=c(0,0,0,0,0,0,0,0,0,2,4,4,6)
var(y3)

(0*8+1*2+3*1)

y4=c(0,0,0,0,0,0,0,0,1,1,3)
var(y4)

golf=read.csv("golfsrs.csv", header=T)
attach(golf)
mean(wkend9)
plot(wkend9,backtee)
cor(wkend9,backtee)
wkend9.lm = lm(wkend9 ~ backtee, data=golf)

hotdog=read.table("hotdog.txt", header=F)
attach(hotdog)
colnames(hotdog)=c"""""")


hotdog=read.table("hotdog.txt", header=F)
attach(hotdog)
sum(V2)
sum(V3)
yhat= sum(V3)/sum(V2)
n=20
N=124
M=sum(V2)/n
vyhat=(1-n/N)*(1/(n*(M^2)))*(sum((V3-yhat*V2)^2)/(n-1))
vyhat

28*0+6*1+2+3*3+4*4+5*3+6*2+7+8*2
x=(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,2,3,3,3,4,4,4,4,5,5,5,6,6,7,8,8)
var(x)


yi=wkend9
ybar=mean(wkend9)
N=16883
n=120
s2=(1/(n-1))*sum((yi-ybar)^2)
varybar=(s2/n)*(1-n/N)
seybar=sqrt(varybar)
seybar

b0=-4.103503
b1=0.004584038
x=backtee
yhat=b0+b1*x
e=yi-yhat
se2=(1/(n-1))*sum(e^2)
varyr=(1-n/N)*(se2/n)
seyr=sqrt(varyr)
seyr