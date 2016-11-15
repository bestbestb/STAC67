sale=read.table("estatsale.txt",header=F)
head(sale)
dim(sale)
attach(sale)
V6=factor(V6)
V8=factor(V8)
V13=factor(V13)
higq=as.numeric(V10>=3) #set median quality as category reference
sale=cbind(sale[,-10],higq,lowq)
sale=sale[,2:14] #get rid of the first column cuz it has nothing to do with price
colnames(sale)=c("Y","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")
dim(sale)
head(sale)
n=dim(sale)[[1]] ## number of observations
n
p=13 ## number of regression coefficients parameters
fullreg=lm(Y~.,data=sale)
summary(fullreg)
anova(fullreg)
MSE=3.3135e+09 
yhati=fitted(fullreg) # predicted values
ei=residuals(fullreg) # residuals
X=as.matrix(cbind(1, sale[, c(2:13)])) ## X matrix
hat_mat=X %*% solve(t(X) %*% X) %*% t(X) ## hat matrix
hii=diag(hat_mat) ## diagonal elements of that hat matrix
MSE=(summary(fullreg)$sigma)^2 ## mean squared error
s2ei=MSE * (1 - hii) ## estimated variance of the ith residual
I=matrix(0, nrow = dim(hat_mat)[[1]], ncol = dim(hat_mat)[[1]])
diag(I)=1 ## I: identity matrix 
s2e=MSE * (I - hat_mat) ## estimated variance-covariance matrix of the residuals 
ri=ei/sqrt(s2ei) ## studentized residuals
SSE=sum(ei^2) ## error sum of squares
ti=ei * sqrt((n - p - 1)/(SSE * (1 - hii) - ei^2)) ## studentized deleted residuals
alpha=0.05
BCV=qt(1 - alpha/(2*n), n - p - 1) ## Bonferroni Critical Value 
absti=abs(ti)
index_of_outlier=c(1:n)[absti >= BCV]
index_of_outlier ## the 72nd,73rd observations in Y direction are outliers##

## Use of Hat Matrix to find outlying cases with regard to X values 
hii=diag(hat_mat) ## diagonal elements of that hat matrix
hsum=sum(hii)
hsum
hmean=hsum/n
cutoff=2 * hmean
index_of_outlier=c(1:n)[hii > cutoff]
index_of_outlier #there are 45 outliers in X direction
summary(hii)
hii[index_of_outlier]
sale[c(index_of_outlier), c(2:13)]
apply(sale[, c(2:13)], 2, mean)
#Using guideline 2, no observations are outlying in X directions 
index_of_outlier2=c(1:n)[hii > 0.5]
index_of_outlier2


## Idenfifying Influential Cases - DFFITS, Cook's Distance, and DFBETAS ##

## Computing DFFITS_i ##
dffits=ti * sqrt(hii/(1 - hii))
n
adffits=abs(dffits)
cutoff= 1
index_of_infl=c(1:n)[adffits > cutoff]
index_of_infl # the 103rd 104th observations are influential outliers

## Cook's distance ##
Di=((ei^2)/ (p * MSE)) * (hii/(1 - hii)^2)
## percentile of F(p, n - p) distribution
PERF=pf(Di, df1 = p, df2 = (n - p))
cbind(Di, PERF)
index_of_infl=c(1:n)[PERF >= 0.50]
index_of_infl 
index_of_infl=c(1:n)[PERF >= 0.10]
index_of_infl
#no influential outliers found

## DFBETAS ##


## R function to compute DFBETAS ##

DFBETAS <- function(data, form){
  ## we assume that the response variable is in the first column of data ##
  n <- dim(data)[[1]] ## number of observations ##
  X <- as.matrix(cbind(1, data[, - 1])) ## X matrix
  invXX <- solve(t(X) %*% X) ## inverse of t(X)X ##
  dfbetas <- NULL
  for(i in 1:n){
    j.in <- c(1:n)[(c(1:n) != i)]
    data.train <- data[j.in,]
    data.train <- as.data.frame(data.train)
    
    fit.all <- lm(form, data = data) ## fitting model using all of the observations 
    coef.all <- coefficients(fit.all) # coefficients of the model that uses all of the observations
    
    fit.train <- lm(form, data = data.train) ## fitting model using training data ##
    coef.train <- coefficients(fit.train) # coefficients of the model that uses all of the observations
    
    MSE_i <- summary(fit.train)$sigma^2
    c_kk <- diag(invXX)
    
    dfbetas_i <- (coef.all - coef.train)/sqrt(MSE_i * c_kk)
    dfbetas <- rbind(dfbetas, dfbetas_i)
  }
  rownames(dfbetas) <- c(1:n)
  print(dfbetas)
}

form <- as.formula(Y ~ .)
DFBETAS(data = sale, form)

n
##[[1]] 46

## consider the data size is medium ##

## None of the DFBETAS is greather than 1 ##

## Hence, no cases has influence on the regression coefficients ##


## Multicollinearity: Variance Inflation Factor ##

VIF <- function(data){
  ## first column of "data" contains the response variable ##
  ## the columns starting from second to the last contain the predictor variables ##
  p <- dim(data)[[2]]
  colnames(data) <- c("Y", paste("X", c(1:(p - 1)), sep = ""))
  vif <- NULL
  for(i in 1:(p - 1)){
    form <- paste(paste("X", i, sep = ""), paste(paste("X", c(1:(p - 1))[-i], sep = ""), collapse=" + "), sep = " ~ ")
    form <- as.formula(form)
    fit.reg <- lm(form, data = data)
    R2 <- summary(fit.reg)$r.squared
    vifk <- 1/(1 - R2)
    vif <- c(vif, vifk)
  }
  cat("Variance Inflation Factors = ", vif, "\n\n")
  maxvif <- max(vif)
  cat("Maximum VIF = ", maxvif, "\n\n")
  meanvif <- mean(vif)
  cat("Mean VIF = ", meanvif, "\n\n")
}

VIF(sale)

pairs(sale)

redureg=lm(Y~X1+X2+X3+X5+X7+X8+X9+X10+X11+X12,data=sale)
anova(redureg)
f=(((3.3417e+10)+(1.2314e+08))/3)/MSE
f
qf(1-0.05,3,n-p)
library("leaps")
leaps(x = sale[, c(2:13)], y = sale[, 1], method = c("r2"))
leaps(x = sale[, c(2:13)], y = sale[, 1], method = c("adjr2"))
leaps(x = sale[, c(2:13)], y = sale[, 1], method = c("Cp"))
sale=as.data.frame(sale)
null=lm(Y ~ 1, data = sale) ## Model with Intercept only ##
full=lm(Y ~ ., data = sale) ## Model with all 12 predictors ##
step(null, scope=list(lower=null, upper=full), direction="forward") ## Forward Selection ##
step(full, data= sale, direction="backward") ## Backward Elimination ##

#add interaction terms into model
salen=sale[,-c(5,7,12)]#new regression function without x4,x6 and x11
head(salen)
myfun=function(x) x-mean(x)
salec=apply(salen[2:10],2,myfun)#xi=Xi-mean(Xi)
sale2=cbind(sale[,1],salec)
detach(sale)
sale2=as.data.frame(sale2)
colnames(sale2)=c("Y","x1","x2","x3","x5","x7","x8","x9","x10","x12")
attach(sale2)
x1x2=x1*x2
x1x3=x1*x3
x5x9=x5*x9
polreg=lm(Y~x1+x2+x3+x5+x7+x8+x9+x10+x12+x1x2+x1x3+x5x9)
anova(polreg)
mse=3.1898e+09 
F=((6.2899e+10)+(1.7648e+09)+(3.3851e+09)/3)/mse
F
qf(1-0.05,3,n-13)
polreg1=lm(Y~x1+x2+x3+x5+x7+x8+x9+x10+x12+x1x2)
anova(polreg1)
MSE1=3.1874e+09 
f21=((6.2899e+10)/1)/MSE1
f21 #19.73364 included
qf(1-0.05,1,n-11) #3.859721
polreg2=lm(Y~x1+x2+x3+x5+x7+x8+x9+x10+x12+x1x2+x1x3)
anova(polreg2)
MSE2=3.1902e+09
f22=((1.7648e+09)/1)/MSE2
f22 #0.5531942 excluded
qf(1-0.05,1,n-12)#3.859757
polreg3=lm(Y~x1+x2+x3+x5+x7+x8+x9+x10+x12+x1x2+x5x9)
anova(polreg3)
MSE3=3.1850e+09
f23=((4.4138e+09)/1)/MSE3
f23 # 1.385808 excluded
qf(1-0.05,1,n-12)#3.859757


