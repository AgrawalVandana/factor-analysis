data <- read.csv(file = "C:/Business Analytics/Winter/Multivariate/Assigment 1/audio.csv")
data

rownames(data)=data[,1]
data=data[,-1]
data
summary(data)
var1=var(data$L500)
var2=var(data$L1000)
var3=var(data$L2000)
var4=var(data$L4000)
var5=var(data$R500)
var6=var(data$R1000)
var7=var(data$R2000)
var8=var(data$R4000)

rho=cor(data)

## Step 2 - Compute the eigenvalues and eigenvactors of the correlation matrix
eigenvalues=eigen(rho)$values
eigenvalues
eigenvectors=eigen(rho)$vectors
eigenvectors
(eigenvalues)>1
m=2
sum(eigenvalues[1:m])/

## Step 3 - Compute Estimated Factor Loadings
m=2
L=matrix(nrow=8,ncol=m)
for (j in 1:m)
  {
  L[,j]=sqrt(eigenvalues[j])*eigenvectors[,j]  
}

L

## Step 4 - Compute common variance and unique variance

common=rowSums(L^2)
unique=1-common  ## this diagonal of error matrix

common
unique

## Step 5 - Check the model to reproduce correlation

phi=diag(8)*unique

recreate=L%*%t(L)+phi
recreate

rho

## Step 6 - Create Residual Matrix
options(digits=3)
residual=rho-recreate
residual  ## check to see if off-diagonal elements are "small"

sum(residual[lower.tri(residual)]^2)*2  ## sum of square of off-diagonal elements

sum(sqrt(residual[lower.tri(residual)]^2))/(10-2)  ## sum of square of off-diagonal elements


sum(eigenvalues[3:8]^2)  ## sum of square of non-used eigenvalues

## Step 7  - Plot pairs of loadings to interpret factor loadings
## if we can't tell, we may need to do a varimax rotation

plot(L[,1],L[,2],col=1:8,xlab="Loading 1",ylab="Loading 2")
text(L[,1],L[,2],names(data))

## Step 8

install.packages('psych')
library(psych)

## should reproduce our results
fit2 <- principal(data, nfactors=2, rotate="none")

fit2
round(fit2$residual,2)

fit <- principal(data, nfactors=2, rotate="varimax")
fit
plot(fit$loadings[,1],fit$loadings[,2],col=1:5)
text(fit$loadings[,1],fit$loadings[,2],names(data))
round(fit$residual,2)

install.packages('nFactors')
library(nFactors)
ev <- eigen(cor(data)) # get eigenvalues
ap <- parallel(subject=nrow(data),var=ncol(data),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
