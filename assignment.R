# Clear plots from the R plots view:
if(!is.null(dev.list())) dev.off()

# Clean workspace - No variables at the current workspace
rm(list=ls())

#setwd("/Users/anderbarriocampos/Desktop/UPC/SIM/SIMassignment")
setwd("C:/Users/odyky/Desktop/SIM-Project")
df <- read.csv("insurance.csv")

summary(df)
#summary(df$age)

#Data types
typeof(df$age)
typeof(df$sex)
typeof(df$bmi)
typeof(df$children)
typeof(df$smoker)
typeof(df$region)
typeof(df$charges)

#CHECK FOR MISSING DATA AND DUPLICATES -> THERE IS NO MISSING DATA

#There is one duplicated
dupli <- duplicated(df)
dupli_ind <- which(dupli)
length(dupli_ind) #one duplicated row
#any(duplicated(df)) #TRUE
#length(any(duplicated(df))) #one duplicated row

#df<- df[-which(duplicated(df)), ]
df<- df[-dupli_ind, ]
any(duplicated(df)) #FALSE

summary(df)

ll <- which( df$age=="NA"); length(ll)
ll <- which( df$sex=="NA"); length(ll)
ll <- which( df$bmi=="NA"); length(ll)
ll <- which( df$children=="NA"); length(ll)
ll <- which( df$smoker=="NA"); length(ll)
ll <- which( df$region=="NA"); length(ll)
ll <- which( df$charges=="NA"); length(ll)

library(ggplot2) #Most famous library for R plotting
library(GGally)
library(car)
ggpairs(df[,c(7,1:6)]) 

#CREATE FACTOR FOR QUALITATIVE VARIABLES
#SEX, REGION and SMOKER
unique(df$region)
unique(df$sex)

#SEX TO FACTOR

# 1- Initialize a variable with all values setted at 0
df$f.sex<-0
# 2- Change its value for the cases where type of sex equals "male"
df$f.sex[df$sex=="male"]<-1
# * Are there NA's present at the type variable
table(df$sex, useNA = "ifany")
df$f.sex
table(df$f.sex) #NA's in type have been converted to 0 as they are not equal to prof 
str(df) #Type of variables present at df
df$f.sex<-factor(df$f.sex, labels=c("F","M")) #Convert variable created to factor type
str(df) #Conversion check

summary(df)

#REGION TO FACTOR

# 1- Initialize a variable with all values setted at 0
df$f.reg<-0
# 2- Change its value for the cases where type of region equals "southwest", "southeast", "northwest"
df$f.reg[df$region=="southwest"]<-3
df$f.reg[df$region=="southeast"]<-2
df$f.reg[df$region=="northwest"]<-1
# * Are there NA's present at the type variable
table(df$region, useNA = "ifany")
df$f.reg
table(df$f.reg) #WHAT IS THAT? -----> NA's in type have been converted to 0 as they are not equal to prof 
str(df) #Type of variables present at df
df$f.reg<-factor(df$f.reg,labels=c("NE","NW","SE","SW")) #Convert variable created to factor type
str(df) #Conversion check

#SMOKER TO FACTOR

# 1- Initialize a variable with all values setted at 0
df$f.smok<-0
# 2- Change its value for the cases where type of smoker equals "yes"
df$f.smok[df$smoker=="yes"]<-1
# * Are there NA's present at the type variable
table(df$smoker, useNA = "ifany")
df$f.smok
table(df$f.smok) #NA's in type have been converted to 0 as they are not equal to prof 
str(df) #Type of variables present at df
df$f.smok<-factor(df$f.smok, labels=c("No","Yes")) #Convert variable created to factor type
str(df) #Conversion check


summary(df)

#We will only continue with those factor so we delete the previous variables
df$sex <- NULL #delete sex
df$region <- NULL #delete region
df$smoker <- NULL #delete smoker
summary(df)

#DETERMINE IF THE RESPONSE VARIABLE (charges) HAS AN ACCEPTABLY NORMAL DISTRIBUTION --> NO

# 1- Histogram for graphical analysis:
hist(df$charges,freq=F,breaks=10)
# 2- Get mean and std of the variable:
m=mean(df$charges);std=sd(df$charges);m;std
# 3- What would be de density curve of a normal distribution with the same mean and std as prestige?
curve(dnorm(x,m,std),col="red",lwd=2,add=T)

shapiro.test(df$charges) #P-value < 0.05 -> We can reject H0

#ADDRESS TESTS TO DISCARD SERIAL CORRELATION

acf(df$charges)
library(lmtest)
dwtest(df$charges~1)
?dwtest
#there is no serial correlation because of the value of p-value that we cannot reject the null hypothesis
#and because all the vertical lines are inside the two horizontal blue lines except the first line...

#UNIVARIATE OUTLIERS (extreme)

# Interesting function:
calcQ <- function(x) {
  s.x <- summary(x)
  iqr<-s.x[5]-s.x[2]
  list(souti=s.x[2]-3*iqr, mouti=s.x[2]-1.5*iqr, min=s.x[1], q1=s.x[2], q2=s.x[3], 
       q3=s.x[5], max=s.x[6], mouts=s.x[5]+1.5*iqr, souts=s.x[5]+3*iqr ) }

# Univariate outlier detection:

# 1 - BMI:
par(mfrow=c(1,1))
Boxplot(df$bmi)
var_out<-calcQ(df$bmi)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")
abline(h=var_out$mouts,col="red")
llout_bmi<-which((df$bmi<var_out$souti)|(df$bmi>var_out$souts))
length(llout_bmi)

# 2 - AGE:
par(mfrow=c(1,1))
Boxplot(df$age)
var_out<-calcQ(df$age)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")
abline(h=var_out$mouts,col="red")
llout_age<-which((df$age<var_out$souti)|(df$age>var_out$souts))
length(llout_age)

# 3 - CHILDREN:
par(mfrow=c(1,1))
Boxplot(df$children)
var_out<-calcQ(df$children)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")
abline(h=var_out$mouts,col="red")
llout_children<-which((df$children<var_out$souti)|(df$children>var_out$souts))
length(llout_children)

# 4 - CHARGES:
par(mfrow=c(1,1))
Boxplot(df$charges)
var_out<-calcQ(df$charges)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")
abline(h=var_out$mouts,col="red")
llout_charges<-which((df$charges<var_out$souti)|(df$charges>var_out$souts))
length(llout_charges)

# Assign them as NA's and as charges is the target variable, it is not necessary to impute missing values, but just discard them
df[llout_charges, 'charges'] <- NA
summary(df)

# Numeric variables: remove NA by assignment of variable mean -- DELETE THIS
#CORRECT THIS!! TARGET VARIABLE NA'S DELETE THEM. -- DELETE THIS
#mm<-mean(df$charges,na.rm=T);mm  -- DELETE THIS
#df[is.na(df$charges),"charges"]<-mm  -- DELETE THIS


# Treating NA values of target variable (charges)
summary(df$charges)

#Here we will make a copy of the dataset with NA's not to delete anything
dfNA <- df
summary(dfNA)

#Now we will remove the rows where the NA value exists in the target variable
df <- df[-llout_charges,]
summary(df$charges)
summary(df)

#MULTIVARIATE OUTLIERS in a 95%

library(chemometrics)
res.mout <- Moutlier( df[ , c(1:4)], quantile = 0.95 )
str(res.mout)
res.mout

par(mfrow=c(1,1))
plot( res.mout$md, res.mout$rd )
text(res.mout$md, res.mout$rd, labels=rownames(df),adj=1, cex=0.5)
abline( h=res.mout$cutoff, lwd=2, col="red")
abline( v=res.mout$cutoff, lwd=2, col="red")

llmout <- which( ( res.mout$md > res.mout$cutoff ) & (res.mout$rd > res.mout$cutoff) );llmout
df[llmout,]
res.mout$md[llmout]
df$mout <- 0
df$mout[ llmout ] <- 1
df$mout <- factor( df$mout, labels = c("MvOut.No","MvOut.Yes"))
summary(df)

# PRELIMINARY EXPLORATORY ANALYSIS (non-parametric: because we do not have a normal distribution)
# we have to rely on (what JOSEP told me) or just assume a normal distribution (but we have checked that it does not follow a normal distribution)

library(FactoMineR)
res.con<-condes(df,4, proba = 1)
str(res.con)
names(res.con)
res.con$quanti
res.con$quali
res.con$category

plot(df[,c(4,1:3)])
cor(df[,c(4,1:3)], method="pearson")

cor(df[,c(4,1:3)],method="spearman") # Non Parametric version for general variables

#INTERACTION BETWEEN CATEGORICAL AND NUMERICAL VARIABLES --> It can be done with condes$quali

#SEX
# Numeric
with(df, tapply(charges,f.sex,summary))
# Graphics
Boxplot(charges~f.sex,data=df, id=list(n=Inf,labels=row.names(df))) # It does work (Because there are no NA's)

#REGION
# Numeric
with(df, tapply(charges,f.reg,summary))
# Graphics
Boxplot(charges~f.reg,data=df, id=list(n=Inf,labels=row.names(df))) # It does work (Because there are no NA's)

#SMOKER
# Numeric
with(df, tapply(charges,f.smok,summary))
# Graphics
Boxplot(charges~f.smok,data=df, id=list(n=Inf,labels=row.names(df))) # It does work (Because there are no NA's)


#LINEAR MODELS (just an example :))

# AGE
par(mfrow=c(1,1))
plot(df$charges,df$age,pch=19)
ma<-lm(df$charges~df$age,data=df)
summary(ma)

#check for influential data
influenceIndexPlot(ma, id.n=5)

par(mfrow=c(1,1))
residualPlot(ma)
rstan <- rstandard(ma) #Standardized residuals
rstud <- rstudent(ma) #Studentized residuals
dcook <- cooks.distance(ma) #Cook distance
dcook
leverage <- hatvalues (ma) #Leverage of observations
leverage #This is used to assess whether there can be a priori influential observations 

plot(ma$fitted.values, rstan) #Standardized residuals vs fitted values
plot(ma$fitted.values, rstud) #Studentized residuals vs fitted values

marginalModelPlots(ma)

dfbetas(ma) #Beta coefficients without observation i
# This is the effect of extracting out the observation i from the estimation

# Detection of influential data:
matplot(dfbetas(ma), type="l", col=3:4,lwd=2)
lines(sqrt(cooks.distance(ma)),col=1,lwd=3)
# See the slides for more details of these limits
abline(h=2/sqrt(dim(anscombe)[1]), lty=3,lwd=1,col=5)
abline(h=-2/sqrt(dim(anscombe)[1]), lty=3,lwd=1,col=5)
abline(h=sqrt(4/(dim(anscombe)[1]-length(names(coef(ma))))), lty=3,lwd=1,col=6)

influenceIndexPlot(ma) #NE?
llev <- which(hatvalues(ma)>3*(3/44))
llout <- Boxplot(cooks.distance(ma))
llout
length(llout) #10 values of influential data??

par(mfrow=c(2,2))
plot(ma)

# BMI
par(mfrow=c(1,1))
plot(df$charges,df$bmi,pch=10, col="red")
mb<-lm(df$charges~df$bmi,data=df)
lines(df$bmi,fitted(mb),col="blue") #xk no me salen las líneas?????
summary(mb)

# CHILDREN
par(mfrow=c(1,1))
plot(df$charges,df$children,pch=19)
mc<-lm(df$charges~df$children,data=df)
summary(mc)


