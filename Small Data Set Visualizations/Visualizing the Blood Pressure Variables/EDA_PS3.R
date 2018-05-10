# Problem Set 3

library(ggplot2)
library(NHANES)
df = NHANES
df = df[,c("BPSysAve","Age","Weight","Height","Gender")]

head(df)

colSums(is.na(df))
df = df[complete.cases(df, df$BPSysAve),]

colSums(is.na(df))

#Section1: Relationship between Average Systolic Blood Pressure and Age
ggplot(df,aes(x=Age,y=BPSysAve,color=Gender)) +  geom_point()  +
  geom_smooth( method.args= list(degree=1),col = "black",span=1) + facet_grid(~Gender) +
  ylab("Average Systolic Blood Pressure") + 
  ggtitle("Relationship between Average Systolic Blood Pressure and Age by Gender")


ggplot(df, aes(x=Age,y=BPSysAve)) +geom_boxplot(aes(group= cut_number(Age, n = 8))) +
  facet_wrap(~Gender,ncol=1)+
  ylab("Average Systolic Blood Pressure") + ggtitle("Boxplots for different age groups")


library(broom)    
df.lm= lm(BPSysAve ~ Age, data=df)
df.lm.au = augment(df.lm)
df.lm.au$Gender = df$Gender

ggplot(df.lm.au, aes(x=Age, y= .resid)) + geom_point() + geom_smooth() + 
  geom_abline(slope = 0, intercept = 0)+ facet_wrap(~Gender,ncol=1) + ylab("Residuals") +
  ggtitle("Residuals Vs Age")


# Section2: Relationship between Average Systolic Blood Pressure and Weigh
ggplot(df,aes(x=Weight,y=BPSysAve,color=Gender)) +  geom_point()  +
  geom_smooth( method.args= list(degree=1),col = "black",span=1) + facet_grid(~Gender) +
  ylab("Average Systolic Blood Pressure") + 
  ggtitle("Relationship between Average Systolic Blood Pressure and Weight by Gender")

library(broom)    
df.lm.wt = lm(BPSysAve ~ Weight + I(Weight^2), data=df)
df.lm.wt.au = augment(df.lm.wt)
df.lm.wt.au$Gender = df$Gender

ggplot(df.lm.wt.au, aes(x=Weight, y= .resid)) + geom_point() + geom_smooth() + 
  geom_abline(slope = 0, intercept = 0)+ facet_wrap(~Gender,ncol=1) +
  ggtitle("Residuals Vs Weight") +
  ylab("Residuals")


ggplot(df.lm.wt.au, aes(x=.fitted, y= sqrt(abs(.resid)))) + geom_point() + geom_smooth() + 
  facet_wrap(~Gender,ncol=1) + xlab("Fitted Values") + 
  ylab("Sqare root of absolute Residuals") +
  ggtitle("Transformed Residuals Vs Fitted Values")



# Section3: Relationship between Average Systolic Blood Pressure and Height
ggplot(df,aes(x=Height,y=BPSysAve,color=Gender)) +  geom_point()  +
  geom_smooth(col = "black", span = 1) + facet_grid(~Gender) +
  ylab("Average Systolic Blood Pressure") +
  ggtitle("Relationship between Average Systolic Blood Pressure and Height by Gender")


library(broom)    
df.lm.ht = lm(BPSysAve ~ Height + I(Height^2), data=df)
df.lm.ht.au = augment(df.lm.ht)
df.lm.ht.au$Gender = df$Gender

ggplot(df.lm.ht.au, aes(x=Height, y= .resid)) + geom_point() + geom_smooth() + 
  geom_abline(slope = 0, intercept = 0)+ facet_wrap(~Gender,ncol=1) + ylab("Residuals") +
  ggtitle("Residuals vs Height")


ggplot(df.lm.ht.au, aes(x=.fitted, y= sqrt(abs(.resid)))) + geom_point() + geom_smooth() + 
  facet_wrap(~Gender,ncol=1) + xlab("Fitted Values") + ylab("Sqare root of absolute Residuals") +
  ggtitle("Transformed Residuals Vs Fitted Values")


