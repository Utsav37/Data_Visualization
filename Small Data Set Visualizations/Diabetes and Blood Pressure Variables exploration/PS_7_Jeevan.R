#Q1

library(NHANES)
library(ggplot2)
df1 = NHANESraw
df = df1[,c("BPSysAve","Age","Weight","Height", "WTMEC2YR","Gender")]



df = df[complete.cases(df, df$BPSysAve),]
colSums(is.na(df))
dim(df)

df$WTMEC2YR = df$WTMEC2YR/s



ggplot(df, aes(x=Age,y=BPSysAve,color=Gender,size=WTMEC2YR)) +
  geom_point()  +
  geom_smooth(method.args= list(degree=1),span=0.75) + 
  facet_wrap(~(cut_number(Height, n = 2)) + ~(cut_number(Weight,n = 2))) +
  ylab("Average Systolic Blood Pressure") + 
  ggtitle("Average BP vs Age- Weight increases from left to right and Height increases from Top to Bottom")



df.lm1 = lm(BPSysAve ~ Age + Gender + Height + Weight  , data = df, weights =WTMEC2YR )


new.grid = expand.grid(Age = seq(10,80,10), Gender = c("female", "male"), Height = c(110,160,210), Weight = c(20,80,240))


new.predict = predict(df.lm1, newdata = new.grid)

new.total = data.frame(new.grid, fit = as.vector(new.predict))



ggplot(new.total, aes(x=Age,y=fit, group = Height, color = factor(Height))) +
  geom_smooth() + facet_wrap(~Weight) + ylab("Fitted value of BPSysAvg") +
  ggtitle("Fitted values for different Heights and conditional on Weights")




#Q2

library(NHANES)
library(ggplot2)
df2 = NHANESraw
dfd = df2[,c("Diabetes","Age","Weight","Height", "WTMEC2YR","Gender")]
dim(dfd)
colSums(is.na(dfd))


dfd = dfd[complete.cases(dfd, dfd$Height),]
colSums(is.na(dfd))
dim(dfd)



dfd$Diabetes = as.numeric(dfd$Diabetes)

dfd$Diabetes = dfd$Diabetes - 1


ggplot(dfd,aes(x=Age, y= Diabetes, color = Gender)) + geom_point() +
  geom_smooth(method = "loess")+
  facet_wrap(~(cut_number(Height, n = 2)) + ~(cut_number(Weight,n = 2))) +
  ylab("Probability of having diabetes")+
  ggtitle("Diabetes Probality vs age, Height increases from Top to Bottom 
and Weight increases from Left to Right")



dfd.logit = glm(Diabetes ~ Age + Gender + Height + Weight, family = "binomial", data = dfd)


summary(dfd.logit)



new.grid = expand.grid(Age = seq(10,80,10), Gender = c("female", "male"), Height = c(80,160,200), Weight = seq(10,240,60))





dfd.pred = predict(dfd.logit, type = "response", newdata = new.grid)




dfd.pred.df = data.frame(new.grid, fit = as.vector(dfd.pred))

ggplot(dfd.pred.df, aes(x=Age,y=fit,color = factor(Height))) + geom_point() +
  geom_smooth() + 
  facet_wrap(~(Weight) + ~(Gender)) +
  ylab("Probability of a person having Diabetes") +
  ggtitle("Diabetes Probability over age for different Heights conditioned on Weights")



#Q3

df3 = df2[,c("Diabetes","Age","Weight","Height", "WTMEC2YR","Gender", "HHIncomeMid", "Poverty", "Pulse", "DirectChol", "TotChol")]
df3 = df3[,c("Diabetes","Age","Weight","Height","Gender", "Pulse")]

df3 = df3[complete.cases(df3, df3$Pulse),]


df3 = df3[,c("Diabetes","Age","Height","Weight","Pulse", "Gender")]

df3$Diabetes = as.numeric(df3$Diabetes)

df3$Diabetes = df3$Diabetes - 1

summary(df3)
dim(df3)


ggplot(df3,aes(x=Age, y= Diabetes, color = Gender)) + geom_point() + geom_smooth(method = "loess")+
  facet_wrap(~(cut_number(Weight, n = 2)) + ~(cut_number(Pulse,n = 2)))



df3.logit = glm(Diabetes ~ (Age*Pulse*Weight*Height) + Gender, family = "binomial", data = df3)


summary(df3.logit)



df3.g = expand.grid(Age = seq(2, 80, 2), Pulse = c(36, 72, 108, 144),  Height = seq(80,200,40),
                    Weight = seq(20, 240, 40) , Gender = c("female","male"))


fit = predict(df3.logit, type = "response", newdata = df3.g)

df3.pred.df = data.frame(df3.g, Diabetes = as.vector(fit))



ggplot(df3.pred.df, aes(x=Age, y = fit, color = factor(Pulse))) + geom_smooth() +
  facet_wrap(~(cut_number(Weight, n = 2))+ ~(cut_number(Height, n = 2)))


