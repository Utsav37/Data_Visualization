df = read.table("minnesota.barley.yield.txt",header = T)
library(ggplot2)
ggplot(df,aes(x=year,y=yield,color = site)) + geom_line() + geom_smooth()+facet_wrap(~site,ncol=3) +
  ggtitle("Barley Yield varied by Year at each location")

ggplot(df,aes(x=year,y=yield,color = gen)) + geom_line()+facet_wrap(~site,ncol=3) + 
  ggtitle("Barley Yield varied by Year at each location for diferent types of Barley")

library(MASS)
df.rlm = rlm(yield ~ gen+(year*site), psi = psi.bisquare,data = df)

library(broom)
df.rlm.au = augment(df.rlm)

ggplot(df.rlm.au, aes(y= .fitted, x= year)) + geom_line()+ geom_smooth(span=0.75)+
  facet_wrap(~site,ncol=3) + ggtitle("Fitted values of yield over different sites by Year")



ggplot(df.rlm.au, aes(y= yield, x= .resid)) + geom_point()+ geom_smooth()+
  facet_wrap(~year,ncol=3) + ggtitle("Fitted values of yield over different sites by Year")



barley = df
morris1932fixed = barley$yield[barley$site == "Morris" & barley$year == 1931]
morris1931fixed = barley$yield[barley$site == "Morris" & barley$year == 1932]
barley.fixed = barley
barley.fixed$yield[barley$site == "Morris" & barley$year == 1932] = morris1932fixed
barley.fixed$yield[barley$site == "Morris" & barley$year == 1931] = morris1931fixed

barley.rlm = rlm(yield ~ gen + year * site, psi = psi.bisquare, data = barley.fixed)

barley.rlm.au = augment(barley.rlm)




ggplot(barley.rlm.au, aes(y= yield, x= .resid)) + geom_point()+ geom_smooth()+
  facet_wrap(~year,ncol=3) + ggtitle("Fitted values of yield over different sites by Year")


