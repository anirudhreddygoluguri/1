XYdata <- read.csv(file="normalised.csv", header=TRUE, sep=",")
model01 = lm(close ~open+high+low+pricetoearningratio+pricetosalesratio, XYdata)
summary(model01)
anova(model01)
a01 = AIC(model01)
plot(model01)


model02 = lm(close ~open+high+low+pricetoearningratio, XYdata)
summary(model02)
anova(model02)
a02 = AIC(model02)

model03 = lm(close ~open+high+low+pricetosalesratio, XYdata)
summary(model03)
anova(model03)
a03 = AIC(model03)

model04 = lm(close ~open+high+pricetoearningratio+pricetosalesratio, XYdata)
summary(model04)
anova(model04)
a04 = AIC(model04)

model05 = lm(close ~open+low+pricetoearningratio+pricetosalesratio, XYdata)
summary(model05)
anova(model05)
a05 = AIC(model05)

model06 = lm(close ~high+low+pricetoearningratio+pricetosalesratio, XYdata)
summary(model06)
anova(model06)
a06 = AIC(model06)

model07 = lm(close ~open+high+low, XYdata)
summary(model07)
anova(model07)
a07 = AIC(model07)

model08 = lm(close ~open+high+pricetoearningratio, XYdata)
summary(model08)
anova(model08)
a08 = AIC(model08)

model09 = lm(close ~open+high+pricetosalesratio, XYdata)
summary(model09)
anova(model09)
a09 = AIC(model09)

model10 = lm(close ~open+low+pricetoearningratio, XYdata)
summary(model10)
anova(model10)
a10 = AIC(model10)

model11 = lm(close ~open+low+pricetosalesratio, XYdata)
summary(model11)
anova(model11)
a11 = AIC(model11)

model12 = lm(close ~open+pricetoearningratio+pricetosalesratio, XYdata)
summary(model12)
anova(model12)
a12 = AIC(model12)

model13 = lm(close ~high+low+pricetoearningratio, XYdata)
summary(model13)
anova(model13)
a13 = AIC(model13)

model14 = lm(close ~high+low+pricetosalesratio, XYdata)
summary(model14)
anova(model14)
a14 = AIC(model14)

model15 = lm(close ~high+pricetoearningratio+pricetosalesratio, XYdata)
summary(model15)
anova(model15)
a15 = AIC(model15)

model16 = lm(close ~low+pricetoearningratio+pricetosalesratio, XYdata)
summary(model16)
anova(model16)
a16 = AIC(model16)

model17 = lm(close~high+low,XYdata)
summary(model17)
anova(model17)
a17 = AIC(model17)

model18 = lm(close~open+high,XYdata)
summary(model18)
anova(model18)
a18 = AIC(model18)

model19 = lm(close~open+low,XYdata)
summary(model19)
anova(model19)
a19 = AIC(model19)

model20 = lm(close~open+pricetoearningratio,XYdata)
summary(model20)
anova(model20)
a20 = AIC(model20)

model21 = lm(close~open+pricetosalesratio,XYdata)
summary(model21)
anova(model21)
a21 = AIC(model21)

model22 = lm(close~high+pricetosalesratio,XYdata)
summary(model22)
anova(model22)
a22 = AIC(model22)

model23 = lm(close~high+pricetoearningratio,XYdata)
summary(model23)
anova(model23)
a23 = AIC(model23)

model24 = lm(close~low+pricetosalesratio,XYdata)
summary(model24)
anova(model24)
a24 =  AIC(model24)

model25 = lm(close~low+pricetoearningratio,XYdata)
summary(model25)
anova(model25)
a25 = AIC(model25)

model26 = lm(close~pricetoearningratio+pricetosalesratio,XYdata)
summary(model26)
anova(model26)
a26 = AIC(model26)

model27 = lm(close ~open, XYdata)
summary(model27)
anova(model27)
a27 = AIC(model27)

model28 = lm(close ~high, XYdata)
summary(model28)
anova(model28)
a28 = AIC(model28)

model29 = lm(close ~low, XYdata)
summary(model29)
anova(model29)
a29 = AIC(model29)

model30 = lm(close ~pricetoearningratio, XYdata)
summary(model30)
anova(model30)
a30 = AIC(model30)

model31 = lm(close ~pricetosalesratio, XYdata)
summary(model31)
anova(model31)
a31 = AIC(model31)


library(ggplot2)
snps<-read.csv("normalised.csv", header = TRUE)
colnames(snps)=c("Date", "open", "high", "low", "pricetoarningsratio", "pricetosalesratio", "Close", "Symbol", "closetrue")
ggplot(snps, aes(x = Date, y = closetrue, group = Symbol, fill = Symbol)) +
  geom_line() + 
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 9, face = "bold")) +
  labs(x = "Date", y = "Close")

library("forecast")
XYdata <- read.csv(file="normalised.csv", header=TRUE, sep=",")
model = lm(close ~open+high+low+pricetoearningratio+pricetosalesratio, XYdata)
X=with(XYdata,cbind(1, open, high, low, pricetoearningratio, pricetosalesratio))
Y= XYdata$close
model = arima(Y, order = c(5,4,4))
summary(model)

XYdata <- read.csv(file="completetimeseries.csv", header=TRUE, sep=",")
timeseries= lm(close~closet1, XYdata)
summary(timeseries)
anova(timeseries)
AIC(timeseries)

XYdata <- read.csv(file="completetimeseries.csv", header=TRUE, sep=",")
total=lm(close ~open+high+low+pricetoearningratio+pricetosalesratio, XYdata)
summary(total)
anova(total)
AIC(total)

XYdata <- read.csv(file="normalised.csv", header=TRUE, sep=",")
dt = sort(sample(nrow(XYdata), nrow(XYdata)*.85))
train<-XYdata[dt,]
test<-XYdata[-dt,]
modfit<-lm(close ~ open+high+low+pricetoearningratio+pricetosalesratio, data= train)
predictest<-predict(modfit, XYdata[-dt,])
exp(coef(modfit))
predict(modfit,newdata = test)
pre<-predict(modfit,newdata = test)
accuracy=table(pre,test[,"close"])
sum(diag(accuracy))/sum(accuracy)
summary(modfit)
anova(modfit)
