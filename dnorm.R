#####load packages####
library(dplyr)
library(magrittr)
library(ggplot2)

####work####
dt<-data.frame(x="x",mean=0.51,ci=0.016)
p<-ggplot(data=dt, aes(x=x,y=mean))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci))
p



####10.30####
oildt<-c(11,10,16,15,18,12,25,20,18,24)
oilsd<-5
oilmean<-mean(oildt)
alpha<-0.05
LCL<-round(oilmean-qnorm(1-alpha/2)*oilsd/sqrt(10),2)
UCL<-round(oilmean+qnorm(1-alpha/2)*oilsd/sqrt(10),2)


ggplot(data = data.frame(x=oildt), aes(x))+
  geom_area(stat = "function", fun=dnorm, args = list(mean=oilmean,sd=oilsd),fill="grey80", xlim=c(LCL,UCL))+
  stat_function(fun = dnorm,args = list(mean=oilmean,sd=oilsd))+
  geom_point(data = data.frame(x=LCL,y=dnorm(LCL,mean=oilmean,sd=oilsd)),aes(x,y))+
  geom_text(data = data.frame(x=LCL,y=dnorm(LCL,mean=oilmean,sd=oilsd)),aes(x,y,label=paste0("LCL = ", as.character(LCL))),hjust=1.1)+
  geom_point(data = data.frame(x=UCL,y=dnorm(UCL,mean=oilmean,sd=oilsd)),aes(x,y))+
  geom_text(data = data.frame(x=UCL,y=dnorm(UCL,mean=oilmean,sd=oilsd)),aes(x,y,label=paste0("UCL = ", as.character(UCL))),hjust=-0.1)+
  geom_text(aes(x=oilmean,y=0.03,label="95% CI"))+
  scale_x_continuous(limits = c(oilmean-3*oilsd,oilmean+3*oilsd))+
  labs(y="Probability density")+
  ggsave("oildnorm.png",height = 10,width = 15,units = "cm")


####10.38####
grasssd<-0.1
grassmean<-0.51
alpha<-0.01
LCL<-round(grassmean-qnorm(1-alpha/2)*grasssd/sqrt(250),2)
UCL<-round(grassmean+qnorm(1-alpha/2)*grasssd/sqrt(250),2)


ggplot(data = data.frame(x=0.51), aes(x))+
  geom_area(stat = "function", fun=dnorm, args = list(mean=grassmean,sd=grasssd),fill="grey80", xlim=c(LCL,UCL))+
  stat_function(fun = dnorm,args = list(mean=grassmean,sd=grasssd))+
  geom_point(data = data.frame(x=LCL,y=dnorm(LCL,mean=grassmean,sd=grasssd)),aes(x,y))+
  geom_text(data = data.frame(x=LCL,y=dnorm(LCL,mean=grassmean,sd=grasssd)),aes(x,y,label=paste0("LCL = ", as.character(LCL))),hjust=1.1)+
  geom_point(data = data.frame(x=UCL,y=dnorm(UCL,mean=grassmean,sd=grasssd)),aes(x,y))+
  geom_text(data = data.frame(x=UCL,y=dnorm(UCL,mean=grassmean,sd=grasssd)),aes(x,y,label=paste0("UCL = ", as.character(UCL))),hjust=-0.1)+
  geom_text(aes(x=grassmean,y=1.5,label="99% CI"))+
  scale_x_continuous(limits = c(grassmean-3*grasssd,grassmean+3*grasssd))+
  labs(y="Probability density")+
  ggsave("grassdnorm.png",height = 10,width = 15,units = "cm")



####rtrim####
red   <- "#E41A1C" # Set up some nice colors
blue  <- "#377EB8"
green <- "#4daf4a"
lgray <- gray(0.8)
mgray <- gray(0.5)
dgray <- gray(0.2)

mu = 0.51                          # mean# mx 即為平均值 mu 的點估計 
sd = 0.1                           # standard deviation
alpha = 0.01                          # 99% confidence interval
n = 250 # n = 樣本數
r1 = qnorm(alpha/2) # 信賴區間，下半截掉 alpha/2
r2 = qnorm(1-alpha/2) # 信賴區間，上半截掉 alpha/2
lo = mu-r2*sd/sqrt(n) # 信賴區間下限
hi = mu-r1*sd/sqrt(n) # 信賴區間上限
lo
hi

# Full normal distribution
x <- seq(mu-3*sd, mu+3*sd, len=100)
y <- dnorm(x, mean=mu, sd=sd)

# Use quantile function to compute the confidence interval (CI)
lo <- qnorm(alpha/2,   mean=mu, sd=sd)  # lower CI bound
hi <- qnorm(1-alpha/2, mean=mu, sd=sd)  # upper CI bound

# start with an empty plot
plot(NULL,NULL, type='n', xlim=range(x), ylim=range(y),
     xlab=NA, ylab="Probability density", las=1, xaxt="n")

xci <- seq(lo, hi, len=100)             # background: confidence interval
yci <- dnorm(xci, mean=mu, sd=sd)
xx <- c(xci, rev(xci))
yy <- c(0*yci, rev(yci))
polygon(xx,yy,col=gray(0.9), border=NA)

lines(x,y, col=red, lwd=2)              # Foreground: complete distribution

# Annotation and decoration
lines(c(mu,mu), c(0,dnorm(mu,mean=mu,sd=sd)), lty="dashed", lwd=0.5) # mean
lines(c(mu-sd,mu-sd), c(0,dnorm(mu-sd,mean=mu,sd=sd)), lty="dashed", lwd=0.5) # mu - s.d.
lines(c(mu+sd,mu+sd), c(0,dnorm(mu+sd,mean=mu,sd=sd)), lty="dashed", lwd=0.5) # mu + s.d.
abline(h=0, lwd=0.5) # proper y=0 line
text(mean(x), mean(y), sprintf("%.0f%%", 100*(1-alpha)))
axis(side = "1", at=c(0.2,0.4,0.6,0.8))

