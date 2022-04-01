rm(list=ls())

x<-read.csv('D:/PMD/predata/d_bi.csv',header=T)

attach(x)
names(x)

summary(x$D_BI)
summary(as.factor(x$D_BI))


# multinomial logi reg
a<-rep(0,nrow(x))
for (i in 1:nrow(x)) {
  if (-100<=D_BI[i] && D_BI[i]<=-40) {
    a[i]<-1
  }else if (-40<D_BI[i] && D_BI[i]<=0){
    a[i]<-2
  }else if (0<D_BI[i] && D_BI[i]<=40){
    a[i]<-3
  }else {
    a[i]<-4
  }
}
a<-as.factor(a)
summary(a)
x$D_BI<-a
attach(x)
str(x)
disc<-c(1,2:6,8,14:15,19:27,67:68)
# scale & factor
for (i in 1:ncol(x)) {
  if (i %in% disc) {
    x[,i]<-as.factor(x[,i])
  }else
    x[,i]<-scale(x[,i])
}
attach(x)


# set multiple y
library(nnet)
head(class.ind(x$D_BI))
x_bi<-cbind(class.ind(x$D_BI),x[,-1])
names(x_bi)[1:4]<-c('one','two','three','four')
head(x_bi)
disc<-c(1:4)
# disc<-c(1:4,5:9,11,17:18,22:30,70:71)
for (i in disc) {
  x_bi[,i]<-as.factor(x_bi[,i])
}
attach(as.data.frame(x_bi))


# VGAM
library(VGAM)
m1<-vglm(
  cbind(one,two,three,four) ~ .,
  data=x_bi,
  family=multinomial)
summary(m1)  #too large not work
#cds
bi_cds<-as.data.frame(x_bi[,c(1:4,46:69)])
attach(bi_cds)
model_cds<-list()
for (i in 5:ncol(bi_cds)) {
  m_cds<-vglm(
    cbind(one,two,three,four) ~ bi_cds[,i],
    data=bi_cds,
    family=multinomial)
  model_cds[[i]]<-list(colnames(bi_cds[i]),summary(m_cds))
}
