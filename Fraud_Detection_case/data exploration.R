setwd("~/Documents/fraud/project 3")
library(gdata)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
#load data
options(scipen=999)
dat<-read.xls("card_transactions.xlsx")
str(dat)

#change variables to appropriate type
dat$Date<-as.character(dat$Date)
#dat$Merchantnum<-as.character(dat$Merchantnum)
dat$Merch.Description<-as.character(dat$Merch.Description)
dat$Amount<-substr(as.character(dat$Amount),2,11)
dat$Amount<-as.numeric(gsub(",", "",dat$Amount))
dat$Fraud<-as.factor(dat$Fraud)
str(dat)

# % populated & # unique
percPopulated<-numeric()
numUnique<-numeric()
for (i in 1:ncol(dat)){
  var<-dat[,i]
  numerator<-ifelse(is.null(length(var[!is.na(var) & var!=""])),0,length(var[!is.na(var) & var!=""]))
  perc<-numerator/length(var)
  percPopulated<-c(percPopulated,perc)
  numUnique<-c(numUnique,length(unique(var)))
}

overallSumm<-data.frame(variable=colnames(dat),percPopulated,numUnique)
#write.csv(overallSumm,"overallSumm.csv")

# cardnum
dat%>%
  group_by(Cardnum)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(Cardnum,Count),Count,label=Count))+geom_bar(stat="identity")+
  coord_flip()+
  xlab("Cardnum")+
  ggtitle("Top 10 Frequent Card Number")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_label()

summary(dat%>%
  group_by(Cardnum)%>%
  summarise(Count=n()))

dat%>%
  group_by(Cardnum)%>%
  summarise(Count=n())%>%
  ggplot(aes(x=Count))+geom_histogram(bins = 30)+xlim(0,800)+
  labs(x="Count of Cardnum", y="Frequency", title="Histogram for Count of Cardnum")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

# date
dat$Date<-as.Date(dat$Date)
dat%>%
  group_by(Date)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(Date,Count),Count,label=Count))+geom_bar(stat="identity")+
  coord_flip()+
  xlab("Date")+
  ggtitle("Top 10 Frequent Date")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_label()

dat%>%
  group_by(Date)%>%
  summarise(Count=n())%>%
  ggplot(aes(Date,Count))+geom_line()+
  ggtitle("Daily Transactions")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

dat%>%
  group_by(week=week(Date))%>%
  summarise(Count=n())%>%
  ggplot(aes(week,Count))+geom_line()+
  ggtitle("Weekly Transactions")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

dat%>%
  group_by(month=month(Date))%>%
  summarise(Count=n())%>%
  ggplot(aes(as.factor(month),Count))+geom_bar(stat="identity")+
  xlab("Month")+
  ggtitle("Payment Count by Month")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
# merchantnum
dat[dat$Merchantnum!="",]%>%
    group_by(Merchantnum)%>%
    summarise(Count=n())%>%
    arrange(-Count)%>%
    slice(1:10)%>%
    ggplot(aes(reorder(Merchantnum,Count),Count,label=Count))+geom_bar(stat="identity")+
    coord_flip()+
    xlab("Merchantnum")+
    ggtitle("Top 10 Frequent Merchant Number")+
    theme(plot.title = element_text(size = 24, hjust = 0.5),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_label()

summary(dat[dat$Merchantnum!="",]%>%
          group_by(Merchantnum)%>%
          summarise(Count=n()))

# Merch.Description
dat%>%
  group_by(Merch.Description)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(Merch.Description,Count),Count,label=Count))+geom_bar(stat="identity")+
  coord_flip()+
  xlab("Merch.Description")+
  ggtitle("Top 10 Frequent Merchant Description")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_label()

summary(dat%>%
          group_by(Merch.Description)%>%
          summarise(Count=n()))

# Merchant.State
dat[dat$Merchant.State!="",]%>%
  group_by(Merchant.State)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(Merchant.State,Count),Count,label=Count))+geom_bar(stat="identity")+
  coord_flip()+
  xlab("Merchant.State")+
  ggtitle("Top 10 Frequent Merchant State")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_label()

summary(dat[dat$Merchant.State!="",]%>%
          group_by(Merchant.State)%>%
          summarise(Count=n()))

# Merchant.Zip
dat[!is.na(dat$Merchant.Zip),]%>%
  group_by(Merchant.Zip)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(Merchant.Zip,Count),Count,label=Count))+geom_bar(stat="identity")+
  coord_flip()+
  xlab("Merchant.Zip")+
  ggtitle("Top 10 Frequent Merchant Zip")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_label()

summary(dat[!is.na(dat$Merchant.Zip),]%>%
          group_by(Merchant.Zip)%>%
          summarise(Count=n()))

# Transtype
dat%>%
  group_by(Transtype)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  ggplot(aes(reorder(Transtype,Count),Count,label=Count))+geom_bar(stat="identity")+
  coord_flip()+
  xlab("Transtype")+
  ggtitle("Count of Each Transaction Type")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_label()

# Amount
summary(dat$Amount)
nrow(dat[dat$Amount<=2000,])/nrow(dat)
dat%>%
  ggplot(aes(x=Amount))+geom_histogram(bins = 30)+xlim(0,2000)+
  labs(x="Amount", y="Count", title="Histogram for Amount")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

summary(dat$Amount[dat$Fraud==1])
nrow(dat[dat$Amount<=6000 & dat$Fraud==1,])/nrow(dat[dat$Fraud==1,])
summary(dat$Amount[dat$Fraud==0])
nrow(dat[dat$Amount<=6000 & dat$Fraud==0,])/nrow(dat[dat$Fraud==0,])

ggplot(dat,aes(x=Amount, fill=Fraud)) + 
  geom_histogram(aes(y=..density..),
                 alpha=0.5,position='identity')+
  xlim(0,6000)+
  labs(x="Amount", title="Histogram for Amount")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Fraud
# Transtype
dat%>%
  group_by(Fraud)%>%
  summarise(Count=n())%>%
  ggplot(aes(reorder(Fraud,Count),Count,label=Count))+geom_bar(stat="identity")+
  coord_flip()+
  xlab("Fraud")+
  ggtitle("Count of Fraud/Non-Fraud")+
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_label()

# missing value
var<-dat[,4]
length(var[var==""])
length(var[var=="0"])
length(var[is.na(var)])
var<-dat[,6]
length(var[var==""])
length(var[is.na(var)])
var<-dat[,7]
length(var[var==""])
length(var[is.na(var)])
