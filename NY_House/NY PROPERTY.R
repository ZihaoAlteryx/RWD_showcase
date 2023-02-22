library(readr)
MyData <- read_csv(file="NY property 1 million.csv")


library(ggplot2)
library(dplyr)

MyData$BLOCK=as.factor(MyData$BLOCK)
nlevels(MyData$BLOCK)


MyData%>%
  group_by(BLOCK)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(BLOCK,-Count),Count))+geom_bar(stat="identity")+
  xlab("Block")
  
MyData%>%
  group_by(BLOCK)%>%
  summarise(fre=n()/1048575)%>%
  arrange(-fre)%>%
  slice(1:10)


MyData$LOT=as.factor(MyData$LOT)
nlevels(MyData$LOT)

MyData%>%
  group_by(LOT)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(LOT,-Count),Count))+geom_bar(stat="identity")+
  xlab("LOT")

MyData%>%
  group_by(LOT)%>%
  summarise(fre=n()/1048575)%>%
  arrange(-fre)%>%
  slice(1:10)




MyData$EASEMENT=as.factor(MyData$EASEMENT)

nlevels(MyData$EASEMENT)
MyData%>%
  group_by(EASEMENT)%>%
  summarise(Count=log(n()))%>%
  arrange(-Count)%>%
  ggplot(aes(reorder(EASEMENT,-Count),Count))+geom_bar(stat="identity")+
  xlab("EASEMENT")



MyData%>%
  group_by(EASEMENT)%>%
  summarise(fre=n()/1048575*100)%>%
  arrange(-fre)%>%
  slice(1:10)


MyData$OWNER=as.factor(MyData$OWNER)

nlevels(MyData$OWNER)
MyData%>%
  group_by(OWNER)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(OWNER,-Count),Count,label=Count))+geom_bar(stat="identity")+
  xlab("OWNER")+coord_flip()+
  geom_label()



MyData%>%
  group_by(EASEMENT)%>%
  summarise(fre=n()/1048575*100)%>%
  arrange(-fre)%>%
  slice(1:10)


MyData$BLDGCL=as.factor(MyData$BLDGCL)

nlevels(MyData$BLDGCL)


MyData%>%
  group_by(BLDGCL)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(BLDGCL,-Count),Count,label=Count))+geom_bar(stat="identity")+
  xlab("BLDGCL")



MyData%>%
  group_by(BLDGCL)%>%
  summarise(fre=n()/1048575*100)%>%
  arrange(-fre)%>%
  slice(1:10)









MyData$TAXCLASS=as.factor(MyData$TAXCLASS)

nlevels(MyData$TAXCLASS)


MyData%>%
  group_by(TAXCLASS)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(TAXCLASS,-Count),Count,label=Count))+geom_bar(stat="identity")+
  xlab("BLDGCL")



MyData%>%
  group_by(TAXCLASS)%>%
  summarise(fre=n()/1048575)%>%
  arrange(-fre)%>%
  slice(1:10)


summary(MyData$LTFRONT)


MyData%>%
  group_by(BLDGCL)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(BLDGCL,-Count),Count,label=Count))+geom_bar(stat="identity")+
  xlab("BLDGCL")



MyData%>%
  group_by(BLDGCL)%>%
  summarise(fre=n()/1048575*100)%>%
  arrange(-fre)%>%
  slice(1:10)





MyData$LTDEPTH=as.numeric(MyData$LTDEPTH)
ggplot(data=MyData,aes(x=LTDEPTH))+geom_histogram(bins = 20)+xlim(0,150)

hist(MyData$LTFRONT)


str(MyData$STORIES)

ggplot(data=MyData,x=STORIES,y=)+geom_histogram(bins = 100)+xlim(0,120)
MyData$STORIES=as.factor(MyData$STORIES)
MyData%>%
  group_by(STORIES)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:11)%>%
  ggplot(aes(reorder(STORIES,-Count),Count,label=Count))+geom_bar(stat="identity")+
  xlab("STORIES")

ggplot(data=MyData,aes(x=EXLAND))+geom_histogram(bins = 300)+xlim(0,1000)


str(MyData$EXLAND)


MyData%>%
  group_by(EXLAND)%>%
  summarise(Count=n())%>%
  arrange(-Count)%>%
  slice(1:10)%>%
  ggplot(aes(reorder(EXLAND,-Count),Count,label=Count))+geom_bar(stat="identity")+
  xlab("EXLAND")
