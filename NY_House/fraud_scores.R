library(dplyr)
library(ggplot2)
#load data
load("Outlier_Score1.rda")
load("Final_Score.rda")

# outlier score
ggplot(data=score1,aes(x=score1))+geom_histogram(bins = 100,fill="lightblue")+xlim(0,1)+
  labs(x="Outlier_Score", y="Count", title="Distribution of Fraud Score using Heuristic Algorithm")+
  theme(text = element_text(size=16))
summary(score1$score1)
length(score1$score1[score1$score1<=1])/1048575
# autoencoder score
ggplot(data=finalRanking,aes(x=score2))+geom_histogram(bins = 100,fill="lightblue")+xlim(0,0.001)+
  labs(x="Autoencoder_Score", y="Count", title="Distribution of Fraud Score using Autoencoder")+
  theme(text = element_text(size=16))
summary(finalRanking$score2)
length(finalRanking$score2[finalRanking$score2<=0.001])/1048575

# combined score
nrow(filter(finalRanking,ranking.x==ranking.y))/1048575

r=c(1000,999,998,997,996,995,994,993,992,991)
p=rep(0,10)
for (i in 1:10){
  p[i]=nrow(filter(finalRanking,ranking.x==ranking.y & ranking.x==r[i]))/nrow(filter(finalRanking,ranking.x==r[i]))
}
percentage=data.frame(combined_score=r,percentage_overlap=p)

ggplot(data=finalRanking,aes(x=combinedRank))+geom_histogram(bins = 100,fill="lightblue")+
  labs(x="Autoencoder_Score", y="Count", title="Distribution of Fraud Score using Autoencoder")+
  theme(text = element_text(size=16))
