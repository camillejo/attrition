#started this on 6/6/16 to try and fit the GLMM pairwise instead
  #of the entire survey due to convergence issues
#nlme- try this package
#nlmixed and write the likelihood yourself
#compare regular logistic regression first
library(lme4)
setwd("C:/Users/Camille/Documents/D word/sim_1")
n<-200
q<-10
#sigmoid attrition for 10 questions
#att.sigmoid<-c(0,2,4,10,7,2,1,1,0,0)
att.sigmoid<-c(0,2,5,20,29,16,5,1,3,2)

#matrix to hold overall results for each sample
out.all<-NULL
#counter for number who dropped
drop<-0 #eventually should make this a vector to hold for each sample
#created so dropout proportions can be updated
num<-n
#assign a random uniform number to each person for each question
  #represent the probability of dropping out in each question
  #which is completely random
t<-n*q
set.seed(1992)
ran<-runif(t)
assign<-matrix(ran,nrow=n,ncol=q)
#this loop randomly creates a dropout pattern
for(j in 1:n){
  out<-cbind(rep(j,q),c(1:q),rep(0,q))
  a<-att.sigmoid/num
  for(k in 2:q){
    l=k-1
    if(out[l,3]==1) out[k,3]<-1
    if(out[k,3] != 1 && assign[j,k] <= a[k]) out[k,3]<-1
  }
  if(sum(out[,3])>0) num<-num-1
  if(sum(out[,3])>0) drop<-drop+1
  out.all<-rbind(out.all, out)
}
#look at outcome and number of drops
out.all
drop
#assign column names
colnames(out.all)<-c("ID", "Q", "YorN")
#need to change format to work with glmer
out2<-data.frame(out.all)
#pairs isolates the pairs in which we are interested 
pairs<-NULL
for(m in 2:q){
  n2<-n*2
  p<-m-1
  question<-out2$Q
  for(u in 1:n){
    find1<-((u-1)*q)+p
    find2<-((u-1)*q)+m
    if(question[find1]==p) first<-out2[find1,]
    if(question[find2]==m) second<-out2[find2,]
    first
    second
    one.pair<-rbind(first, second)
    pairs<-rbind(pairs, one.pair)
  }
}

pairs$ID<-factor(pairs$ID)
pairs$Q<-factor(pairs$Q)


#qdta<-vector("list",q-1)
for(i in 1:q-1){ 
  #name <- paste("Q", i, i+1 , sep = "")
  name <- paste("Q", i, "_out", sep = "")
  if(i==1) start<-1
  if(i!=1) start<-1+(n*(2*(i-1)))
  end<-n*(2*i)
  dataset<-assign(name, pairs[start:end,])
  #filename<-paste("Q", i, i+1 , ".csv", sep = "")
  filename<-paste("Q", i, "_out.csv", sep = "")
  write.csv(dataset, filename, row.names=FALSE)
}

m1<-glmer(YorN~(Q)+(1|ID), data=Q1_out, family=binomial, contrasts=C, nAGQ=20)
summary(m1)
m2<-glmer(YorN~(Q)+(Q|ID), data=Q1_out, family=binomial, contrasts=C)
summary(m2)
m3<-glmer(YorN~(Q)+(Q+1|ID), data=Q1_out, family=binomial, contrasts=C)
summary(m3)



qdta
q12<-
write.csv(q12,'q12.csv')
q23<-pairs[401:800,]
write.csv(q23,'q23.csv')
q34<-pairs[801:1200,]
write.csv(q34,'q34.csv')
q45<-pairs[1201:1600,]
q56<-pairs[1601:2000,]
q67<-pairs[2001:2400,]
q78<-pairs[2401:2800,]
q89<-pairs[2801:3200,]
q910<-pairs[3201:3600,]

#write.csv(q1, 'q1.csv')
C<-c(1,-1)
is.factor(q1$ID)
m1<-glmer(YorN~(Q)+(1|ID), data=q1, family=binomial, contrasts=C, nAGQ=20)
summary(m1)
m2<-glmer(YorN~(Q)+(Q|ID), data=q1, family=binomial, contrasts=C)
summary(m2)
m3<-glmer(YorN~(Q)+(Q+1|ID), data=q1, family=binomial, contrasts=C)
summary(m3)



m.noint<-glmer(YorN~(Q-1)+(Q|ID), data=q1, family=binomial, contrasts=C)
summary(m.noint)


#m1<-glmer(YorN ~ factor(Q) + (1|ID), data=out2,  family=binomial, 
 #         control = glmerControl(optimizer = "bobyqa"))
#summary(m1)