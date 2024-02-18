g<-read.csv("travelled abroad_csv.csv")
traveeled<-nrow(subset(g,Travelledabroad=="Y"))
percentage<-traveeled*100/nrow(g)
cat("Percentage of pepople travelled abroad ",percentage)

p=percentage/100
cat("\nProbablity of success ",p)

d<-dbinom(0:10,10,p)
cat("\nProbability that in a randomly chosen sample of 10 persons, no one has traveled abroad:", d, "\n")

plot(0:10,d,type="l")

d2<-sum(dbinom(59:100,100,p))
d3<-dbinom(0:100,100,p)
plot(0:100,d3,type="l")

cat("Probablity of binomial distribution with n=100  ",d2) 

m<-100*p
cat("\nMean is ",m)
sd<-sqrt(100*p*(1-p))
cat("\nStandard deviation is ",sd)
norm<-pnorm(59,m,sd,lower.tail = F)
cat("\nProbabliy of Normal distribution with N=100 ",norm)