cat("\n \n \n case 1 ")
g<-read.csv("Hypothesis_csv1.csv")
m<-mean(g$Life_Hrs)
cat("\n Mean of Life Hrs is ",m)
s<-sd(g$Life_Hrs)
se<-s/sqrt(50)
cat("\n standard error value is ",se)
p<-pnorm( m,10000,se,lower.tail = T)
cat("\n Probability of Normal distribution is ",p)
if(p<0.05)
{
  cat("\n claim can be rejected for 0.05")
}else{
  cat("\n claim cannot be rejected for 0.05")
}
if(p<0.01)
{
  cat("\n claim can be rejected for 0.01")
  
}else
{
  cat("\n claim cannot be rejected for 0.01")
  
}
cat("\n \n \n case 2 ")
se1<-17/sqrt(35)
cat("\n standard error value is ",se1)
p2<-pnorm(134,130,se1,lower.tail = F)*2
cat("\n Probability of Normal distribution is ",p2)
if(p2<0.05)
{
  cat("\n Null hypothesis can be rejected for 0.05")
}else{
  cat("\n Null hypothesis cannot be rejected for 0.05")
}
if(p2<0.01)
{
  cat("\n Null hypothesis can be rejected for 0.01")
  
}else
{
  cat("\n Null hypothesis cannot be rejected for 0.01")
  
}