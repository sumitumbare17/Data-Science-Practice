f <- read.csv('knn1_csv.csv')
dist<-sqrt(((3-f$x)**2)+((2-f$y)**2))
f<-cbind(f,dist)
f<-f[order(f$dist),]
cat("\n class of p for NN is ",f[1,4])


df_5<-f[1:5,]
l1<-sum(df_5$class==1)
l2<-sum(df_5$class==2)
l3<-sum(df_5$class==3)
if(l1>l2&l1>l3)
{
  cat("\n Class of P for KNN is 1")
}
if(l2>l1&l2>l3)
{
  cat("\n Class of P for KNN is 2")
}
if(l3>l2&l3>l1)
{
  cat("\n Class of P for KNN is 3")
}

df_7<-f[1:7,]
s1<-sum(df_7$class==1)
s2<-sum(df_7$class==2)
s3<-sum(df_7$class==3)
if(s1>s2&s1>s3)
{
  cat("\n Class of P for KNN is 1")
}
if(s2>s1&s2>s3)
{
  cat("\n Class of P for KNN is 2")
}
if(s3>s2&s3>s1)
{
  cat("\n Class of P for KNN is 3")
}




rnn<-f[c(f$dist<1.45),]
r1<-sum(rnn$class==1)
r2<-sum(rnn$class==2)
r3<-sum(rnn$class==3)
if(r1>r2&r1>r3)
{
  cat("\n Class of P for KNN is 1")
}
if(r2>r1&r2>r3)
{
  cat("\n Class of P for KNN is 2")
}
if(r3>r2&r3>s1)
{
  cat("\n Class of P for KNN is 3")
}


dk=max(df_5$dist)
d1= min(df_5$dist)
dj=1:5

wj<-(dk-dj)/(dk-d1)

df_5<-cbind(df_5,wj)

lw1<-sum(df_5$wj,(df_5$class==1))
lw2<-sum(df_5$wj,(df_5$class==2))
lw3<-sum(df_5$wj,(df_5$class==3))

if(lw1>lw2&lw1>lw3)
{
  cat("\n Class of P for MKNN is 1")
}
if(lw2>lw1&lw2>lw3)
{
  cat("\n Class of P for MKNN is 2")
}
if(lw3>lw2&lw3>lw1)
{
  cat("\n Class of P for MKNN is 3")
}

