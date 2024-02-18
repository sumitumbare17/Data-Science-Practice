#Pollutant Dataset----
cat("\nPollutant Dataset :")
f<-read.csv("pollutant_csv.csv")
m<-mean(f$Temp[f$Month==6])
cat("\nMean Of Temprature When Month Is 6 : ",m)
n<-nrow(f)
cat("\nNo of Observation in Dataset :",n)

cat("\nLast two rows of the data \n")
last<-tail(f,2)
print(last)

o<-f$Ozone[47]
cat("\n47th Value of Ozone is :",o)

ms<- sum(is.na(f$Ozone))
cat("\nNumber values are missing in Ozone column : ",ms)

m1<-mean(f$Ozone,na.rm = T)
cat("\nThe mean of Ozone column excluding missing values : ",m1)

m2<-mean(f$Solar.R[f$Ozone>31  & f$Temp>90],na.rm=T)
cat("\nThe mean of Solar.R in subset when where Ozone> 31 and Temp > 90 : ",m1)

mx<-max(f$Ozone[f$Month==5],na.rm=t)
cat("\nThe maximum ozone value in the month of May ",mx)

#Hair Eye color Data set ----
cat("\n\nHair Eye color Data set OUTPUT :")

g<-read.csv("hair_eye_color_csv.csv")
nBrown<-sum(g$Eye.Color=="Brown")
cat("\nNumberof people have brown eye color =",nBrown)

nBlonde<-sum(g$Hair.Color=="Blonde")
cat("\nNumberof people have Blonde Hair color =",nBlonde)

nBB<-sum(g$Hair.Color=="Brown" & g$Eye.Color=="Black")
cat("\nNumberof Brown haired people have Black eyes=",nBB)

green<-sum(g$Eye.Color=="Green")
allColors<-length(g$Eye.Color)
percent<-(green/allColors)*100
cat("\nThe percentage of people with Green eyes",percent)
red<-nrow(subset(g,Hair.Color=="Red" & Eye.Color=="Blue"))
percentage<-red*100/nrow(g)
cat("\nThe percentage of people have red hair and Blue eyes", percentage)


#Germination Dataset ----
cat("\n\nGermination Dataset OUTPUT :")
h=read.csv("germination_csv.csv")
a2<-subset(h,Box=="Uncovered" & water_amt==4)
avg<-mean(a2$germinated)
cat("\nAverage no of seeds germinated for uncovered boxes & watering equal to 4 :",avg)

germinated=h$germinated
covered<-subset(h,Box=="Covered",select=germinated)
med<-median(covered$germinated)
cat("\nThe median value of the germinated seeds for the data covered boxes: ",med)


par(mfrow=c(1,2))
Covered<-subset(h,Box=="Covered")
Uncovered<-subset(h,Box=="Uncovered")
plot(Covered$water_amt,Covered$germinated,col="Green",pch=16,xlab="Water amount",ylab="Seeds germinated",main="For Covered Box")
plot(Uncovered$water_amt,Uncovered$germinated,col="Green",pch=16,xlab="Water amount",ylab="Seeds germinated",main="For Uncovered Box")

par(mfrow=c(1,2))
hist(Covered$germinated,label=T)
hist(Uncovered$germinated,label=T)

#iris Dataset ----
p<-ggplot(iris,aes(Sepal.Length,Species,fill=Species))+geom_boxplot(outlier.size = 4,outlier.colour = "red",outlier.shape =4)+theme(legend.position = "none")+labs(title="BOXPLOT")+coord_flip()
print(p)
#dslabs Dataset----

library(dslabs)
library(ggplot2)
g<-ggplot(murders, aes(x = population/10^6, y = total,label=abb))+geom_point(aes(color = region))+scale_x_log10()+scale_y_log10()+geom_text(nudge_x = 0.025)+  labs(x = "Population in millions (log scale)", y = "Total murders (log scale)",title = "US Gun Murders in 2010")
print(g)
