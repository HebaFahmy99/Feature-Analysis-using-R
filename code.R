
df = read.csv(file = "data.csv") 

#1
head(df,10)
tail(df,10)

#2
orderedDataFrame = order(df$dob)
oldestThree <- head(orderedDataFrame,3)
oldestThree

df[oldestThree[1:3],c("gender","avg_commute","ancestry")]

#3
df[df$children>2,c("gender","daily_internet_use","avg_commute","ancestry","disease")]

#4
table(complete.cases(df))

# missngrows<- rowSums(is.na(df))
# table(missngrows)

#5
summary(Filter(is.numeric, df))
categoricalData <- Filter(is.character, df)
table(categoricalData$dob) 
table(categoricalData$id)
table(c(categoricalData$gender,categoricalData$employment_status, 
        categoricalData$education,categoricalData$marital_status,categoricalData$ancestry, 
        categoricalData$disease))

#6
colWithNA = df[,is.na(df)]
colWithNA 
df <-na.omit(df)
table(complete.cases(df))

#7
eduCategories = unique(df$education)
for (i in 1:length(eduCategories))
{ 
  eduTemp = df[df$education==eduCategories[i],]
  print(eduCategories[i])
  print(mean(eduTemp$daily_internet_use))
}

#another way
avg_ = tapply(df$daily_internet_use, df$education, mean)
avg_


#8
hist(df$children,xlab = "Number of children" , ylab = "children count" ) 

#9
par(mfrow=c(1,2))
plot(df$avg_commute[df$gender=="male"],type='l',col=6)
plot(df$avg_commute[df$gender=="female"],type='l',col=10)

#another answer using denisty plot 
plot(density(df$avg_commute[df$gender=="male"]),xlab="avg_commute", 
     main = "Comparing avg_commute of males and females")
lines(density (df$avg_commute[df$gender=="female"]),col=5)

#10
table(df$gender)
barplot(table(df$gender))


#11
temp <- table(df$gender,df$disease)
barplot(temp,beside = TRUE)

#12
age = function(from, to) {
  from_date = as.POSIXlt(from)
  to_date = as.POSIXlt(to)
  
  age = to_date$year - from_date$year
  
  ifelse(to_date$mon < from_date$mon |
           (to_date$mon == from_date$mon & to_date$mday < from_date$mday),
         age - 1, age)
}

ageDf <- data.frame()

for(i in 1:length(df$dob)){ 
  
  row = df[i,"dob"]
  ageInYears = age(row,Sys.Date())
  
  #append ages as rows in ageDf
  ageDf = rbind(ageDf,ageInYears)
}
table(ageDf)
barplot(table(ageDf),col=4)


#13
diseaseCategories = unique(df$disease)
diseaseDF = data.frame(diseaseCategories)
table(df$disease)
childDf = data.frame()
for (i in 1:length(diseaseCategories)){ 
  disDf = df[df$disease==diseaseCategories[i],]
  output = sum(disDf$children)
  childDf = rbind(childDf,output)
}

disease_children = cbind(diseaseDF,childDf)
barplot(disease_children$X625L,names.arg=disease_children$diseaseCategories)


#14
barplot(table(df$ancestry),col=12)
