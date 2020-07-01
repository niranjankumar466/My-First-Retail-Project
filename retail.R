####....... Set the working directory, where is my data. 

setwd("E:/Laptop/E/Data Science/R/Project detail/Project_data/2_retail")
getwd


####_________ for read the csv file , where the train and test data are saved. 

store_train=read.csv("store_train.csv",stringsAsFactors = FALSE)
store_test=read.csv("store_test.csv",stringsAsFactors = F)


####....... create a new variable store, 
#####.......which is store NA values. 
#####.......We creating this variables,beacuse we combine the our the test and train data for data preparation.

store_test$store=NA


####.....
store_test$data='test'
store_train$data='train'
store_all=rbind(store_train,store_test)


####..........
sum(is.na(store_all))
lapply(store_all, function (x) sum(is.na(x)))


###...........
glimpse(store_all)

#####

sort(table(store_all$country))
unique(store_all$country)

mean(store_all$country, na.rm=T)
median(store_all$country, na.rm=T)
mode('store_all$country')

boxplot(store_all$country)
store_all$country[is.na(store_all$country)]=median(store_all$country, na.rm=T)

lapply(store_all, function (x) sum(is.na (x)))



####...........
glimpse(store_all)
unique(store_all$population)
mean(store_all$population, na.rm=T)
median(store_all$population, na.rm=T)

boxplot(store_all$population)
store_all$population[is.na(store_all$population)]=median(store_all$population,na.rm=T)

######....
lapply(store_all, function (x) sum(is.na(x)))


####.....
library(dplyr)
glimpse(store_all)

#####.............
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  sort(t)
  categories=names(t)[-1]
  
  for (cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

d=CreateDummies(d,"Suburb",150)
d=CreateDummies(d,"SellerG",300)
d=CreateDummies(d,"CouncilArea",500)

######......
sort(table(store_all$countyname))

t=sort(table(store_all[,'countyname']))
t

###...
t=t[t>20]
t
t=t[t>70]
t

store_all=CreateDummies(store_all,'countyname',30)
###....
glimpse(store_all)
sort(table(store_all$storecode))
  
t=table(store_all[,'storecode'])

t=t[t>50]
t

store_all=CreateDummies(store_all,'storecode',50)

####.......
glimpse(store_all)
t=sort(table(store_all[,'Areaname']))
t
t=t[t>50]
t

store_all=CreateDummies(store_all,'Areaname',50)

####.......
glimpse(store_all)
t=sort(table(store_all[,'state_alpha']))
t
t=t[t>50]
t

t=t[t>150]
t

t=t[t>200]
t

store_all=CreateDummies(store_all,'state_alpha',200)

#####...............
glimpse(store_all)
t=sort(table(store_all[,"countytownname"]))
t
t=t[t>20]
t

store_all$countytownname=NULL
store_all$Areaname=NULL
######......

glimpse(store_all)

t=sort(table(store_all[,'store_Type']))
t
d=store_all

d=d %>%
  mutate(store_GS=as.numeric(store_Type=='Grocery Store'),
         store_ST1=as.numeric(store_Type=='Supermarket Type1'),
         store_ST2=as.numeric(store_Type=='Supermarket Type2'),
         store_ST3=as.numeric(store_Type=='Supermarket Type3')
  ) %>%
  select(-store_Type)


store_all=store_all %>%
  mutate(store_GS=as.numeric(store_Type=='Grocery Store'),
         store_ST1=as.numeric(store_Type=='Supermarket Type1'),
         store_ST2=as.numeric(store_Type=='Supermarket Type2'),
         store_ST3=as.numeric(store_Type=='Supermarket Type3')
  ) %>%
  select(-store_Type)


#####........
glimpse(store_all)


#####.......

store_train=store_all %>% filter(data=='train') %>% select(-data)
store_test=store_all %>% filter(data=='test') %>% select(-data, -store)




#####............
set.seed(2)
s=sample(1:nrow(store_train),0.7*nrow(store_train))
store_train1=store_train[s,]
store_train2=store_train[-s,]

library(car)
fit=lm(store~.,data=store_train1)
sort(vif(fit))
summary(fit)

store_all$store_ST3=NULL
store_test$store_ST3=NULL
store_train$store_ST3=NULL
store_train1$store_ST3=NULL
store_train2$store_ST3=NULL
###....


####..................
glimpse(store_all)
fit=lm(store~ . -Id -sales0 -sales2 -sales3 -sales1,data=store_train1)
sort(vif(fit) ,decreasing = TRUE)
summary(fit)
formula(fit)

########.............

fit=lm(store ~  sales4  + 
         State + CouSub + population + countyname_BerkshireCounty + 
         countyname_CoosCounty + countyname_CumberlandCounty + 
          countyname_HillsboroughCounty + 
          countyname_PenobscotCounty + countyname_RockinghamCounty + 
          countyname_YorkCounty + 
          state_alpha_ME + state_alpha_NH + 
         state_alpha_TX + state_alpha_VT  
         , data=store_train1)


summary(fit)

#####... using DECISION Tree


library(tree)
store.tree=tree(store~.,data=store_train1)

store.tree

val.score=predict(store.tree,newdata=store_train2,type='vector')

pROC::roc(store_train2$store,val.score)$auc

#### AUC = 0.7552
store.tree.final=tree(store~.,data=store_train)

store.tree.final
test.score=predict(store.tree.final,newdata=store_test,type='vector')

test.score

write.csv(test.score,"DT_retail_submission_part2.csv",row.names=F)


###### USING RANDOM FOREST
library(randomForest)

fit_store= randomForest(as.factor(store)~.,data=store_train1)
fit_store

response=predict(fit_store, newdata = store_train2,type = "prob")[,2]

### Checking auc score ------------

library(pROC)

auc(roc(store_train2$store,response))

####..............AUC=0.8149  

## prediction on test data using the entire training data -----------

library(randomForest)

fit_store= randomForest(as.factor(store)~.,data=store_train)
fit_store

response=predict(fit_store, newdata = store_test,type = "prob")[,2]

write.csv(response,"RF_Niranjan_Kumar_Retail_P2_part2.csv",row.names = F)


######
table=sum(store_train$sto)

x=sum(storeall)
apply()

sum()

#calculate sum of all sales.
outliers= sum(store_all$sales0,store_all$sales1,store_all$sales2,store_all$sales3,store_all$sales4,na.rm=F)

#find the outliers.

store_all$outliers=NULL

d=list(store_all$sales0)
d

order(d,na.last=T, decreasing=F)


####.......... we are check the unique value in train data for state_alpha variable and sort the variable.

sum(unique(table(store_train$state_alpha)))

sum(sort(table(store_train$state_alpha)))
