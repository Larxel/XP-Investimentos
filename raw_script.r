library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(caTools)
library(ranger)
library(pROC)
library(caret)
setwd("~/Documents/XP Investimentos/real")
set.seed(451)

# [] Read Data
raw.data  = fread("_transacao_train.csv")
test.data = fread("_transacao_test.csv")
additional.data = fread("_pessoas.csv")

#===================================================================================
# Data Preprocessing
#===================================================================================
# [] Join training data with pessoas df
raw.data  = merge(raw.data, additional.data,  by="ID")
test.data = merge(test.data, additional.data, by="ID")


glimpse(raw.data)
glimpse(test.data)


# [] Isolate labels
label = raw.data$retorno
raw.data$retorno = NULL

# [] Bind all data
#full.data = rbind(raw.data, test.data)
full.data  = raw.data
glimpse(full.data)

# [] Check NAs
colSums(is.na(full.data))

summary(full.data)

# [] Create time-based features
full.data$data.x = as.Date(full.data$data.x, format='%Y-%m-%d')
full.data$data.y = as.Date(full.data$data.y, format='%Y-%m-%d')

full.data$weekday_x = wday(full.data$data.x)
full.data$weekday_y = wday(full.data$data.y)

full.data$numday_x = day(full.data$data.x)
full.data$numday_y = day(full.data$data.y)

full.data$month_x  = month(full.data$data.x)
full.data$month_y  = month(full.data$data.y)

full.data$year_x   = year(full.data$data.x) - 2018
full.data$year_y   = year(full.data$data.y) - 2018

full.data$time_dif = as.numeric(full.data$data.x) - as.numeric(full.data$data.y)

# [] Inspect Cardinality
cardinality = function(x){
  x = x %>% as.data.frame()
  for (j in 1:ncol(x)){
    print(paste0(colnames(x)[j],": ",length(unique(x[,j])), " dtype:",class(x[,j])))
  }
}

cardinality(full.data)

# [] Convert var1 to binary
full.data$var1 = ifelse(full.data$var1 == "tipo2",1,0)

# [] Create count list and past results list
full.data$retorno = label
count_list  = full.data  %>% group_by(ID) %>% summarize(count = n())
past_result = full.data  %>% group_by(ID) %>% summarize(past_result = mean(retorno))


# [] Add these variables in the main dataset
full.data = merge(full.data, count_list,  by="ID")
full.data = merge(full.data, past_result, by="ID")


#===================================================================================
# Exploratory Data Analysis
#===================================================================================
glimpse(full.data)

quickplot_density = function(variable){
  ggplot(full.data, aes(x=variable, fill=as.factor(retorno)))+
    geom_density(color="white",alpha=0.8)+
    theme_hc(bgcolor = 'darkunica')+
    ggtitle(paste0(substr(deparse(substitute(variable)),11,99)," vs Retorno"))
}

quickplot_bar = function(variable){
  ggplot(full.data, aes(x=variable, y=1, fill=as.factor(retorno)))+
    geom_bar(stat="identity",position='fill',alpha=0.8)+
    theme_hc(bgcolor = 'darkunica')+
    ggtitle(paste0(substr(deparse(substitute(variable)),11,99)," vs Retorno"))
}




quickplot_boxplot = function(variable){
  ggplot(full.data, aes(x=variable, y=1, fill=as.factor(retorno)))+
    geom_boxplot(color="white",alpha=0.8)+
    theme_hc(bgcolor = 'darkunica')+
    ggtitle(paste0(substr(deparse(substitute(variable)),11,99)," vs Retorno"))
}

quickplot_violin = function(variable){
ggplot(full.data, aes(x=variable,y=var38))+
  geom_violin(aes(fill=as.factor(retorno)), position="identity",alpha=0.7,color="grey65",width=1.5)+
  theme_hc(bgcolor="darkunica")+
  scale_fill_manual(values=c("cyan3","maroon3"))+
  ggtitle(paste0(substr(deparse(substitute(variable)),11,99)," vs Retorno"))
}

quickplot_bar(full.data$categoria)
#quickplot_bar(full.data$var2)
quickplot_bar(full.data$var3)
quickplot_bar(full.data$var4)
quickplot_bar(full.data$var5)


quickplot_bar(full.data$var6)
quickplot_bar(full.data$var7)
quickplot_bar(full.data$var8)
quickplot_bar(full.data$var9)
quickplot_bar(full.data$var10.x)
quickplot_bar(full.data$grupo)





quickplot_violin(full.data$categoria)

# Var2
round(prop.table(table(full.data$var2, full.data$retorno)),2)
round(prop.table(table(full.data$var3, full.data$retorno)),2)




# Var38
quickplot_density(full.data$var38)
quickplot_density(full.data$past_result)
quickplot_density(full.data$time_dif)
quickplot_density(full.data$month_x)
quickplot_density(full.data$month_y)
quickplot_density(full.data$numday_x)
quickplot_density(full.data$numday_y)
quickplot_density(full.data$var1)




# Rejected variables
quickplot_density(full.data$year_y)
quickplot_density(full.data$year_x)
quickplot_density(full.data$weekday_x)
quickplot_density(full.data$weekday_y)
quickplot_density(full.data$var10.y)

quickplot_density(full.data$var11)
quickplot_density(full.data$var12)
quickplot_density(full.data$var13)
quickplot_density(full.data$var14)
quickplot_density(full.data$var15)
quickplot_density(full.data$var16)
quickplot_density(full.data$var17)
quickplot_density(full.data$var18)
quickplot_density(full.data$var19)
quickplot_density(full.data$var20)
quickplot_density(full.data$var21)
quickplot_density(full.data$var22)
quickplot_density(full.data$var23)
quickplot_density(full.data$var24)
quickplot_density(full.data$var25)
quickplot_density(full.data$var26)
quickplot_density(full.data$var27)
quickplot_density(full.data$var28)
quickplot_density(full.data$var29)
quickplot_density(full.data$var30)
quickplot_density(full.data$var31)
quickplot_density(full.data$var32)
quickplot_density(full.data$var33)
quickplot_density(full.data$var34)
quickplot_density(full.data$var35)
quickplot_density(full.data$var36)
quickplot_density(full.data$var37)


#===================================================================================
# Being a Dick Approach
#===================================================================================
past_result$pred = round(past_result$past_result)
probe = fread("holdout_set2.csv") %>% as.data.frame()

probe = merge(probe, past_result, by="ID")
probe$retorno = ifelse(probe$retorno == "Sim",1,0)
glmnet::auc(probe$retorno, probe$pred)

#===================================================================================
# Baseline model
#===================================================================================
selected_features = c('ID','retorno','var38','time_dif','month_x','month_y','numday_x','numday_y','var1','categoria') #'grupo'

#raw.data = fread("all_train_data.csv") %>% as.data.frame()
#raw.data$var1 = ifelse(raw.data$var1 == "tipo2",1,0)
#raw.data$retorno = raw.data$retorno
#training_data = raw.data[,selected_features]

full.data = full.data %>% as.data.frame()
training_data = full.data[,selected_features]


splitr = sample.split(training_data$retorno, SplitRatio = 0.7)
train  = subset(training_data, splitr == TRUE)
probe  = subset(training_data, splitr == FALSE)

# [] Set CV Parameters and Grid
train.ctrl = trainControl(method="cv",number=4,classProbs=T,summaryFunction = twoClassSummary)
rf.grid    = expand.grid(mtry=c(3,4,5),
                         splitrule='gini',
                         min.node.size=1)

# Reencode targets (so caret won't crash)
train$retorno  = ifelse(train$retorno == 1,"Sim","Nao")
probe$retorno  = ifelse(probe$retorno == 1,"Sim","Nao")
truthfull.data = train[!duplicated(train$ID),]
truthfull.data$ID = NULL

# Train baseline model (CV + Holdout validated)
rf.model = train(as.factor(retorno) ~ ., data=truthfull.data, method="ranger", num.trees=500,
                 trControl = train.ctrl, metric='ROC', tuneGrid = rf.grid)



# Export truthfull data - data leak free
fwrite(truthfull.data, "truthfull_set.csv")
fwrite(probe, "holdout_set2.csv")

rf.pred = predict(rf.model, probe,type = 'prob')
table(probe$retorno, rf.pred$Sim >= 0.5)

probe$retorno  = ifelse(probe$retorno == "Sim",1,0)

glmnet::auc(probe$retorno, rf.pred$Sim)

# Export to disk for safety
fwrite(train, "training_set.csv")
fwrite(probe, "holdout_set.csv")


# Baseline model
rf.model = ranger(retorno ~ ., data=train, importance = 'impurity', num.trees=100)




# Prepare final data for prediction
test.data$data.x = as.Date(test.data$data.x, format='%Y-%m-%d')
test.data$data.y = as.Date(test.data$data.y, format='%Y-%m-%d')

test.data$weekday_x = wday(test.data$data.x)
test.data$weekday_y = wday(test.data$data.y)

test.data$numday_x = day(test.data$data.x)
test.data$numday_y = day(test.data$data.y)

test.data$month_x  = month(test.data$data.x)
test.data$month_y  = month(test.data$data.y)

test.data$year_x   = year(test.data$data.x) - 2018
test.data$year_y   = year(test.data$data.y) - 2018

test.data$time_dif = as.numeric(test.data$data.x) - as.numeric(test.data$data.y)
test.data$var1 = ifelse(test.data$var1 == "tipo2",1,0)

# Export to disk
test.data = test.data %>% as.data.frame()
selected_features = c('ID','var38','time_dif','month_x','month_y','numday_x','numday_y','var1','categoria')
testing.data = test.data[,selected_features]
fwrite(testing.data,"testing_set.csv")

# Generate baseline predictions
rf.pred = predict(rf.model, test.data)
final.output = data.frame(ID = test.data$ID,retorno = rf.pred$predictions)
fwrite(final.output, "final_pred.csv")
























