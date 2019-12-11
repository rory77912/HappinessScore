#setwd("/Users/changhungchao/Desktop/R/businessanalysis/final_project/")
#################   import package    ###################
install.packages("magrittr")        
install.packages("dplyr")   
install.packages('Metrics')
install.packages("party")
install.packages("partykit")
install.packages("maps")
install.packages(c('rworldmap', 'RColorBrewer')) 
install.packages("countrycode")
install.packages("d3heatmap")
#################   import package    ###################

#########################################################

####################  use/check package  ######################
library("caTools")
library("rpart")
library("gbm")
library("rpart.plot")
library("d3heatmap")
library("shiny")
library("ROCR")
library("pROC")
library("ggplot2")
library("Metrics")
library("tidyverse")
library("corrplot")
library("party")
library("partykit")
library("rpart.plot")
library(randomForest)
library("ROCR")
library("pROC")
library("ggplot2")
library("maps")
library("rworldmap")
library("RColorBrewer")
library("countrycode")
library("dplyr")
library("magrittr")
####################  use package  ######################
#########################################################
##################    combine data   ####################
#import data
human_development<-
  read.csv("human_development.csv")

happiness_2015<-
  read.csv("2015.csv")

economic_freedom_index2019_data<-
  read.csv("economic_freedom_index2019_data.csv")

#get the happiness score and rank from the happiness_2015.csv
happiness<-happiness_2015[,c(1,4)]
#這裏會有Country,Happiness.Score的變數


#1:without the data of economic freedom index 
inner = happiness %>% inner_join(human_development)
ncol(inner) #check the num of columns 
str(inner) #to get the info of data

#2:with the data of economic freedom index 

inner=merge(x=inner,
            y=economic_freedom_index2019_data, 
            by.x = "Country", 
            by.y = "Country.Name")
ncol(inner) #check the num of columns 
str(inner) #to get the info of data

##################    combine data   ####################
#########################################################
####################   modify data   ####################

#to store the data of country names
store.countryname = inner$Country

#to store rank value to visualization
other.name = c("World.Rank","HDI.Rank")
visual = inner[other.name]

#delete some variables 
inner = inner[,-which(names(inner)%in%c("Country","WEBNAME","Country.y","CountryID", "Region", "Region.Rank", "World.Rank", "HDI.Rank", "Happiness.Rank" ))]
names(inner)
str(inner)

#change the type of some variables 
for(x in c(6:ncol(inner))){
  inner[,x] = as.numeric(inner[,x])
}
str(inner)
names(inner)

#####################  modify data   ####################
##################    visualization   ###################
#剛剛的country name轉換成country code 
x = c(countrycode(store.countryname, 'country.name', 'iso3c'))

#show country 
#list of numerical variables
list = c()
for (i in colnames(inner)){
  if(typeof(inner[i])=="int"||typeof(inner[,i])=="double"){
    list = c(list,i)
    print(list)
    print(i)
  }
}
list1 = c(other.name,list)
print(list)

if(interactive()){
  ui<-navbarPage("Visualization",
    tabPanel(
      "Map",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "show","Choose What you want to see:",
            choices = list1
          ),
          helpText(
            "The deeper color means the highest value."
          )),
          mainPanel(
            h3("Map to show"),
            plotOutput("Mapshow"))
          )
        ),tabPanel(
          "Details",
          sidebarLayout(
            sidebarPanel(
              checkboxGroupInput(
                "detail","to see the details of the columns",
                choices = colnames(inner)
              )
            ),
            mainPanel(
              h3("Details:"),
              verbatimTextOutput("det")
              
            )
            
          )
        ),tabPanel(
          "Distribution",
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                "histt","to see the destribution of some columns",
                choices = list
              )
            ),mainPanel(
              h3("Destribution:"),
              plotOutput("showhist")
            )
          )
        ),tabPanel(
          "Correlation",
          sidebarLayout(
            sidebarPanel(
              checkboxGroupInput(
                "corr","to see the correlation of these columns",
                choices = colnames(inner),selected = c("Happiness.Score","Human.Development.Index..HDI.")
              )
            ),
            mainPanel(
              h3("Correlation:"),
              d3heatmapOutput("rela")
            )
          )
        )
        
    )
  server <- function(input, output) {
    output$Mapshow<-renderPlot({
      Stringofname = input$show
      print(Stringofname)
      #print(inner[Stringofname])
      if(Stringofname %in% other.name){
        country1 = data.frame(Country = x,Input = visual[Stringofname])
      }else{
        country1 = data.frame(Country = x,Input = inner[Stringofname])
      }
      #print(country1)
      p1sales.map <- joinCountryData2Map(country1, joinCode = "ISO3",
                                         nameJoinColumn = "Country")
      mapCountryData(p1sales.map, nameColumnToPlot=Stringofname,
                     mapTitle=paste("Map of ",Stringofname,sep = ""),
                     colourPalette=brewer.pal(7, "Greens"),
                     catMethod="fixedWidth", addLegend=FALSE)})
    output$det<-renderPrint({
      for (i in input$detail){
        print(paste(i,":",sep = ""))
        print(summary(inner[i]))
      }
    })
    output$showhist<-renderPlot({
      
      qplot(unlist(inner[input$histt]), geom="histogram",xlab = input$histt)+ggtitle(input$histt) 
    })
    output$rela<-renderD3heatmap({
      plotvar<-input$corr
      corrr<-cor(inner[plotvar])
      d3heatmap(corrr)
    })
  }
  shinyApp(ui = ui, server = server)
}
qplot(unlist(inner["Happiness.Score"]), geom="histogram")

cor(inner[c("Happiness.Score","Labor.Freedom")])
##################    visualization   ###################
#########################################################
####################   split data  ######################
#split through Happiness.Rank
set.seed(9527)
inner.split = sample.split(inner$Happiness.Score,SplitRatio = 0.7)

table(inner.split) #get the number of two parts

#split into training data & testing data
inner.train = subset(inner,inner.split)
inner.test = subset(inner,!inner.split)

nrow(inner.test) #check the number of the rows of the training data
nrow(inner.train) #check the number of the rows of the testing data
####################   split data  ######################
#########################################################
####################   find R^2   #######################
#cor(inner)
#corrplot.mixed(corr=cor(inner, use="complete.obs"), upper="ellipse", tl.pos="lt")
#quartz(height=10, width=8)
####################   find R^2   #######################
#########################################################
####################   run model  #######################

#linear regression
(avg = sum(inner.train$Happiness.Score)/nrow(inner.train))
(sum((inner.train$Happiness.Score-avg)^2/nrow(inner.train))**(1/2))


#run with the whole variables
lin.model<-lm(Happiness.Score~.,data = inner.train)
summary(lin.model)

#run through step linear 
lin.model2<-step(lin.model, direction = 'backward')
summary(lin.model2)

#rmse
(rmse(predict(lin.model,inner.test),inner.test$Happiness.Score))
(rmse(predict(lin.model2,inner.test),inner.test$Happiness.Score))

#random forest
ranfor.model<-randomForest(Happiness.Score~.,data = inner.train)
plot(ranfor.model)
importance(ranfor.model)
varImpPlot(ranfor.model)

#有條參數
ranfor.model2<-randomForest(Happiness.Score~.,data = inner.train,ntree=50,mtry=7)
plot(ranfor.model2)
importance(ranfor.model2)
varImpPlot(ranfor.model2)
#顯然並沒有比較好

#rmse
(rmse(predict(ranfor.model,inner.test),inner.test$Happiness.Score))
(rmse(predict(ranfor.model2,inner.test),inner.test$Happiness.Score))

#decision tree
dec.model<-rpart(Happiness.Score~.,data = inner.train)
prp(dec.model,faclen=0,fallen.leaves=TRUE,cex=1)



dec2.model = rpart(Happiness.Score ~ Human.Development.Index..HDI.+
                     Life.Expectancy.at.Birth+Government.Integrity+
                     Property.Rights+Judical.Effectiveness+Trade.Freedom+Expected.Years.of.Education+
                     X2019.Score+Mean.Years.of.Education+Business.Freedom, data = inner.train)
prp(dec2.model, faclen=0, fallen.leaves = TRUE,cex=1)


#調參
dec3.model = rpart(Happiness.Score ~ Human.Development.Index..HDI.+
                     Life.Expectancy.at.Birth+Government.Integrity+
                     Property.Rights+Judical.Effectiveness+Trade.Freedom+Expected.Years.of.Education+
                     X2019.Score+Mean.Years.of.Education+Business.Freedom, data = inner.train,minsplit=3,minbucket=10,cp=0.017)
prp(dec3.model, faclen=0, fallen.leaves = TRUE,cex=1)

dec4.model = rpart(Happiness.Score ~ Human.Development.Index..HDI.+
                     Life.Expectancy.at.Birth+Government.Integrity+
                     Property.Rights+Judical.Effectiveness+Trade.Freedom+Expected.Years.of.Education+
                     X2019.Score+Mean.Years.of.Education+Business.Freedom, data = inner.train,minsplit=4,minbucket=20,cp=0.00017)
prp(dec4.model, faclen=0, fallen.leaves = TRUE,cex=1)

sum(inner$Happiness.Score)/nrow(inner)
max(inner$Happiness.Score)
min(inner$Happiness.Score)

#rmse
(rmse(predict(dec.model,inner.test),inner.test$Happiness.Score))
(rmse(predict(dec2.model,inner.test),inner.test$Happiness.Score))
(rmse(predict(dec3.model,inner.test),inner.test$Happiness.Score))
(rmse(predict(dec4.model,inner.test),inner.test$Happiness.Score))


#gbm 
gbm.model<-gbm(Happiness.Score ~.,data = inner.train)
summary(gbm.model)
print(rmse(predict(gbm.model,inner.test,n.trees = 50),inner.test$Happiness.Score))

#調參
gbm.model2<-gbm(Happiness.Score ~.,data = inner.train,
               n.trees=100, interaction.depth=4,cv.folds = 6)
(rmse(predict(gbm.model2,inner.test,n.trees = 50),inner.test$Happiness.Score))

####################   run model  #######################

#########################################################

####################  predict model #####################

print(rmse(predict(lin.model,inner.test),inner.test$Happiness.Score))
print(rmse(predict(dec.model,inner.test),inner.test$Happiness.Score))
print(rmse(predict(ranfor.model,inner.test),inner.test$Happiness.Score))
print(rmse(predict(gbm.model,inner.test,n.trees = 50),inner.test$Happiness.Score))


####################  predict model #####################


######################把Happiness.Score用二分法的方式在5.213的地方分成0 and 1################
#用一個新的資料變數去操作
inner1 = inner

inner1$Happiness.Score= ifelse(inner1$Happiness.Score<5.213, 0, 1)
inner1.split = sample.split(inner1$Happiness.Score,
                           SplitRatio = 0.8)


table(inner1.split) #get the number of two parts

#split into training data & testing data
inner1.train = subset(inner1,inner.split)
inner1.test = subset(inner1,!inner.split)
nrow(inner.test) #check the number of the rows of the training data
nrow(inner.train) #check the number of the rows of the testing data

inner1.train$Happiness.Score=as.factor(inner1.train$Happiness.Score)
inner1.test$Happiness.Score=as.factor(inner1.test$Happiness.Score)

#################### run logit model #####################
logit_reg=glm(Happiness.Score ~ ., data=inner1.train, family=binomial(link="logit")) #?
summary(logit_reg) 

#看預測的tpr,accuracy等
table(true=inner1.train$Happiness.Score, pred=ifelse(predict(logit_reg,data = inner1.train)>5.213,1,0)) #train data
table(true=inner1.test$Happiness.Score, pred=ifelse(predict(logit_reg,newdata = inner1.test)>5.213,1,0)) #test data

#跑roc curve
pred=predict(logit_reg,newdata = inner1.test,type = "response")
prefff<-prediction(as.numeric(pred),inner1.test$Happiness.Score)
per<-performance(prefff, "tpr", "fpr")
plot(per)
#get auc value
auc<-performance(prefff,"auc")
auc

#################### run logit model #####################


#################### run decision tree model #####################

#######跑全部變數模型
dec.model2=as.party(rpart(Happiness.Score~., data=inner1.train))
plot(dec.model2)

#看預測的tpr,accuracy等
table(true=inner1.train$Happiness.Score, pred=predict(dec.model2,newdata = inner1.train)) #?
table(true=inner1.test$Happiness.Score, pred=predict(dec.model2,newdata = inner1.test)) #?

#跑roc curve
pred1=predict(dec.model2,newdata = inner1.test,type = "response")
prefff1<-prediction(as.numeric(pred1),inner1.test$Happiness.Score)
per1<-performance(prefff1, "tpr", "fpr")
plot(per1)
#get auc value
auc1<-performance(per1,"auc")
auc1

#######跑全部變數模型

#######決策樹跑部分變數模型
dec.model3=as.party(rpart(Happiness.Score~Life.Expectancy.at.Birth+Human.Development.Index..HDI.+Mean.Years.of.Education+Expected.Years.of.Education+Government.Integrity+Business.Freedom+Property.Rights, data = inner1.train))
plot(dec.model3)

#看預測的tpr,accuracy等
table(true=inner1.train$Happiness.Score, pred=predict(dec.model3,newdata = inner1.train)) #?
table(true=inner1.test$Happiness.Score, pred=predict(dec.model3,newdata = inner1.test)) #?


#跑roc curve
pred2=predict(dec.model3,newdata = inner1.test,type = "response")
prefff2<-prediction(as.numeric(pred2),inner1.test$Happiness.Score)
per2<-performance(prefff2, "tpr", "fpr")
plot(per2)
#get auc value
auc2<-performance(per2,"auc")
auc2
#######決策樹跑部分變數模型

#################### run random forest #####################

random=randomForest(Happiness.Score~.,data = inner.train)
plot(random)
importance(random)
varImpPlot(random)

#看預測的tpr,accuracy等
pred=predict(random, newdata=inner.test)
table(Real = inner.test$Happiness.Score, Predict = pred)

#################### run random forest #####################


#################### run gbm #####################
gbm.model<-gbm(Happiness.Score~., data = inner.train, n.trees = 1500,interaction.depth = 4,shrinkage = 0.05,distribution="gaussian")
summary(gbm.model, order = T,plotit=T)

#################### run gbm #####################
#######################################end#################################