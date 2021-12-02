# Group Zetta
#Title: How Rich are the Rich?
# R Codes - Part 2: Initial Analysis Report
# Group Members: Lea Alkhoury, Harshita Raju Manek, Pratik Premkishor Mantri, Isha Rajesh More, Megh Vipul Patel.
# Presented to Prof. Valeriy 
# May 12, 2021 - Spring 2021 - ALY 6015 - 80506
####These are my libraries if you want to use
#install packages
# install.packages('plotly')#plotting graphs
# install.packages('tidyverse')#it helps connect between the tools to make  easy workflow
# install.packages("dplyr")#this library used for data manipulation and data manging
# install.packages("'ggplot")# it is popular visualization library
# install.packages("caret")# For machine learning algorithms
# install.packages("corrplot")#it allows correlation matrix
# install.packages('pastecs')
# 
# library(corrplot)
# library(psych)
# library(ggplot2)
# library(pastecs)
# library(dplyr)
# library(RColorBrewer)
# library(tidyverse)
# library(plotly)


library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(psych)
library(epiDisplay)
library(ggplot2)
library(skimr)
library(devtools)
library(readr)
library(funModeling)
library(plotrix)
library(scales)
library(moments)
library(ggplot2)
library("ggpubr")
theme_set(
  theme_bw() +
    theme(legend.position = "top")+
    theme_set(theme_classic())
)
library(MASS)
library(Tmisc)
library(DataExplorer)
library(haven)
library(knitr)
library(gginference)
library(car)
library(BAS)
library(conover.test)
library(FSA)
library(FSAdata)
library(magrittr)
library(tidyverse)
library(psych)
library(epiDisplay)
library(ggplot2)
library(gmodels)
library(skimr)
library(devtools)

devtools::install_github("ropensci/visdat")
library(visdat)
devtools::install_github("alastairrushworth/inspectdf")
library(inspectdf)
library(readr)
library(funModeling)
library(plotrix)
library(scales)
library(moments)
library(ggplot2)
library(ggpubr)
library(reshape)
library(sjPlot)
library(rmarkdown)
library(broom)
library(stargazer)
library(correlation)
library(rquery)
library(RColorBrewer)
library(Hmisc)
library(nortest)
library(gtsummary)

### Importing the Dataset
FortuneData <- read.csv(file.choose()) #uploading my dataset

### Question 2: EDA and Descriptive Analysis: 
vis_dat(FortuneData) #shows observation and type of each observation
x<-inspect_na(FortuneData)  #inspects na for each of the observation 
show_plot(x) #plotting the NA availability
y<-inspect_cat(FortuneData) #shows the frequency of categorical level 
show_plot(y) #plotting the frequency of categorical levels
summary(FortuneData) #gives me a summary of the dataset
glimpse(FortuneData) #gives me a glimpse of my datasets
dim(FortuneData) #gives me number of rows and columns of the dataset

## Cleaning data from NAs
sum(is.na(FortuneData)) ### Total number of NAs 525
most.NA <- colSums(is.na(FortuneData))
most.NA <- sort(most.NA, decreasing = T)  # Descending Order
NA.Values <- data.frame(Variable = names(most.NA)[1:10],
                        NA.Values = unname(most.NA)[1:10]) # Creating Summary Table.
NA.Values

#Assign better coloumn names
names(FortuneData)[6]<-"no.of_employees" #changing the name of the column
names(FortuneData)[1]<-"Comapany_name" # changing the name of the column
names(FortuneData)

# Selecting Columns to work with:
Fortune.Data <- dplyr::select (FortuneData,Comapany_name,rank,rank_change,revenue,profit, no.of_employees, sector, city,state,newcomer,ceo_founder,ceo_woman,profitable,Market.Cap)
colSums(is.na(Fortune.Data))
ls(Fortune.Data) ### listing column Names

## Cleaning data from missing values
#3. Removing missing values/NA's

#Using average for the missing values for the profit and revenue columns
mean(Fortune.Data$profit,na.rm=TRUE)
Fortune.Data$profit[is.na(Fortune.Data$profit)]<-mean(Fortune.Data$profit,na.rm=TRUE) #Replacing NAs with mean value

#sum of no. of missing values
sum(is.na(Fortune.Data))
map(Fortune.Data, ~sum(is.na(.)))#sum of no. of missing values coloumn wise

#put NA values where we have " "
Fortune.Data[Fortune.Data==""]<-NA
sum(is.na(Fortune.Data))
map(Fortune.Data, ~sum(is.na(.)))

####we have used mutate function to replace the NA values as none for newcomer 
Fortune.Data<-Fortune.Data%>%mutate(newcomer=replace_na(newcomer,"none"))
map(Fortune.Data, ~sum(is.na(.)))

#we want to know the structure of the data like what are the numeric and characters in our data
str(Fortune.Data)

#Transforming my integer columns into numeric
Fortune.Data$no.of_employees <- as.numeric(Fortune.Data$no.of_employees)
Fortune.Data$Market.Cap<-as.numeric(Fortune.Data$Market.Cap)

#Replacing NA with mean value for market capital
mean(Fortune.Data$Market.Cap,na.rm=TRUE)
Fortune.Data$Market.Cap[is.na(Fortune.Data$Market.Cap)]<-mean(Fortune.Data$Market.Cap,na.rm=TRUE) #Replacing NAs with mean value
map(Fortune.Data, ~sum(is.na(.)))

# Visualizing Dataset
dim(Fortune.Data)
glimpse(Fortune.Data) 
skim(Fortune.Data)
vis_dat(Fortune.Data)
plot_missing(Fortune.Data)

# Descriptive Analysis: 
Statistics <- psych:: describe(Fortune.Data)
Statistics
write.csv(Statistics,"StatisticsTableFortune.csv")
StatDataSet <- read.csv(file.choose())
keeps <- c("n","mean","median","sd","min","max")
StatDataSet = StatDataSet[keeps] ### Keeping dataset with only 6 variables to study.

#Question for Initial Analysis

#Correlation Matrix for our dataset to understand the relationship between the variables
NumericData <- Fortune.Data[c("rank","rank_change","revenue","profit","no.of_employees","Market.Cap")]
NumericData$Market.Cap <- as.numeric(NumericData$Market.Cap)
NumericData$rank <- as.integer(NumericData$rank)
NumericData$rank_change <- as.integer(NumericData$rank_change)
DataCorr <- cor(NumericData, use="pairwise")
install.packages("corrplot")
library(corrplot)
corrplot(DataCorr, type = "upper", col = brewer.pal(n = 8, name = "RdYlBu"))

#Inference
#The above correlation plot shows the relation between all the numeric variables present in the given data set.
#According to the plot, there is a strong correlation between revenue, profit, number of employees and market capital.

#Question 1: Is there an existing relation between revenue and profit ?
ggplot(data = Fortune.Data, aes(x = revenue, y = profit)) + geom_point(color='blue') + geom_smooth(method = "lm", se = FALSE,color="red") + xlab("Revenue") + ylab("Profit") + ggtitle("Relation between Revenue and Profit")

#Inference
#The scatter plot shows a positive relation between profit and revenue with few outliers

#Question 2: Does the ranking of the company affect the number of employees?
#H0 : The ranking does affect the number of employees.
#Ha : The ranking does not affects the number of employees.

#checking if our dataset is normally distributed 
ggqqplot(Fortune.Data$rank)+
  ggtitle("Normality distribution for company rank")+
  theme(plot.title = element_text(face = "bold"))

ggqqplot(Fortune.Data$no.of_employees)+
  ggtitle("Normality distribution for number of employees")+
  theme(plot.title = element_text(face = "bold"))

#Assuming our data set is normally distributed with alpha equals to 0.05
model <- lm(no.of_employees~rank, data = Fortune.Data)
summary(model)

#Inference
#The p value for the model is extremely less than 0.05 which means  there is no significant effect of ranking on number of employees we rejected null hypothesis and accepted alternative hypothesis.


#Question 3: Which sector generates the most profit ?
ggplot(data=Fortune.Data, aes(x=sector, y=profit)) +geom_bar(stat="identity", fill="blue")+ xlab("Sector") + ylab("Profit") + ggtitle("Profit Vs.Sector") + coord_flip()

#Inference
#According tot the chart, Technology, Health care and Financial are generating the most profit of all sectors.

#Question 4:How does the Profit affect market capital ?
SectorFilter <- filter(Fortune.Data,sector=="Technology" | sector=="Health Care" | sector == "Financials") #We created a data for the 3 most profitable sectors to test the effect of profit on market capital 
View(SectorFilter) 
ggplot(SectorFilter, aes(x=Market.Cap, y=profit)) + geom_point(size=2, shape=19, colour = "blue") + geom_smooth(method = "lm", se = FALSE,color="red") + ggtitle("Relationship of Profit and Market Capital for top 3 profiting sectors") + xlab("Market Capital") + ylab("Profit")

#Inference 
#According to the correlation plot there is a high correlation between market capital and profit. This result is based on the top profitable sectors for better accuracy.

#Question 5: Some claim that the number of employees affect the profit generated by the company. Is that correct are these two correlated? 

#Studying normality distribution of profit
ggqqplot(Fortune.Data$profit)+
  ggtitle("Normality distribution for profit")+
  theme(plot.title = element_text(face = "bold"))


# Number of employees and profit are both normally distributed
# we can assume alpha is 0.05

# Step 1: State Hypothesis 
# H0 = Number of employees does affect the profit generated.
# H1 = number of employees does not affect the profit generated.

#Step 2: Compute critical value 
alpha = 0.05
n=1000
critical.r <- function( n, alpha = .05 ) {
  df <- n - 2
  critical.t <- qt(alpha/2, df, lower.tail = F)
  critical.r <- sqrt( (critical.t^2) / ( (critical.t^2) + df ) )
  return(critical.r)
}
critical.r(1000)
#Critical value = 0.06 > alpha 

#Step 3: Computing test value for: Spearman Test
spearman <- cor.test(Fortune.Data$no.of_employees, Fortune.Data$profit, 
                     method = "spearman", conf.level = 0.95, correct=FALSE, exact = FALSE )
spearman

#Step 4: Making decision: 
ifelse(spearman$p.value >alpha, "Fail to reject the Null Hypothesis", "Reject Null Hypothesis")

# Step 5: Summarizing results 
# alpha < 0.05 

#Inference
#From the  hypothesis testing the p value has been extremely smaller compared to 0.05 which means number of employees have a significant effect on profit.

#Question 6: Can a non-profitable company generate a high revenue ?
ggplot(data=Fortune.Data, aes(x=profitable, y=revenue)) + geom_bar(stat="identity", fill="coral",width = 0.5)+ xlab("Profitable") + ylab("Revenue") + ggtitle("Profitable Vs. Revenue") 

#Inference
#Non profitable company cannot generate more revenue


#Question 7: Does the CEO gender affect company's profit generation?
NewData <-select(Fortune.Data,profit,ceo_woman)
NewData$ceo_woman<-as.factor(NewData$ceo_woman)

# Split the data into training and test set
library(glmnet)
library(caret)
set.seed(123)
training.samples <- sample(nrow(NewData), nrow(NewData)*.7) #getting 70% random samples
training.samples
train.data  <- NewData[training.samples, ]
test.data <- NewData[-training.samples, ]

## Fit the model
model2 <- glm( ceo_woman ~profit, data = train.data, family = binomial)
# Summarize the model
summary(model2)
#The p-value is less than level of significance 0.05 so we can conclude that 
# CEO gender does not affect profit of the company


#Question 8: Which state has the most profitable companies ? What percentage of companies have women CEO's ? 
mostprofit <- filter(Fortune.Data,profit>=20000)
mostprofit2 <- filter(Fortune.Data,profit>=50000)

#All states
ggplot(data=Fortune.Data, aes(x=state, y=profit)) +geom_bar(stat="identity", fill="blue")+ xlab("State") + ylab("Profit") + ggtitle("State Vs. Profit") + coord_flip()
#Greater than 20000
ggplot(data=mostprofit, aes(x=state, y=profit)) +geom_bar(stat="identity", fill="blue")+ xlab("State") + ylab("Profit") + ggtitle("State Vs. Profit") 

#Greater than 50000
ggplot(data=mostprofit2, aes(x=state, y=profit)) +geom_bar(stat="identity", fill="blue")+ xlab("State") + ylab("Profit") + ggtitle("State Vs. Profit") 

#Percentage of Women CEO in companies 
Women <- count(Fortune.Data$ceo_woman)
names(Women) [1] = "WomenCEO"
names(Women)[2] = "Frequency"
lbls <- c(Women$WomenCEO)
pct <- round(Women$Frequency/sum(Women$Frequency)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(Women$Frequency,labels = lbls, col=rainbow(length(lbls)),
    main="Percentage of Women CEO", cex= 0.5)
#Inference
#The percentage of companies with woman CEOs is 7% whereas others 93%

#Question 9:Which is the most significant variable that impacts profit? 
Impact.Profit <- lm(profit ~ revenue+no.of_employees+Market.Cap, data = Fortune.Data)
summary(Impact.Profit)  
#we can say that number of employees is mostly affecting profit since it has the highest absolute coefficient estimate

#Question 10: How many companies are newcomers ?
ggplot(Fortune.Data,aes(x = newcomer)) + geom_bar(aes(y=(..count..)), fill = "coral", color = "coral", width = 0.5) + ylab("Total number of companies") + xlab("New comers") + ggtitle("Graph for total number of new companies")

#Inference
#The graph displays the number of companies which are new comers is very less which is approximately between 0 to 20

#Question 11: What percentage of companies have CEOs who were also founders? 
CEO.Founder <- count(Fortune.Data$ceo_founder)
names(CEO.Founder) [1] = "Founder"
names(CEO.Founder)[2] = "Frequency"
lbls2 <- c(CEO.Founder$Founder)
pct2 <- round(CEO.Founder$Frequency/sum(CEO.Founder$Frequency)*100)
lbls2 <- paste(lbls2, pct2) # add percents to labels 
lbls2 <- paste(lbls2,"%",sep="") # ad % to labels 
CEO.Founder$pct = pct2

ggplot(data=CEO.Founder, aes(x=Founder, y=pct)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=lbls2), vjust=-0.3, size=3.5)+
  theme_minimal()


#Question 12: What role does profitability play? are the prediction correct? 
mydata<-select(Fortune.Data,profit,revenue,no.of_employees,profitable)
mydata$profitable <- as.factor(mydata$profitable)  
mydata$no.of_employees <- as.numeric(mydata$no.of_employees)
mydata$profit <- as.numeric(mydata$profit)
mydata$revenue <- as.numeric(mydata$revenue)
 #scatter plot
 qplot(data=mydata,x=profit,y=revenue,fill=profitable,shape=profitable,geom='point')+scale_shape(solid=FALSE)
 
 #Preparing the data
 set.seed(1)
 Sample <- sample(nrow(mydata), nrow(mydata)*.7) #Get70% of random row numbers
 train.data2 <- mydata[Sample, ] #Get Train data which includes 70% of rows
 train.data2
 test.data2 <- mydata[-Sample, ] #Get Test data which excludes 70% of rows and includes 30%
 test.data2
 
 #The R function glm(),for generalized linear model, can be used to compute logistic regression.
 MyGlMmodel <- glm(profitable ~profit+revenue+no.of_employees, family="binomial", data=train.data2) # where Private is dependent and the rest are independent
 summary(MyGlMmodel) # to get the coefficient and information of the model
 MyGlMmodel
 train.data2$pred <- predict(MyGlMmodel,train.data2, type="response") # calculating the prediction values 
 train.data2$pred_label <- as.factor(ifelse(train.data2$pred >= 0.5, "Yes", "No")) #less than 0.5 is No, adding a column to the data frame
 
#Creating Confusion Table 
 install.packages("e1071") #required for confusion matrix
 library(e1071)
 library(caret)
 table(train.data2$profitable, train.data2$pred_label) # studying the prediction of public/private uni
 
 # Confusion Table for test 
 test.data2$pred <- predict(MyGlMmodel,test.data2, type="response") # adding prediction column
 test.data2$pred_label <- as.factor(ifelse(test.data2$pred >= 0.5, "No", "Yes")) # adding predicion labels  where less than 0.05 is NO
 table(test.data2$profitable, test.data2$pred_label) # creating confusion matrix
 
 #Generating ROC
 install.packages("pROC")
 library(pROC)
 # Generate data for the ROC curve to see how well the model fit for prediction
 rocobj <- roc(as.ordered(test.data2$profitable), as.ordered(test.data2$pred_label), ordered = TRUE) # calculating ROC and plotting private to predicted TPR and FPR
 auc <- round(auc(as.ordered(test.data2$profitable), as.ordered(test.data2$pred_label)),4) # calculating area under ROC (AUC)
 ggroc(rocobj, colour = 'steelblue', size = 2) +
   ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) # plotting ROC with a blue line 
 
 # Calculate area under the curve
 auc1 <- auc(rocobj) # calculating AUC from ROCOBJ
 auc1
 #The area under the curve is 0.9737 which is the best model.
 
#Question 13: Taking a sample of 2 different sectors in 2 different states, can we say that sector and State affect the
#profit of the company? Can an interaction be concluded between sector and state? 
 #Step 1: State Hypothesis
#H0 : Sector and state have no effect on the profit of the company. There is no interaction between state and sector.
#H1 : Sector and state have an effect on profit of the company. There is an interaction
 sector <- filter(Fortune.Data, Fortune.Data$sector == c("Financials","Technology"))
 target <- c("CA","NY")
 sector_state <- filter(sector,state %in% target) 
 sector_state
 
 #Step 2:Calculate critical value
 mylm3 <- lm(profit ~ sector + state, data = sector_state)
 myanova3 <- anova(mylm3)
 cbind(myanova3, 'CriticalValue' = qf(1-.05, myanova3[1,1], myanova3[2,1])) # adding critical value
 
 
 #Step 3: Compute test value 
 anova_two_way_interaction <- aov(profit ~sector+ state + sector*state , data = sector_state)
 anova_two_way_interaction
 Anova.Sum <- summary(anova_two_way_interaction)
 Anova.Sum

 #Question 14: Can we conclude that there is a difference in the average of revenue, profit, market capital?

 #Taking samples from each of the variables; sample of 10 and creating the data frame 
 
 Revenue <- head(Fortune.Data$revenue,10)
 revenue <- data.frame(amount = Revenue, group = rep("Revenue",10))
 Profit <- head(Fortune.Data$profit,10)
 profit <- data.frame(amount = Profit,group = rep("Profit",10))
 Market.Cap <- head(Fortune.Data$Market.Cap,10)
 market.capital <- data.frame(amount = Market.Cap, group = rep("Market.Capital",10))
 
 average <- rbind(revenue,profit,market.capital)
 
 #Step 1: Stating Hypothesis: 
 #H0: There is no difference in the average of revenue, profit, market capital 
 #Ha: There is difference in the average of revenue, profit, market capital (Claim)

 # Step 2: Computing critical value: 
 
 mylm1 <- lm(amount~ group,data=average)
 myanova <- anova(mylm1)
 cbind(myanova, 'CriticalValue' = qf(1-.05, myanova[1,1], myanova[2,1])) # adding critical value
 
 # Step 3: Calculating test-value: 
 Anova.Testing <-aov(amount~ group,data=average)
 Summary <-summary(Anova.Testing)
 Summary
 TukeyHSD(Anova.Testing)
 p.value <-Summary [[1]][[1,"Pr(>F)"]]
 p.value
 
#Step 4: Make decision 
 ifelse(p.value>alpha, "Fail to reject the Null Hypothesis", "Reject Null Hypothesis")
