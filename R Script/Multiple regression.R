library(rpart)
library(rpart.plot)
library(tree)
library(evtree)

install.packages("partykit")

pacman :: p_load(caret, party, reshape, ggplot2, dplyr,reshape,corrplot,rpart,rpart.plot,tree,evtree)


setwd("C:/Users/Martin Albaek/Documents/coding academy/Ubiqum/Course/Module 2/Part 3/Module 3 Task 2 - Multiple Regression in R/Datasets")

Test_DataSet <- read.csv("existingproductattributes2017.2.csv")
Predicting_dataset <- read.csv("newproductattributes2017.2.csv")

# Creation of Loop so to have a graph of each column against product number to see the distribution of variables
loop.vector <- c(1:18)

pdf(file="Exploration data.pdf")

for (i in loop.vector) {
  x <- Test_DataSet[,i]
  plot(Test_DataSet$ProductType,
       x,
       main = paste("Plot of Product number against",colnames(Test_DataSet)[i]),
       xlab = "Product number",
       ylab = colnames(Test_DataSet)[i])
}

dev.off()

#dummify the data
NewDataFrame <- dummyVars("~.", data=Test_DataSet)
readyData <- data.frame(predict(NewDataFrame, newdata=Test_DataSet))

# Get rid of the outliers of the independent variables
hist(readyData$Volume)
boxplot(readyData$Volume)
readyData <- readyData[which(readyData$Volume<=4000),]


#Graph for dummy variables
pdf("Dummyvariables Graphs")
plot(readyData$ProductType.GameConsole,readyData$Volume)
dev.off()

str(readyData)

#Best Seller Rank has 15 missing variables
readyData$BestSellersRank <- NULL

#Product Number should not have qany influence on the customer choice
readyData$ProductNum <- NULL

#Correlation Data
corrData <- cor(readyData)
corrplot(corrData)

#Corr of Sales with other variables
corrSales <- cor(readyData$Volume,readyData)
corrplot(corrSales)

#Delete Overly fitted variables 
readyData$x5StarReviews <- NULL

#Drop the variables with lowest correlation with sales (Dot almost absent) except for product types and those who overfit
readyData$ProfitMargin <- NULL
readyData$Price <- NULL
readyData$ProductDepth <- NULL
readyData$x5StarReviews <- NULL
readyData$ShippingWeight <- NULL
readyData$ProductHeight <- NULL
readyData$ProductWidth <- NULL

#Drop attributes to highly correlated among them
readyData$x3StarReviews <- NULL
readyData$x2StarReviews <- NULL
readyData$x1StarReviews <- NULL

#Drop severala product type with little correlation with Sales
readyData$ProductType.Display <- NULL
readyData$ProductType.ExtendedWarranty <- NULL
readyData$ProductType.Netbook <- NULL
readyData$ProductType.PC <- NULL
readyData$ProductType.Printer <- NULL
readyData$ProductType.PrinterSupplies <- NULL
readyData$ProductType.Smartphone <- NULL
readyData$ProductType.Software <-NULL
readyData$ProductType.Tablet <- NULL
readyData$ProductType.Laptop <- NULL
readyData$ProductType.Accessories <- NULL

#Check the Decision tree to see if there is any significant correlation among the remaining variables

dtm <- rpart(Volume ~., 
                     readyData,
             control = rpart.control(maxdepth = 30))

rpart.plot(dtm)

#only Positive Service Review and x4StarReviews remain, delete all the rest of variables
readyData$ProductType.GameConsole <- NULL
readyData$Recommendproduct <- NULL
readyData$NegativeServiceReview <- NULL

# Plot them to see if there is any outlier and delete them
boxplot(readyData$x4StarReviews)
hist(readyData$x4StarReviews)
readyData <- readyData[which(readyData$x4StarReviews<60),]

boxplot(readyData$PositiveServiceReview)
hist(readyData$PositiveServiceReview)
readyData <- readyData[which(readyData$PositiveServiceReview<14),]

#only Positive Service Review and x4Star Reviews Remain

#Create Test Datasets
set.seed(123)

inTrain <- createDataPartition(readyData$Volume, p =0.7, list=F)

trainSet <- readyData  [ inTrain,]
testSet <- readyData   [-inTrain,]

#Creation of loop for the use of different models with the dataset compared to Volume of sales
methods <- c("knn","rf","svmLinear","svmRadial")

Model_Variables <- c("Volume ~ .","Volume ~ x4StarReviews","Volume ~ PositiveServiceReview" )

comp_model <- c()

na.exclude(readyData)

is.na(readyData)

for (i in methods) {
  for (j in Model_Variables) {
  model <- train(
  data = trainSet,
  method = i,
  formula(j))
  }
  Pred <- predict(model,testSet)
  Pre.metric <- postResample(Pred,testSet$Volume)
  comp_model <- cbind(comp_model,Pre.metric)
}

comp_model
ResulTable <- comp_model
colnames(ResulTable) <- methods

#Creation of graph with the metrics of the different models
ResulTable_Melt <- melt(ResulTable)

ggplot(ResulTable_Melt, aes(x=X2, y=value))+geom_col() +facet_grid(X1~.,scales="free")

#Applying the predictive models to the existing attributes 