# Sales prediction --------------------------------------------------------
# Floriana Trama ----------------------------------------------------------
# Data analysis department ------------------------------------------------
# Predicting sales of four different product types:PC,Lap,Net,Smart -------
# 15th February 2019 ------------------------------------------------------
# KNN ---------------------------------------------------------------------



# Libraries ---------------------------------------------------------------

library(readr)

library(caret)

library(corrplot)

library(BBmisc)

library(e1071)


# Uploading Data set ------------------------------------------------------

ExistingPdt <- read_csv("C:/Users/T450S/Desktop/Floriana/Ubiqum/Data Analytics II/Task 3/existingproductattributes2017.csv")


# Data exploration --------------------------------------------------------

summary(ExistingPdt)

str(ExistingPdt)

plot(ExistingPdt$x5StarReviews, ExistingPdt$Volume)

plot(ExistingPdt$x4StarReviews, ExistingPdt$Volume)

plot(ExistingPdt$x3StarReviews, ExistingPdt$Volume)

plot(ExistingPdt$x2StarReviews, ExistingPdt$Volume)

plot(ExistingPdt$PositiveServiceReview, ExistingPdt$Volume)

plot(ExistingPdt$Recommendproduct, ExistingPdt$Volume)

ggplot(ExistingPdt, aes(x4StarReviews, Volume, color = ProductType))+
  
  geom_jitter(alpha = 1)

ggplot(ExistingPdt, aes(PositiveServiceReview, Volume, color = ProductType))+
  
  geom_jitter(alpha = 1)


# Outliers ----------------------------------------------------------------

boxplot(ExistingPdt$Volume)$out

ExistingPdt <- ExistingPdt[ExistingPdt$Volume < 7000,]


# Dummify the data --------------------------------------------------------

ExistingPdtDummy <- dummyVars(" ~ .", data = ExistingPdt)

readyData <- data.frame(predict(ExistingPdtDummy, newdata = ExistingPdt))

str(readyData)

summary(readyData)


# Correlation -------------------------------------------------------------

corrData <- cor(readyData)

corrData

corrplot(corrData)

cor(readyData$PositiveServiceReview, readyData$Volume)


# New variable creation: wighted mean of star reviews ---------------------

summary(readyData)

readyData["StarWeighted"] <- NA

readyData$StarWeighted <- as.numeric(readyData$StarWeighted)

readyData$StarWeighted <- (readyData$x5StarReviews*5 +
                             readyData$x4StarReviews*4 +
                             readyData$x3StarReviews*3 +
                             readyData$x2StarReviews*2 +
                             readyData$x1StarReviews*1) / rowSums(readyData[15:19])
summary(readyData$StarWeighted)


# New variable creation: sum of stars number ------------------------------

readyData["TotStar"] <- NA

readyData$TotStar <- as.numeric(readyData$TotStar)

readyData$TotStar <- (rowSums( readyData[,15:19]))

str(readyData)

summary(readyData)


# Calculate correlation of the new variables ------------------------------

cor(readyData$StarWeighted, readyData$Volume)

cor(readyData$TotStar, readyData$Volume)

cor(readyData$x4StarReviews, readyData$StarWeighted)

cor(readyData$x4StarReviews, readyData$TotStar)

cor(readyData$PositiveServiceReview, readyData$StarWeighted)

cor(readyData$PositiveServiceReview, readyData$TotStar)


# Normalization -----------------------------------------------------------

readyData <- cbind(readyData$Volume ,
                   normalize(readyData[,-29],
                             method = "standardize",
                             range = c(0, 1), 
                             margin = 2, on.constant = "quiet"))


# Feature selection -------------------------------------------------------

readyDatasubset <- readyData[c(1,17,21,31)]


# Modelization: KNN -------------------------------------------------------
# Set seed ----------------------------------------------------------------

set.seed(123)


# Create 75%/25% training and test sets -----------------------------------

inTraining <- createDataPartition(readyDatasubset$`readyData$Volume`, p = 0.75, list = FALSE)

training <- readyDatasubset[inTraining,]

testing <- readyDatasubset[-inTraining,]


# Cross validation --------------------------------------------------------

KNNControl <- trainControl(method="repeatedcv", search = "random", repeats = 3)


# Train KNN model ---------------------------------------------------------

KNNModel <- train(`readyData$Volume` ~ ., data = training, method = "knn",
                  trControl = KNNControl,
                  tuneLength = 20)


# Training results --------------------------------------------------------

KNNModel


# Predictions -------------------------------------------------------------

KNNPrediction <- predict(KNNModel, testing)

KNNPrediction

postResample(KNNPrediction, testing$`readyData$Volume`)

Specialtable <- cbind(testing, KNNPrediction )


# Calculate error ---------------------------------------------------------

Specialtable$`readyData$Volume2` <- as.numeric(Specialtable$`readyData$Volume`)

Specialtable$KNNPrediction2 <- as.numeric(Specialtable$KNNPrediction)

error <- abs(Specialtable$`readyData$Volume2` - Specialtable$KNNPrediction2)

Errorplot <- cbind(Specialtable, error)

plot(Specialtable$KNNPrediction2, Specialtable$`readyData$Volume`)


# GGplot for errors -------------------------------------------------------

ggplot(Errorplot, aes(x4StarReviews, PositiveServiceReview, color = error))+
  
  geom_jitter(alpha = 0.5)+
  
  scale_color_gradient(low="white", high="red")


# New product dataset -----------------------------------------------------

NewPdt <- read_csv("newproductattributes2017.csv")


# Dummify the data in the New product dataset -----------------------------

NewPdtDummy <- dummyVars(" ~ .", data = NewPdt)

readyDataNewPdt <- data.frame(predict(NewPdtDummy, newdata = NewPdt))

str(readyDataNewPdt)

summary(readyDataNewPdt)


# New variable creation in the New product dataset: wighted mean o --------

summary(readyDataNewPdt)

readyDataNewPdt["StarWeighted"] <- NA

readyDataNewPdt$StarWeighted <- as.numeric(readyDataNewPdt$StarWeighted)

readyDataNewPdt$StarWeighted <- (readyDataNewPdt$x5StarReviews*5 +
                                   readyDataNewPdt$x4StarReviews*4 +
                                   readyDataNewPdt$x3StarReviews*3 +
                                   readyDataNewPdt$x2StarReviews*2 +
                                   readyDataNewPdt$x1StarReviews*1) / rowSums(readyDataNewPdt[15:19])

summary(readyDataNewPdt$StarWeighted)


# New variable creation in the New product dataset: sum of stars n --------

readyDataNewPdt["TotStar"] <- NA

readyDataNewPdt$TotStar <- as.numeric(readyDataNewPdt$TotStar)

readyDataNewPdt$TotStar <- (rowSums( readyDataNewPdt[,15:19]))

str(readyDataNewPdt)

summary(readyDataNewPdt)


# Changing the columns' disposition ---------------------------------------

readyDataNewPdt <- cbind(readyDataNewPdt$Volume ,
                   normalize(readyDataNewPdt[,-29],
                             method = "standardize",
                             range = c(0, 1), 
                             margin = 2, on.constant = "quiet"))


# Feature selection in New product dataset --------------------------------

readyDataNewPdtSubset <- readyDataNewPdt[c(1,17,21,31)]


# Predictions on New products dataset -------------------------------------

FinalPrediction <- predict(KNNModel, readyDataNewPdtSubset)

FinalPrediction

SpecialtableNewPdt <- cbind(readyDataNewPdtSubset, FinalPrediction )

summary(FinalPrediction)


# Output ------------------------------------------------------------------

output <- readyDataNewPdtSubset 

output$FinalPrediction <- FinalPrediction

write.csv(output, file="C2.T3output.csv", row.names = TRUE)
