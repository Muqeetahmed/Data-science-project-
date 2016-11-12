# Injecting the data in to R environment

wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

str(wbcd)

#Removing the unwanted colums that do not need for oredicting the model

wbcd <- wbcd[-1]

#checking for classification for malignant and benign

table(wbcd$diagnosis)

#Now converting the labels of malignant and benign

wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),labels = c("Benign",
                                                                        
                                                                        "Malignant"))

# checking the propotionality 

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#creating the function to normalize the data

normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean)

#Data preparation process, creating training and test datasets

wbcd_train <- wbcd_n[1:469, ]

wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1]

wbcd_test_labels <- wbcd[470:569, 1]

#Training  The model

install.packages("class")

library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k =
                        
                        21)

#evaluating model performance

install.packages("gmodels")

library(gmodels)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)
