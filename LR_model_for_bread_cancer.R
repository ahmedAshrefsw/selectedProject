# Import the data and set up header names 

dataset <- read.table("breast cancer.csv", header=FALSE,
                     sep=",")

#1 Clump Thickness = CT
#2 Uniformity of Cell Size = UCSize
#3 Uniformity of Cell Shape = UCShape
#4 Marginal Adhesion = MA
#5 Single Epithelial Cell Size = SECS
#6 Bare Nuclei = BN
#7 Bland Chromatin = BC
#8 Normal Nucleoli = NN

names(dataset) <- c('id','CT','UCSize','UCShape','MA','SECS','BN','BC','NN','Mitoses','Target Class')


#To see dataset structure 
#str(dataset)

# cleaning Data 

#Drop id col 
dataset <- dataset[ , 2:11]


#dealing with Bn Col and transform target class to 0 / 1 col 
for (value in 1:length(dataset$BN)) {
  if(dataset$BN[value] == "?")
    dataset$BN[value] = "0"
  
  # if the person don't have cancer "changing the value from 2 to 0 "
  if(dataset$`Target Class`[value] == 2)
    dataset$`Target Class`[value] = 0
  
  #person have cancer "from 4 to 1"
  if(dataset$`Target Class`[value] == 4)
    dataset$`Target Class`[value] = 1
}

dataset$BN <- as.integer(dataset$BN)



#to check 
#unique(dataset$BN)
#str(dataset$BN)


#split the data into training and test set 
#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset$`Target Class` , SplitRatio = 0.7)
training_set = subset(dataset , split = TRUE)
test_set = subset(dataset , split = FALSE)

#implement LR model 
model = glm(formula = `Target Class` ~ . ,
            family = binomial,
            data = training_set)


predictions_prop = predict(model , type = "response" , newdata = test_set[-10])

predictions = ifelse(predictions_prop >0.5 , 1 , 0)


#confusion matrix and measuring accuracy 
cm = table(test_set[,10] , predictions )
accuracy = (448+230) / (448+230+11+10) *100 
accuracy
