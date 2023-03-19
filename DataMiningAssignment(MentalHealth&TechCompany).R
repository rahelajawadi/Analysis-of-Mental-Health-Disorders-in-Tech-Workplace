setwd("C:/Users/DELL/Documents/statistic")
x = read.csv("grades1.csv")
View(x)

install.packages("arules")
install.packages("arulesViz")

#Problem: predicting/classifing whether a new students' grade goes to A or Not A class, using their midterm and Quizz scores

                             ####Linear Regression####

#The classification/Binary attribute in this case is grade
#Construct a model to classify the student grade to A or Not A based on two features (Midterm and Quiz) score
#Spiliting the data into train and test datasets
#For that we check the number of observation we have in our dataset
dim(x)

nrow(x)
0.8*117
s=sample(nrow(x), 93)

#Traning data for constrcting the model
x.train=x[s,]
x.train

#Testing data for checking the performance of the model using new data
x.test=x[-s,]
x.test
View(x.test)
View(x.train)

#Constructing the model 0 
model0= lm(Grade~MT, data =x.train )
lm0.test=predict(model0, x.test)

#Constructing the model 1
modelOne= lm(Grade~Q, data =x.train )
lm1.test=predict(modelOne, x.test)
lm1Test = ifelse(lm1.test>0.5,1,0)
lm1Test

#Checking/ comparing the actual grades with the predicted one 
lmTest = ifelse(lm0.test>0.5,1,0)

# The predicted value of Grade in test data
lmTest
x.test$Grade

#Checking with the new data
mt.new = data.frame(Q=c(100,70,98))
mt.new
lm1.test=predict(modelOne, mt.new)
lm1.test

#Checking the error size with the use of confusion matrix
table(x.test$Grade, lmTest)
table(x.test$Grade, lm1Test)
#Based on the result of the confussion matrix is 18/24

# The error rate, which is 0.75 -> 75 %
18/nrow(x.test)

#Since for both model0 and model one the error rate is the same, we can select one of the models
#The probablity of wrong prediction is very high in this two models

                         ####Logistic Regressions####

#The first step is to specify the classification attribure y and the features x
# Grades will work as the y attribute and Midterm and Quiz score as x features

# Now, we spilit the dataset into test and train dataset 
nrow(x)
# Since we have already did that, we can use the x.test and x.train from above
x.test
x.train

# Building the model by using the train data
l0_mod0= glm(Grade~MT, family = "binomial", data = x.train)

#Checking the predicted probablity 
m0.train.pl= fitted(l0_mod0, x.train)
plot(x.train$MT, m0.train.pl) # After plotting the output of logistic regression, we knew that the main 
#difference it has with the linear regression is the fact that all the values 
# predicted from the logistic reg remain between 0-1, That is consistent with the defination of probablity 

#testing the constructed model using our test data
l0_mod0_pre= predict(l0_mod0, x.test)

# Creating the dummy varaible 
cl0_pre= ifelse(l0_mod0_pre>0.5,1,0)

#Checking the error rate
table(x.test$Grade, cl0_pre) # The error rate of model 0 is:75%

#Constructing model one
l1_mod1= glm(Grade~Q, family = "binomial", data = x.train)
l1_mod1_pre= predict(l1_mod1, x.test)

# Creating the dummy varaible 
cl1_pre= ifelse(l1_mod1_pre>0.5,1,0)

#Checking the error rate
table(x.test$Grade, cl1_pre) # The error rate is 8/24 
8/nrow(x.test) # The error rate of model 1 is 33%
#Since the error rate rate of model 1 is smaller we will select this model as the best model

# Checking model one with new data/ since model one error rate is smaller
Q.new = data.frame(Q= c(99,100,67))
cl_pre_new= predict(l1_mod1, Q.new)
cl_pre_new
#Bases on the output the two first students will get an A, while the last one won't

#changing to Binary varaible 
Grade_new_class = ifelse(cl_pre_new>0.5, 1,0)
Grade_new_class

                  ####KNN####
#First, creating the predictor matrix and classification matrix
#Spiliting the dataset to smaller dataset by picking only the columns that we pick as x and y attibutes
#coulmn three is the MT and four is the Quiz column
x.new=x[,c(3,4)]

#column number 8 is the grade, which is our classification attributes
x.group=x[,8]

#Split the dataset/ picking 80% as train and 20% as test data
nrow(x)*0.8

s=sample(nrow(x), 93)
x.train=x.new[s, ]
x.test=x.new[-s, ]

#Limiting the data to the only the classification attributes y and x feature (midterm and quiz)
train.group=x.group[s]
test.group=x.group[-s]


#Specify k, the k value better to be an odd number, in this case we use 11 knn to predict the class of our attributes
sqrt(nrow(x)) 

library(class)

#running the KNN model and using the model in test for predicting y 
x.knn=knn(train = x.train, test = x.test, cl = train.group, k=11)

# Checking the actual grade with the predicted one
x_pr=data.frame(x.test$MT, test.group, x.knn)
View(x_pr)

#Construct the Confusion matrix for the checking the number of errors
table(test.group, x.knn)

error.knn= 3/NROW(test.group) # The error rate is 12%
error.knn

#Testing the model with new data 
MT=c(98, 70, 60, 60)
Q=c(100, 89, 73, 80)

#where Knn would classify them 
pre.new=data.frame(cbind(MT, Q))
pre.new

# The predicted model that classifies the new data 
x.knn_new=knn(train = x.train, test = pre.new, cl = train.group, k=11)
View(x.knn_new)
table(x.knn_new)
# Based on the prediction model the first observation will be classified as A and the three remainings will be 
# graded as Not A

                             ####ANN####
library(neuralnet)

#Narrowing down the dataset into smaller datasets, containing only the varaibles we are working on it for classifing
x=data.frame(x$MT, x$Q, x$Grade)
colnames(x)=c("MT", "Q","Grade")
View(x)

#Split the dataset x into training and test
nrow(x)*0.8
s=sample(nrow(x), 93)
x.train=x[s, ]
# In this case the -3 is used to exclude the third column, which is our classification variable (grade)
x.test=x[-s, -3]

# Here we store the classification var column which is grade
x.test.y=x[-s, 3]
head(x.test)
head(x.test.y)

#Running the ANN classification Model, Here the model work on the input we provide
#And will provide us output, whcih this output is  
nnM=neuralnet(Grade~MT+Q, data =x.train, hidden = 3, act.fct = "logistic", linear.output = FALSE)
plot(nnM)

##Compute the error rate using test dataset 
pr.test=compute(nnM, x.test)

p1=pr.test$net.result
head(p1)
pred=ifelse(p1>0.5, 1, 0) #dummy/binary variable

table(x.test.y, pred) #confussion matrix

Err.rate=4/nrow(x.test)
Err.rate # The error rate was 0.166 -> 16%

#Use the ANN model to classify new students
MT=c(94, 90, 60)
Q=c(97, 50, 70)

#Storing the new data into a new dataframe
new.stu=data.frame(cbind(MT, Q))
head(new.stu)
pr.test=compute(nnM, new.stu)

p1=pr.test$net.result

pred0=ifelse(p1>0.5, 1, 0)  #dummay/Binary variable
pred0
#The model predict the first student as A and the two others as Not A

#Building another model using the MT score alone as the x feature
nnM_Mid=neuralnet(Grade~MT, data =x.train, hidden = 2,act.fct = "logistic", linear.output = FALSE)
plot(nnM_Mid)

#Finding out the error rate of Mideterm model
pr.test=compute(nnM_Mid, x.test)

p2=pr.test$net.result
head(p2)
pred_Mid=ifelse(p2>0.5, 1, 0)  #dummy/Binary variable
between the
table(x.test.y, pred_Mid)   #confussion matrix for finding out the number of wrong prediction

Err.rate=6/nrow(x.test)
Err.rate # The error rate was 0.25 -> 25% ---> Since the error rate of this model is bigger than the first one, we will select that model

#Building another model using the Quiz score alone as the x feature
nnM_Q=neuralnet(Grade~Q, data =x.train, hidden = 2,act.fct = "logistic", linear.output = FALSE)
plot(nnM_Q)

#hidden is the number of neuran or layers  initial input and the final output

#Finding out the error rate of Mideterm model
Q_pr.test=compute(nnM_Q, x.test)

p3=Q_pr.test$net.result
head(p3)
# predicted class of the test dataset
pred_Q=ifelse(p3>0.5, 1, 0)  #dummy/Binary variable

x.test.y #actual class of the test dataset
table(x.test.y, pred_Q)   #confussion matrix for finding out the number of wrong prediction

Err.rate=6/nrow(x.test)
Err.rate # The error rate for this model was also 0.25 -> 25% ---> Since the error rate of this model is bigger than the first one, we will select that model



                       ####Decison Tree#####

#Again, first of all, we specifiy our classifiaction attribute/binary variable as well as the vector of x features
# In this case, students' grade work as y and students' MT and Q score as x features
#Decision tree check which features are playing a key rule in specifying the value of Y. 

#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#Using the test and train data using above 
x.test=x[-s, ]
View(x.test)
x.train=x[s, ]
View(x.train)

#Building another model for the MT score alone
fit0=rpart(Grade~MT, data = x.train, method = "class")
rpart.plot(fit0, extra = 106)

pred_MT=predict(fit0, x.test, type = "class")
table(x.test$Grade, pred_MT)

Err.rate_MT=6/NROW(x.test) # the error rate for this model is 0.25 which is 25%
Err.rate_MT

#Building another model for the Quiz alone
fit1=rpart(Grade~Q, data = x.train, method = "class")
rpart.plot(fit1, extra = 106)

pred_Q=predict(fit1, x.test, type = "class")
table(x.test$Grade, pred_Q)

fit=rpart(Grade~., data = x.train, method = "class")
rpart.plot(fit, extra = 106)

pred=predict(fit, x.test, type = "class")
table(x.test$Grade, pred)

Err.rate=4/NROW(x.test) # the error rate for this model is 0.16 which is 16%
Err.rate

#Testing the model with new data for predicting their grade as A or Not A 
MT=c(94, 70, 90)
Q=c(90, 65, 50)

new.stu=data.frame(cbind(MT, Q))
pred.new=predict(fit, new.stu, type = "class")
pred.new

#The model predicts the first student as A and the two others an Not A

                              ### Comparison### 
# To compare all these classification methods, I believe one can not say that one method
# is superior than other methods. Since each method has its own strenght and weaknesses based on the type and size of data
# However, based on the outcomes that I come up with from these methods while the classification/Binary attribute and x attribute for all of them 
# was the same, the outcomes of the KNN method (the way it classified the new students' grade) was more satisfing.
# As in this method the error rate was less than other methods. Also, while comparing the actual class and predicted class of data there were few wrong predctions.

#Then, the decison tree method was very intresting for me, the way it provides the graphical representtion with the percenatge of probabity of each class
#At the same time the error rate was not that high as other methods like the linear classification method

#Finally, the artificial neural network (ANN) method was the challenging method for me, since it was new to me
#Also, the inner calcualtion of the layers from the initial input up to the final output was distracting as in some
#plots it showed around 4000 inner steps or calcualtion that was performed by the neuralnet function. Fortunately, we were dealing only with the initial input and the final output