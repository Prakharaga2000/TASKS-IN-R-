#importing the data
mydata <- read_excel("C:/Users/prakhar/OneDrive/Desktop/DATA.xlsx")
mydata
#to check the descriptive statistics of the data
summary(mydata)
#to find the structure of the data
str(mydata)
#It is a data frame
#draw a simple scatter plot between hours and scores 
plot(Hours,Scores,data = mydata ,main= "Scatter plot")
#From the graph it is clear that there is a linear realationship between the variables 
#to we will divide our data into test and training set 
training<-sample(1:nrow(mydata),.7*nrow(mydata),replace = FALSE)
train<-mydata[training,]
test<-mydata[-training,]
test
#now we will make our linear model using the training data 
#Scores as dependent variable 
#Hours as independent variable
model<-lm(Scores~Hours,data= train)
#now we will predict the values using this model on the test dataset
prediction<-predict(model,test[,1])
#we will make the data frame with both actual values and predicted values from the test data set
comparison<-data.frame(Original=test[,2],Predicted = prediction)
comparison
#we can see our original values and predicted values does not vary much so our model's predictions are statistically correct
#fit the model to the graph
abline(model)
#to evaluate the model
summary(model)
#value of R^2 come to be 94.76% so our model is a very good model
#To predict the scores when the student studies for 9.25hrs/day
predictedvalue<-predict(model,data.frame(Hours=9.25))
predictedvalue
#The student will score 95.2752 marks if he studies for 9.25hrs/day.