#polinomial Regression

#importing the dataset
dataset=read.csv('Position_Salaries.csv')
dataset=dataset[2:3]

#splitting the dataset to training set and test set
# library(caTools)
# set.seed(123)
# split=sample.split(data$  ,SplitRatio=0.8)
# training_set=subset(dataset,split=TRUE)
# test_set=subset(dataset,split=FALSE)

#feature Scaling 
# training_set=scale(training_set)
# test_set=scale(test_set)

#fitting linear regression to the dataset
lin_reg=lm(formula =Salary ~ ., 
           data=dataset)

#fitting polinomial regression to the dataset
dataset$Level2=dataset$Level^2
dataset$Level3=dataset$Level^3
poly_reg=lm(formula = Salary ~ .,
            data = dataset)
#visualizing linear regression model
library(ggplot2)
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),color='red')+
  geom_line(aes(x=dataset$Level,y=predict(lin_reg,newdata=dataset)),color='blue')+
  ggtitle('truth vs bluff')+
  xlab('levels')+ylab('Salary')

#visualizing the polynomial regression model
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),color='red')+
  geom_line(aes(x=dataset$Level,y=predict(poly_reg,newdata=dataset)),color='blue')+
  ggtitle('truth vs bluff')+
  xlab('levels')+
  ylab('salary')

#predict new result with linear regression 
y_pred=predict(lin_reg,data.frame(Level=6.5))

#predict new result with polinomial regression 
z_pred=predict(poly_reg,data.frame(Level=6.5,Level2=6.5^2,Level3=6.5^3))

#smooth visualizing the polynomial regression model
x_grid=seq(min(dataset$Level),max(dataset$Level),0.1)
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),color='red')+
  geom_line(aes(x=x_grid,y=predict(poly_reg,newdata=data.frame(Level=x_grid,Level2=x_grid^2,Level3=x_grid^3))),color='blue')+
  ggtitle('truth vs bluff')+
  xlab('levels')+
  ylab('salary')










