library(rpart)
library(rpart.plot)
install.packages("devtools")
devtools::install_github("devanshagr/CrossValidation")

train <- Tillet.Analytics_train 
summary(train)

tree <- rpart(Party~ Social.Media + Favorite.song + Calories + Coffees, data =train)
rpart.plot(tree)
 
tree2 <-rpart(Party~ Social.Media + Favorite.song, data = train)
rpart.plot(tree2)

tree3 <- rpart(Party ~ Calories + Coffees,data = train )
rpart.plot(tree3)

tree4 <- rpart(Party~ Calories + Coffees + Favorite.song, data = train)
rpart.plot(tree4)

tree5<- rpart(Party~ Calories + Coffees + Social.Media, data = train)
rpart.plot(tree5)

CrossValidation::cross_validate(train,tree5, 5, 0.8)
prediction<- predict(tree5, newdata=Tillet_Analytics_students ,type="class")
Tillet_Submisson$Party <- prediction
write.csv(Tillet_Submisson, file = "mysubmission.csv",row.names=FALSE)
## tree 1 = 77 to 80 
## tree 2 = 69 to 80 
## tree 3 - 79 to 96 95.1% 
## tree 4 = 76 to 80 
## tree 5 = 95.3% 
