library(rpart)
library(rpart.plot)
install.packages("devtools")
devtools::install_github("devanshagr/CrossValidation")

summary(M2019btrain)

tree <- rpart(GRADE ~ SCORE + ASKS_QUESTIONS + PARTICIPATION + LEAVES_EARLY, data = M2019btrain,control=rpart.control(minsplit = 50, minbucket = 50)) 
rpart.plot(tree)

tree2 <- rpart(GRADE ~ SCORE + ASKS_QUESTIONS + LEAVES_EARLY,data = M2019btrain, control = rpart.control(minsplit =50))
rpart.plot(tree2)

tree3 <- rpart(GRADE ~ SCORE + ASKS_QUESTIONS + PARTICIPATION,data = M2019btrain, control = rpart.control(minsplit =50))
rpart.plot(tree3)

CrossValidation::cross_validate(M2019btrain,tree3, 1, 0.8)

prediction<- predict(tree3, newdata=M2019_test_students ,type="class")
M2019_sample_submission$GRADE <- prediction
write.csv(M2019_sample_submission, file = "mysubmission.csv",row.names=FALSE)