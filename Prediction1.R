boxplot(M2019btrain$SCORE ~ M2019btrain$GRADE)

AGrade <-subset(M2019btrain,M2019btrain$GRADE=="A")
# mean is 89.87
mean(AGrade$SCORE)

BGrade <-subset(M2019btrain,M2019btrain$GRADE=="B")
# mean is 76.07 
mean(BGrade$SCORE)

CGrade <-subset(M2019btrain,M2019btrain$GRADE=="C")
## mean for C is 63.96 
mean(CGrade$SCORE)

DGrade <- subset(M2019btrain,M2019btrain$GRADE=="D")
#mean for D 53.571 
mean(DGrade$SCORE)

## Bgrade lower half 
Blower <-subset(BGrade,BGrade$SCORE < 76.07 )
#breaking down the attributes to see how many of everything 
#28
BlowerAskQuestionAlways <- subset(Blower,Blower$ASKS_QUESTIONS == "always")
#17
BlowerAskQuestionSometimes <- subset(Blower,Blower$ASKS_QUESTIONS == "sometimes")
#35
BlowerAskQuestionNever <- subset(Blower,Blower$ASKS_QUESTIONS == "never")


#22
BlowerLeavesAlways <- subset(Blower,Blower$LEAVES_EARLY == "always")
#1
BlowerLeavesSometimes <- subset(Blower,Blower$LEAVES_EARLY == "sometimes")
#27
BlowerLeavesRarely <- subset(Blower,Blower$LEAVES_EARLY == "rarely")








# real code happens here 
myprediction<-M2019_test_students

#allocation of points for asking 
askPoints <- rep(0,nrow(myprediction))
askPoints[myprediction$ASKS_QUESTIONS == "always"] <- 0
askPoints[myprediction$ASKS_QUESTIONS == "sometimes"] <- -3
askPoints[myprediction$ASKS_QUESTIONS == "never"] <- -10
myprediction$numAsk<- askPoints

#allocation of points for staying or leaving 
attendancePoints <- rep(0,nrow(myprediction))
attendancePoints[myprediction$LEAVES_EARLY == "always"] <- -5
attendancePoints[myprediction$LEAVES_EARLY == "rarely"] <- -1
attendancePoints[myprediction$LEAVES_EARLY == "never"] <- 0
myprediction$numLeave<- attendancePoints

#participation points 
attendancePoints <- rep(0,nrow(myprediction))
attendancePoints[myprediction$PARTICIPATION > .9] <- myprediction$PARTICIPATION * 9 + 5 
attendancePoints[myprediction$PARTICIPATION > .8] <- myprediction$PARTICIPATION * 8 + 4 
attendancePoints[myprediction$PARTICIPATION > .7] <- myprediction$PARTICIPATION * 8 + 3
attendancePoints[myprediction$PARTICIPATION > .6] <- myprediction$PARTICIPATION * 6 + 2 
attendancePoints[myprediction$PARTICIPATION > .5] <- myprediction$PARTICIPATION * 5 + 1
attendancePoints[myprediction$PARTICIPATION > .4] <- myprediction$PARTICIPATION * -1
attendancePoints[myprediction$PARTICIPATION > .3] <- myprediction$PARTICIPATION * -2
attendancePoints[myprediction$PARTICIPATION > .2] <- myprediction$PARTICIPATION * -3
attendancePoints[myprediction$PARTICIPATION > .1] <- myprediction$PARTICIPATION * -4
myprediction$numAttend <- attendancePoints

##my prediction 
decision <- rep('F',nrow(myprediction))

#transform add to prediction 
myprediction <- transform(myprediction, final = SCORE + numAsk + numLeave + numAttend)

decision[myprediction$final>40] <- 'D'
decision[myprediction$final>45] <- 'C'
decision[myprediction$final>62] <- 'B'
decision[myprediction$final>70] <- 'A'
myprediction$GRADE <- decision
M2019_sample_submission$GRADE <- decision

#CrossValidation 
train <- M2019btrain[sample(1:nrow(M2019btrain)), ]
training<-train[1:100,]
testing<-train[101:nrow(train),]
error <- mean(M2019btrain$GRADE!= myprediction$GRADE)

M2019_sample_submission$GRADE <- decision
write.csv(M2019_sample_submission, file = "mysubmission.csv",row.names=FALSE)
