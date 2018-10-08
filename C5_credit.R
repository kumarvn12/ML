
library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)

#$fancyRpartPlot(dtree)

#setwd("D:/AP/baging and  RF")

credit = read.csv("credit(1).csv")
names(credit)
head(credit)

prop.table(table(credit$default))

set.seed(1324)

idx = sample( nrow(credit), nrow(credit)*0.8) 

train = credit[ idx, ]
test = credit[ -idx, ]

dtree1 = rpart(default ~ ., data=train, method="class")

fancyRpartPlot(dtree1)

table(train$checking_balance)


printcp(dtree1)
dtree1


test$pred = predict(dtree1, newdata = test, type="class")

#### confusion matrix 

table( test$default, test$pred)

printcp(dtree1)


tree2 = prune( dtree1, cp =0.012448 )

fancyRpartPlot(tree2)

### performance of pruned tree 

test$pred2 = predict( tree2 , newdata = test , type="class")

table( test$default, test$pred2)

 
 ### credit default prediction using c5.0 
 #install.packages("C50")
 library(C50)
 
 ?C5.0
 
 target = credit$default
 #taking default as the target variable
 table(target)
 
 credit$default = NULL
 # removing default as input variable
 
 set.seed(1234)

 ### create index to break into train and test sets
 idx = sample(nrow(credit),  nrow(credit)*0.8)  
### trains set ( both input and target)
 train_x = credit[ idx, ]
 train_y= target[idx]
### test sets( input and target)
 test_x = credit[-idx,]
 test_y = target[-idx]

 ### model bulding udsing c5.0 
 ## C5.0 usex (x,y ) approach not the formulae 
 treec50 = C5.0(train_x, train_y )
 
summary(treec50) 

### check the accuracy on test dataset 

pred = predict(treec50, test_x )

head(pred)

### tabulate the confusion matrix 

table(test_y, pred )

#testerror = 1-(154/200)
#testerror
### Accuracy and error( misclassification rate)
(108+35)/200

### compare the train error and test error 

train_pred = predict(treec50, train_x)

table(train_y, train_pred)

trainerror = 1-( (551+146)/800)

### Precision 
p = 35/(35+24)
p 

### Recall 
r = 35/(35+33)
r
### f-1 score 
f_1 = 2 * p * r /( p + r )

