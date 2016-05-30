 library("RWeka", lib.loc="~/R/win-library/3.2")
library("RWekajars", lib.loc="~/R/win-library/3.2")
 require(RWeka)
 ml <- read.csv("C:\\Users\\Sowmya\\Desktop\\NUI GALWAY\\Machine Learning\\Assignment 3\\owls15.csv")
 
# J48(owl~X1 + X2 + X3 + X4, data = ml,control= Weka_control(W=list(J48,m=1,U=T)))
#Error in .jcall(classifier, "V", "buildClassifier", instances) : 
#  java.lang.Exception: Illegal options: -W weka.classifiers.trees.J48 -m 1 -U 
m1<-J48(owl~X1 + X2 + X3 + X4, data = ml)
#J48 pruned tree

eval_j48 <- evaluate_Weka_classifier(m1, numFolds = 10, complexity = FALSE, 
                                     seed = 1, class = TRUE)