#Reading the data from csv file
ml <- read.csv("C:\\Users\\Sowmya\\Desktop\\NUI GALWAY\\Machine Learning\\Assignment 3\\owls15.csv")

#Running for 10 times

for(i in 1:10){
  
  #Dividing the data into 2/3 and 1/3 for training and testing
  
index <- 1:nrow(ml)
trainindex <- sample(index, trunc(length(index)*(2/3)))
training_data <- ml[trainindex, ]
testing_data <- ml[-trainindex, ]


#Funtion to find Information Gain
Infogain<- function(x){
  X4<-sort(x)
  
  Y<-NULL
  for(i in 1:length(X4)-1){
    Y[i]<-X4[i+1]-X4[i]
  }
  
  #Finding the Threshold value
  #Sorting the attributes to find Threshold value
  
  #Threshold for  X4
  Threshold<-X4[which.max(Y)]
  
  #Count of LongEaredOwl(LEO), SnowyOwl(SO), BarnOwl(BO)
  
  Length_LEO<-length(which(training_data$owl=="LongEaredOwl"))
  Length_SO<-length(which(training_data$owl=="SnowyOwl"))
  Length_BO<-length(which(training_data$owl=="BarnOwl"))
  Length_Owl<-length(training_data$owl)
  
  #Calculating Entropy E(S) = -Plog(P)
  Entropy_S <- (-(Length_LEO/Length_Owl)*(log2(Length_LEO/Length_Owl)))+
    (-(Length_SO/Length_Owl)*(log2(Length_SO/Length_Owl)))+
    (-(Length_BO/Length_Owl)*(log2(Length_BO/Length_Owl)))
  
  #Calculating Entropy X4<=Threshold  E(X4)= (-P_LOlog(P_LO))+(-P_BOlog(P_BO))+(-P_SOlog(P_SO))
  
  X4_ThLess<-which(training_data$X4<=Threshold)
  Count_Less<-length(X4_ThLess)
  
  Count_LO<-0
  Count_SO<-0
  Count_BO<-0
  
  for(i in 1:length(x)){
    if(x[i]<=Threshold){
      if(training_data$owl[i]=="LongEaredOwl"){Count_LO=Count_LO+1}else
      {if(training_data$owl[i]=="SnowyOwl"){Count_SO=Count_SO+1}else
      {Count_BO=Count_BO+1}}
      
    }}
  
  #Calculating Entropy
  S1<-0
  S2<-0
  S3<-0
  if(Count_LO!=0){S1<-(Count_LO/Count_Less)*(log2(Count_LO/Count_Less))}
  if(Count_SO!=0){S2<-(Count_SO/Count_Less)*(log2(Count_SO/Count_Less))}
  if(Count_BO!=0){S3<-(Count_BO/Count_Less)*(log2(Count_BO/Count_Less))}
  
  Entropy_Less<-(-S1)+(-S2)+(-S3)
  
  #Calculating Entropy X4<in between>Threshold  E(X4)= (-P_LOlog(P_LO))+(-P_BOlog(P_BO))+(-P_SOlog(P_SO))
  
  subset_index<-which.max(Y)
  Threshold_median <- X4[((subset_index + 1) + length(X3))/2]
  count_l<-count_middle<-count_s<-count_b<-0
  for(i in 1:length(x)){
    if(x[i]>Threshold && x[i]<=Threshold_median){
      {
        count_middle<-count_middle+1
        if(training_data$owl[i]=="LongEaredOwl"){
          count_l=count_l+1
        }else if(training_data$owl[i]=="SnowyOwl"){
          count_s=count_s+1
        }else if(training_data$owl[i]=="BarnOwl"){
          count_b=count_b+1
        }
      }
    }
  }
  E1<-ifelse(count_l==0,0,(count_l/count_middle)*(log2(count_l/count_middle)))
  E2<-ifelse(count_s==0,0,(count_s/count_middle)*(log2(count_s/count_middle)))
  E3<-ifelse(count_b==0,0,(count_b/count_middle)*(log2(count_b/count_middle)))
  Entropy_Middle<- -E1-E2-E3
  
  
  #Calculating Entropy X4>Threshold  E(X4)= (-P_LOlog(P_LO))+(-P_BOlog(P_BO))+(-P_SOlog(P_SO))
  
  
  
  count_l<-count_more<-count_s<-count_b<-0
  for(i in 1:length(x)){
    if(x[i]>Threshold_median){
      {
        count_more<-count_more+1
        if(training_data$owl[i]=="LongEaredOwl"){
          count_l=count_l+1
        }else if(training_data$owl[i]=="SnowyOwl"){
          count_s=count_s+1
        }else if(training_data$owl[i]=="BarnOwl"){
          count_b=count_b+1
        }
      }
    }
  }
  E1<-ifelse(count_l==0,0,(count_l/count_more)*(log2(count_l/count_more)))
  E2<-ifelse(count_s==0,0,(count_s/count_more)*(log2(count_s/count_more)))
  E3<-ifelse(count_b==0,0,(count_b/count_more)*(log2(count_b/count_more)))
  Entropy_greater<- -E1-E2-E3
  
  
  Information_gain <- Entropy_S - ((Count_Less/Length_Owl)*(Entropy_Less)) -
    ((count_middle/Length_Owl)*(Entropy_Middle)) - 
    ((count_more/Length_Owl)*(Entropy_greater))
  return(Information_gain)
}


#Calling functions to calculate Information gain for 4 attributes

GainX4<-Infogain(training_data$X4)
GainX3<-Infogain(training_data$X3)
GainX2<-Infogain(training_data$X2)
GainX1<-Infogain(training_data$X1)

#To find the the attribute with highest Informain gain

Gain<-c(GainX1,GainX2,GainX3,GainX4)
name<-names(ml)
Information_Gain_Max_1<-name[which.max(Gain)]

#Second Iteration depending on the Information gain: X4 is higher , hence X4 is the root node
#Subsetting the data for those values of X4 greater than the Threshold

Second_Set<-subset(training_data, training_data$X4 > Threshold)

#Finding the second Threshold

Threshold2<- median(Second_Set$X4)

# Calculating Information gain for the second iteration

InfoGain <-function(x){
   count_l<-count_total_greater<-count_s<-count_b<-0
  for(i in 1:length(x)){
    if(x[i]>Threshold){
      {
        count_total_greater<-count_total_greater+1
        if(training_data$owl[i]=="LongEaredOwl"){
          count_l=count_l+1
        }else if(training_data$owl[i]=="SnowyOwl"){
          count_s=count_s+1
        }else if(training_data$owl[i]=="BarnOwl"){
          count_b=count_b+1
        }
      }
    }
  }
  E1<-ifelse(count_l==0,0,(count_l/count_total_greater)*(log2(count_l/count_total_greater)))
  E2<-ifelse(count_s==0,0,(count_s/count_total_greater)*(log2(count_s/count_total_greater)))
  E3<-ifelse(count_b==0,0,(count_b/count_total_greater)*(log2(count_b/count_total_greater)))
  entropy_greater<--E1-E2-E3
  
  Total_Owl<-length(training_data$owl)
  Total_L<-length(which(training_data$owl=="LongEaredOwl"))
  Total_S<-length(which(training_data$owl=="SnowyOwl"))
  Total_B<-length(which(training_data$owl=="BarnOwl"))
  #Calculating Entropy E(S) = -Plog(P)
  Entropy_S <- (-(Total_S/Total_Owl)*(log2(Total_S/Total_Owl)))+
    (-(Total_B/Total_Owl)*(log2(Total_B/Total_Owl)))
  
  #Calculating Entropy X4<in between>Threshold  E(X4)= (-P_LOlog(P_LO))+(-P_BOlog(P_BO))+(-P_SOlog(P_SO))
  
  count_l<-count_total_middle<-count_s<-count_b<-0
  for(i in 1:length(x)){
    if(x[i]>Threshold & x[i]<Threshold2){
      {
        
        count_total_middle<-count_total_middle+1
        if(training_data$owl[i]=="LongEaredOwl"){
          count_l=count_l+1
        }else if(training_data$owl[i]=="SnowyOwl"){
          count_s=count_s+1
        }else if(training_data$owl[i]=="BarnOwl"){
          count_b=count_b+1
        }
      }
    }
  } 
  
  E1<-ifelse(count_l==0,0,(count_l/count_total_middle)*(log2(count_l/count_total_middle)))
  E2<-ifelse(count_s==0,0,(count_s/count_total_middle)*(log2(count_s/count_total_middle)))
  E3<-ifelse(count_b==0,0,(count_b/count_total_middle)*(log2(count_b/count_total_middle)))
  entropy_middle<--E1-E2-E3
  
  
  
  #entropy of x4 for thres1hold greater than situation
  count_l<-count_total_lesser<-count_s<-count_b<-0
  for(i in 1:length(x)){
    if(x[i]<=Threshold){
      {
        count_total_lesser<-count_total_lesser+1
        if(training_data$owl[i]=="LongEaredOwl"){
          count_l=count_l+1
        }else if(training_data$owl[i]=="SnowyOwl"){
          count_s=count_s+1
        }else if(training_data$owl[i]=="BarnOwl"){
          count_b=count_b+1
        }
      }
    }
  }
  E1<-ifelse(count_l==0,0,(count_l/count_total_lesser)*(log2(count_l/count_total_lesser)))
  E2<-ifelse(count_s==0,0,(count_s/count_total_lesser)*(log2(count_s/count_total_lesser)))
  E3<-ifelse(count_b==0,0,(count_b/count_total_lesser)*(log2(count_b/count_total_lesser)))
  entropy_lesser<--E1-E2-E3
  
  #Information Gain
  Information_Gain_2<-Entropy_S-(count_total_greater/length(Second_Set$owl))*entropy_greater-(count_total_lesser/length(Second_Set$owl))*entropy_inbet-(count_total_inbet/length(Second_Set$owl))*entropy_inbet
  return(Information_Gain_2)
}





GainX1<-InfoGain(Second_Set$X1)
GainX2<-InfoGain(Second_Set$X2)
GainX3<-InfoGain(Second_Set$X3)
GainX4<-InfoGain(Second_Set$X4)


#To find the the attribute with highest Informain gain

Gain<-c(GainX1,GainX2,GainX3,GainX4)
name<-names(ml)
Information_Gain_Max_2<-name[which.max(abs(Gain))]

#Third Iteration depending on the Information gain: X4 is higheragain , hence X4 is the next node
#Subsetting the data for those values of X3 greater than the Threshold3 and X4 less than Threshold 2


Threshold3<-median(Second_Set$X3)
Third_Subset<-subset(training_data, training_data$X4>Threshold & training_data$X4 <= Threshold2)

InfoGain<-function(x){
  count_l<-count_total_greater<-count_s<-count_b<-0
  for(i in 1:length(x)){
    if(x[i]>Threshold3){
      {
        count_total_greater<-count_total_greater+1
        if(training_data$owl[i]=="LongEaredOwl"){
          count_l=count_l+1
        }else if(training_data[i]=="SnowyOwl"){
          count_s=count_s+1
        }else if(training_data[i]=="BarnOwl"){
          count_b=count_b+1
        }
      }
    }
  }
  E1<-ifelse(count_l==0,0,(count_l/count_total_greater)*(log2(count_l/count_total_greater)))
  E2<-ifelse(count_s==0,0,(count_s/count_total_greater)*(log2(count_s/count_total_greater)))
  E3<-ifelse(count_b==0,0,(count_b/count_total_greater)*(log2(count_b/count_total_greater)))
  entropy_greater<--E1-E2-E3
  
  Total_Owl<-length(training_data$owl)
  Total_L<-length(which(training_data$owl=="LongEaredOwl"))
  Total_S<-length(which(training_data$owl=="SnowyOwl"))
  Total_B<-length(which(training_data$owl=="BarnOwl"))
  
  
  #Calculating Entropy E(S) = -Plog(P)
  Entropy_S <- (-(Total_L/Total_Owl)*(log2(Total_L/Total_Owl)))+
    (-(Total_S/Total_Owl)*(log2(Total_S/Total_Owl)))+
    (-(Total_B/Total_Owl)*(log2(Total_B/Total_Owl)))
  
  
  
  
  #entropy of x for Threshold greater than X3 situation
  count_l<-count_total_lesser<-count_s<-count_b<-0
  for(i in 1:length(x)){
    if(x[i]<=Threshold3){     
      {
      count_total_lesser<-count_total_lesser+1
      if(training_data$owl[i]=="LongEaredOwl"){
        count_l=count_l+1
      }else if(training_data$owl[i]=="SnowyOwl"){
        count_s=count_s+1
      }else if(training_data$owl[i]=="BarnOwl"){
        count_b=count_b+1
      }
    }
    }
  }
  E1<-ifelse(count_l==0,0,(count_l/count_total_lesser)*(log2(count_l/count_total_lesser)))
  E2<-ifelse(count_s==0,0,(count_s/count_total_lesser)*(log2(count_s/count_total_lesser)))
  E3<-ifelse(count_b==0,0,(count_b/count_total_lesser)*(log2(count_b/count_total_lesser)))
  entropy_lesser<--E1-E2-E3
  
  #Information Gain
  IG<-Entropy_S-(count_total_greater/length(training_data$owl))*entropy_lesser-(count_total_lesser/length(training_data$owl))*entropy_lesser
  return(IG)
}




GainX1<-InfoGain(Third_Subset$X1)
#GainX2<-InfoGain(Third_Subset$X2)
#GainX3<-InfoGain(Third_Subset$X3)
GainX4<-InfoGain(Third_Subset$X4)


#To find the the attribute with highest Informain gain

Gain<-c(GainX1,GainX2,GainX3,GainX4)
name<-names(ml)
Information_Gain_Max_3<-name[which.max(abs(Gain))]

#Fourth Subset

Fourth_Subset<-subset(training_data,  training_data$X4<=Threshold2 & training_data$X3 > Threshold3)

Threshold4<-median(Fourth_Subset$X4)

}

#Predicting the testing samples

Prediction<-function(testing_data)
{
  Predict<-NULL
  for(i in 1:nrow(testing_data))
  {
    if(testing_data$X4[i]<=Threshold){
      Predict[i]<-"LongEaredOwl"
    }else if(testing_data$X4[i]>Threshold & testing_data$X4[i]>Threshold2){
      Predict[i]<-"SnowyOwl"
    }else if(testing_data$X4[i]>Threshold & testing_data$X4[i]<=Threshold2){
      if(testing_data$X3[i]<=Threshold3){
        Predict[i]<-"BarnOwl"
      }else if(testing_data$X4[i]>Threshold3 & testing_data$X4[i]>Threshold4){
        Predict[i]<-"BarnOwl"
      }else if(testing_data$X4[i]>Threshold3 & testing_data$X4[i]<=Threshold4){
        Predict[i]<-"SnowyOwl"
      }
    }
  }
  return(Predict)
}
Predict<-Prediction(testing_data)

ConfusionMatrix<-table(testing_data$owl,Predict)
