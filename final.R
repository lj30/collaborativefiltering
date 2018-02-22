rm(list=ls())
library(Hmisc) #to run %nin% operator
ratings=read.csv("yelp_academic_dataset_review.csv",header=TRUE)
ratings=data.frame(ratings,user_id_n=as.numeric(ratings$user_id))
businesses=read.csv("yelp_academic_dataset_business.csv",header=T)


user_based=function(input_user,method){
#sets code up as a function with the user id as an input
  
  st=Sys.time()
  #well help determine computing time at the end of the code
  
  #input_user=399
  #manually sets user id for testing
  
  users=unique(ratings$user_id_n) #identifies unique user ids in the dataset
  
  #input_user=as.character(sample(users,1))
  #randomly assigns user id for testing
  
  iterations=length(users) #calculates number of iterations for the for-loop
  sub1=subset(ratings,ratings$user_id==input_user) #subsets the ratings of businesses rated by input user
  
  value=0 #initializes vector
  index=0 #initializes vector
  distance1=0 #initializes vector
  distance2=0 #initializes vector
  distance3=0 #initializes vector
  distance4=0 #initializes vector
  
  for (i in 1:iterations){
    if (input_user != users[i]){ #runs over users that are not the selected user 
      sub2=subset(ratings,ratings$user_id==users[i]) #subsets ratings of the ith user in the loop
      same=intersect(sub1$business_id,sub2$business_id) #looks at intersection of ratings from the two user subsets
      if (identical(same,character(0))==FALSE){ #proceeds if there are common businesses in the intersection of subsets
        predist1=subset(sub1,sub1$business_id%in%same) #subsets initial user data for distance calculation
        predist2=subset(sub2,sub2$business_id%in%same) #subsets ith user data for distance calculation
        
        manhattan=sum(abs(predist1$stars-predist2$stars)) #calculates manhattan distance
        euclidean=sqrt(sum((predist1$stars-predist2$stars)^2)) #calculates euclidean distance
        if ((length(predist1$stars)>1 & length(predist2$stars)>1) == TRUE){
          pearson=cov(predist1$stars,predist2$stars,method="pearson")/(sd(predist1$stars)*sd(predist2$stars)) #calculates the pearson correlation coefficient
        }
        cosinesim=sum(predist1$stars*predist2$stars)/(sqrt(sum(predist1$stars^2))*sqrt(sum(predist2$stars^2))) #calculates the cosine similarity
        
        value=value+1 #increases value with each iteration
        index[value]=i #identifies index
        distance1[value]=method #identifies distance
      }
    }
  }
  

  finaldata=data.frame(cbind(index,distance1,distance2,distance3,distance4)) #creates dataframe of "neighbors" calculated by for-loop
  if (method == 1){
    finaldata=finaldata[order(finaldata$distance1),] #orders data from closest neighbor to farthest, based on method input into function
    prel_index=subset(finaldata,finaldata$distance1%in%finaldata$distance1[1]) #subsets the closest user(s)
  } else if (method == 2){
    finaldata=finaldata[order(finaldata$distance2),]
    prel_index=subset(finaldata,finaldata$distance2%in%finaldata$distance2[1])
  } else if (method == 3){
    finaldata=finaldata[order(finaldata$distance3,decreasing = TRUE),]
    prel_index=subset(finaldata,finaldata$distance3%in%finaldata$distance3[1])
  } else if (method == 4){
    finaldata=finaldata[order(finaldata$distance4),]
    prel_index=subset(finaldata,finaldata$distance4%in%finaldata$distance4[1])
  }
  
  l_index=prel_index[,1] #vectorizes the indices of the closest user(s)
  finaldata2=subset(ratings,ratings$user_id%in%users[l_index] & ratings$business_id%nin%sub1$business_id & ratings$stars>4) #creates dataframe of the businesses rated with 5 stars by closest users that the input user has not already rated 
  rec_rest=finaldata2$business_id #vectorizes business ids of recommended businesses
  finaldata3=subset(businesses,businesses$business_id%in%rec_rest) #creates dataframe of recommended businesses
  
  print("Business Recommendations:")
  if (dim(finaldata2)[1]==0){
    print("No Business Recommendations!")
  } else {
    print(finaldata3$name)
  }
  
  
  ft=Sys.time() #time stamp when code is finished
  print("Time Elapsed")
  print(ft-st) #prints total time code took to run
}
