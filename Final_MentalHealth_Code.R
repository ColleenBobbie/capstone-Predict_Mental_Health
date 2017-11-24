#-----
#Install Packages
#-----

  library(ggplot2)
  library(gridExtra)
  library(Hmisc)
  library(reshape2)
  library(DMwR)
  library(caret)
  library(pROC)
  library(RWeka)
  library(corrplot)
  library(mice)
  library(VIM)
  library(lattice)
  library(plotROC)
  library(pROC)


#-----
#Load dB 
  #This will automatically load your data based on the file path given
#----

  readfile <- function()
  { 
  n <- readline(prompt="Enter the file path for the BRFSS data: ")
  return(n)
  }
  
  mydata=sasxport.get((readfile()))
  
#----
#Data Scrubbing and Preprocessing
#---
  #rename dataframe to prevent overwriting the original file, and identify the column names
    mydataclean=mydata
    names(mydataclean)=(contents(mydata))$contents$Labels
    
  #remove the 'Record Identification' records from the dB (unnecessary for downstream analysis)
    mydatacleanremove=mydataclean[,-c(1:26)]
  
  #count NAs in Data, replace 88, 7, 9 in the data with NAs (see CodeBook Link in final report)
    
    na_count <-sapply(mydatacleanremove, function(y) sum(length(which(is.na(y)))))
    na_count <- data.frame(na_count)
    na_count
    
    mydatacleanremovenum=as.data.frame(mydatacleanremove)
    mydatacleanremovenum[] <- lapply(mydatacleanremove[], as.numeric)
    mydatacleanremovenum[mydatacleanremovenum == 88] <- NA
    mydatacleanremovenum[mydatacleanremovenum == 7] <- NA
    mydatacleanremovenum[mydatacleanremovenum == 9] <- NA
    
    sum(is.na(mydatacleanremovenum))
  
  #replace all NA values with means
    
    for(i in 1:ncol(mydatacleanremovenum)){
      mydatacleanremovenum[is.na(mydatacleanremovenum[,i]), i]=mean(mydatacleanremovenum[,i], na.rm=TRUE)
    }
    
    sum(is.na(mydatacleanremovenum))
    
    
    for(i in 1:ncol(mydatacleanremovenum)){
      if(sum(is.na(mydatacleanremovenum[,i]))){print(sum(is.na(mydatacleanremovenum[,i])))
        print(names(mydatacleanremovenum[i]))}
    }
    
  #remove attributes that have no clinical relavance, such as calculated values, and ensure there are no NAs in the data
    
    columnsnotneeded=c(2,3,15,23,30,31,32,50,52,55:57,59,64,67,69,71,77,78,82,83,85,92:95,97,103,107,135:139,155,157,159,160161,164,165,167:170,177:249)
    mydatacleanremovenum=mydatacleanremovenum[,-columnsnotneeded]
    sum(is.na(mydatacleanremovenum))
 
  
  #Attribute exploration
    
    #How many Nos arein the mental crises attribute? Yeses?
      nos=(mydatacleanremovenum[17]==2)
      sum(nos)
      
      yes=(mydatacleanremovenum[17]==1)
      sum(yes)
    
    #Keep everything that isn't NA
      mentalcrisesrough=mydatacleanremovenum
      mentalcrisesdf=mentalcrisesrough[!is.na(mentalcrisesrough[17]),]
    
    #explore the mental health dataset, and move the class attribute to the last column (necessary for downstream analyses)
      dim(mentalcrisesdf)
      sum(is.na(mentalcrisesdf))
      depressive=mentalcrisesdf[17]
      colnames(depressive)="depressive"
      mentalcrisesdf=data.frame(mentalcrisesdf, depressive)
      mentalcrisesdf=mentalcrisesdf[,-c(17)]

    #Explore how many Nos and Yeses and NAs are in the dataset (remember - NAs were replaced by the mean)
      print(prop.table(table(mentalcrisesdf$depressive)))
      
    #Remove all NAs
      
      mentalcrisesdf=mentalcrisesdf[!(mentalcrisesdf$depressive=="1.82412113226952"),]
      print(prop.table(table(mentalcrisesdf$depressive)))
      
    #Subsample the dataframe (this part can be skipped, but was done due to computation limitations)
      set.seed(123)
      mentalcrisesdfsmall = sample(1:nrow(mentalcrisesdf), size=0.1*nrow(mentalcrisesdf))
      mentalsmall = mentalcrisesdf[mentalcrisesdfsmall,]
      dim(mentalsmall)
      prop.table(table(mentalsmall$depressive))
          #Note the Yes/No ratio is approximately the same in the full vs small dataset, suggesting the smaller dataset
          #is representative of the whole.
      
  #SMOTE
      #Synthetic Minority Over-sampling Technique allows us to balance the class design, eliminating any bias that might hinder
      #our downstream analyses.

        mentalsmall$depressive = as.factor(mentalsmall$depressive)
        mentalsmall_smote=SMOTE(depressive~., data=mentalsmall, k=5, perc.over=110)
        table(mentalsmall_smote$depressive)
        levels(mentalsmall_smote$depressive)=c("Yes", "No")
        sum(mentalsmall_smote$depressive=="Yes")
        sum(mentalsmall_smote$depressive=="No")
        View(mentalsmall_smote)
        dim(mentalsmall_smote)
        
      #FIGURE
      #compare the number of classes before and after SMOTE
        
        levels(mentalsmall_smote$depressive)=c("Yes", "No")
        smote_graph=ggplot(mentalsmall_smote, aes(depressive, fill=depressive))+geom_bar()+theme_classic(base_size = 22)+scale_fill_manual(values=c("midnightblue","firebrick4"))+guides(fill=F)+
          ylab("Number of samples")+xlab("")+ggtitle("B")
        
        levels(mentalsmall$depressive)=c("Yes", "No")
        graph=ggplot(mentalsmall, aes(depressive,fill=depressive))+geom_bar()+theme_classic(base_size=22)+
          scale_fill_manual(values=c("midnightblue", "firebrick4"))+guides(fill=F)+
          ylab("Number of samples")+xlab("")+ggtitle("A")

        final=grid.arrange(graph, smote_graph, nrow=1)
        final

        
  #Correlation
      #To further clean the dataset, only keep attributes with 10%+ correlation with the class attribute (calculated through
      #Pearson's correlation)
      
      #pearson's correlation to determine correlation and save the values as a CSV
        mentalsmall_smote$depressive=as.numeric(mentalsmall_smote$depressive)
        write.csv(cor(mentalsmall_smote, method="pearson"), "correlationoutput.csv")
      
      #include only values with correlation =>0.1
        mentalsmall_smote_cor=mentalsmall_smote[, c(1,4,5,7,12,15,16,19,20,21,22,23,24,26,34,35,36,37,38,39,40,75,77,129,130,131, 133)]

#---
#Classification Algorithm
#---
    #10 fold cross validation, C4.5 (or J48) tree
        
      tc=trainControl("cv", 10, classProbs = TRUE, savePredictions = T)
      mentalsmall_smote_cor$depressive=as.factor(mentalsmall_smote_cor$depressive)
      levels(mentalsmall_smote_cor$depressive)=c("Yes", "No")
      Clean_c45_mental_tightcor=train(depressive~., data=mentalsmall_smote_cor, method="J48", trControl=tc)
      Clean_c45_mental_tightcor$results
      confusionMatrix(Clean_c45_mental_tightcor)
      Clean_c45_mental_tightcor$finalModel
      
#---
#Assessing Algorithm
#---
    #Claculating the ROC area
      pred=predict(Clean_c45_mental_tightcor, newdata=mentalsmall_smote_cor, type="prob")
      
      rocgraphno=roc(predictor=pred$No, response=mentalsmall_smote_cor$depressive)
      rocgraphno
      rocgraphyes=roc(predictor=pred$Yes, response=mentalsmall_smote_cor$depressive)
      rocgraphyes$specificities=1-rocgraphyes$specificities
      rocgraphno$specificities=1-rocgraphno$specificities
      g=ggroc(list(yes=rocgraphyes, no=rocgraphno))+theme_classic(base_size = 22)+scale_colour_manual(values=c("black", "black"))+guides(colour=FALSE)+
        xlab("Specificity")+ylab("True Positive Rate (Sensitivity)")
      g
      
     
      
  
  
  