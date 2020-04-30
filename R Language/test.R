CheckInput<-function(input){
  
  if (input>100 || input<0){
    print("Input out of range")
    return (TRUE)
  } 
  
  return (FALSE)
}


GetDataReady<-function(){
  # Path to the ticdata2000.txt file: path
  path <- file.path("D:/4th year computer/SECOND TERM/big data/project/R Language/ticdata2000.txt")
  
  # Import the ticdata2000.txt file: allData
  allData <- read.table(path,header = F, sep = "\t",) 
  
  #extract the data we will cork on from allData
  ourData <- allData[,11:22]
  
  #Setting the Column Names
  colnames(ourData) <- c("MRELSA","MRELOV","MFALLEEN","MFGEKIND",
                         "MFWEKIND","MOPLHOOG","MOPLMIDD","MOPLLAAG",
                         "MBERHOOG","MBERZELF","MBERBOER","MBERMIDD")
  #return ourData
  return(ourData)
}


# Get the input from the user and check validity
error=TRUE
while(error==TRUE){
  minSupport=readline("Please Enter the minimum Support: ")
  minSupport<- as.numeric(minSupport)
  error=CheckInput(minSupport)
}

error=TRUE
while(error==TRUE){
  minConfidence=readline("Please Enter the minimum Confidence: ")
  minConfidence<- as.numeric(minConfidence)
  error=CheckInput(minConfidence)
}



prune<<- data.frame()

######apriori CODE BE EZN ALLAAAAAAAAAAAAAAAAAAAAAAAAAAAAH########

calculate_Support<-function(data,minSupport)
{

  y<-as.data.frame(data)
  level_Num = 0
  c_Names = c('MRELSA','MRELOV','MFALLEEN',"MFGEKIND","MFWEKIND","MOPLHOOG","MOPLMIDD","MOPLLAAG","MBERHOOG","MBERZELF","MBERBOER","MBERMIDD")
  #pre_level<-y #t
  pre_support <-data.frame()
  support <- data.frame()
  
  
  while(level_Num<=11){
    
    level_Num <- level_Num + 1
    
    #you get vector of vectors containing all combinations[x---> combinations]
    x = combn(c_Names,level_Num,simplify = TRUE)
    
    #calculate frequency in every combination in X
    for(i in 1:(length(x)/level_Num)){
      
      #calculate the support in every combination
      pre_support= plyr::count(y,x[,i])

      pre_support$percent_freq = ((pre_support$freq)/nrow(y))*100
      
      pre_support <- subset(pre_support, pre_support$percent_freq >= minSupport)
      
  
      support <- dplyr::bind_rows(support,pre_support)
      
      if(level_Num>=2){
        
      prune <<-dplyr::bind_rows(prune,pre_support)
      }
    }
    
  }
  
  
  return(support)
}  
  
  
  #Function of confidence
Calculate_Confidence<-function(support,minConfidence)
{
  rules<-data.frame()
  #ncol(rules) <- 5
  #names(rules)<- c("Association_rule","support","confidence","lift", "leverage")
  rule_names<-data.frame()
  
  #get level one support only to use it confidence rule (intersection of both / intersection of one)
  l1_support<-dplyr::setdiff(support,prune)
  
 
  
  #loop on every row in prune data frame
  for ( row_index in 1: nrow(prune)) {
    
    #variable for calculating lift and leverage
    mult=1
    
    #flag for detecting the presence of a rule
    there_is_rule = FALSE
    
    #get first rule only seperately in a row without nulls
    rule <- prune[row_index,]
    rule <- rule[ , colSums(is.na(rule)) == 0]
    print(rule)
   
    #ids of specified coloumns in l1 suppport
    temp <- c("freq","percent_freq" )
    id  <- setdiff(as.vector(colnames(rule)), temp)
    idx  <- match(id,names(l1_support))
    print(idx)
    
    for (i in 1:length(idx)) {
      
      #get the support of one element
      temp  <- dplyr::select(l1_support, idx[i],"freq")
      temp2 <- dplyr::select(prune[row_index,], idx[i])
      temp  <- as.data.frame(temp)
      temp2 <- as.data.frame(temp2)
      den   <- subset(temp, temp[1]== temp2[[1]])
      den   <- den[1,2]
      num   <- rule$freq[1]
      mult  <- mult * den
      print(den)
      
      
      
      #calculate the confidence
      confidence <- (num / den)*100
      print(confidence)
      
      
      
      if(confidence >= minConfidence)
      { 
        there_is_rule = TRUE
      
        #produce assoication rule
      
        #dplyr::add_row(rules)
        #rules$Association_rule
      
      }
      
      #lift and leverage didn't change by changing which itemset implies other in the same rule
      if(i==length(idx) && there_is_rule)
      {
        #calculate lift
        lift <- num / mult
        print(lift)
        
        #calculate leverage
        leverage <- num - mult
        print(leverage)
        
        #initialize mult once again and there_is_rule(redundant i think)
        mult <- 1
        there_is_rule = FALSE
      }
      
  }
    
  }
  
  return(rules)
}
  
  
  
  
  
  


#install.packages("tidyverse")
# Get the data
data=GetDataReady()
# Call head() on data
head(data)
support <- calculate_Support(data,minSupport)
rules <- Calculate_Confidence(support,minConfidence)



