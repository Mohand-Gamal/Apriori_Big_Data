CheckInput<-function(input){
  
  if (input>100 || input<0){
    print("Input out of range")
    return (TRUE)
  } 
  
  return (FALSE)
}

shifter <- function(x, n = 1) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}

GetDataReady<-function(){
  # Path to the ticdata2000.txt file: path
  path <- file.path("ticdata2000.txt")
  
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
  rules<-data.frame(matrix(ncol = 14, nrow = 0))
  colnames(rules) <- colnames(support)
  
  Liftcol<-data.frame(matrix(ncol = 1,nrow = 0))
  colnames(Liftcol)<-c("Lift")
  
  Confcol<-data.frame(matrix(ncol = 1,nrow = 0))
  colnames(Confcol)<-c("Confidence")
  
  Leveragecol<-data.frame(matrix(ncol = 1,nrow = 0))
  colnames(Leveragecol)<-c("Leverage")
  
  implycol<-data.frame(matrix(ncol = 1,nrow = 0))
  colnames(implycol)<-c("imply")
  
  #get level one support only to use it confidence rule (intersection of both / intersection of one)
  l1_support<-dplyr::setdiff(support,prune)
  
  
  #loop on every row in prune data frame
  for ( row_index in 1: nrow(prune)) {
    
    rulesCounter=0
    
    #variable for calculating lift and leverage
    mult=1
    
    #flag for detecting the presence of a rule
    there_is_rule = FALSE
    
    #get first rule only seperately in a row without nulls
    rule <- prune[row_index,]
    rule <- rule[ , colSums(is.na(rule)) == 0]
    #print(rule)
    
    #ids of specified coloumns in l1 suppport
    temp <- c("freq","percent_freq" )
    id  <- setdiff(as.vector(colnames(rule)), temp)
    idx  <- match(id,names(support))
    possibleRules <- as.data.frame(idx)
    idx1<-idx
    for(i in 2:length(idx)){
      idx1<-shifter(idx1)
      possibleRules <- dplyr::bind_cols(possibleRules,as.data.frame(idx1))
    }
    
    #print(length(possibleRules))
    for (i in 1:length(possibleRules)) {
      cols <-dplyr::select(rule,-freq)
      cols <-dplyr::select(cols,-percent_freq)
      coln<-colnames(dplyr::select(support,possibleRules[1,i]))
      den_support <- subset(support,support[,coln]==cols[1,coln])
      
      if(length(idx)>2){
        for(r in 2:(length(idx)-1)){
          coln<-colnames(dplyr::select(den_support,possibleRules[r,i]))
          den_support <- subset(den_support,den_support[,coln]==cols[1,coln])
        }
      }
      den <- den_support[1,"freq"]
      #print(den)
      coln<-colnames(dplyr::select(den_support,possibleRules[1,i]))
      colnSupport<- dplyr::select(l1_support,coln,"percent_freq")
      colnsupport <- subset(colnSupport,colnSupport[,coln]==cols[1,coln])
      l1s<-colnsupport[1,"percent_freq"]
      num   <- rule$freq[1]
      mult  <- mult * (l1s/100)
      #print(den2)
      
      
      
      #calculate the confidence
      confidence <- (num / den)*100
      #print(confidence)
      
      
      if(confidence >= minConfidence)
      { 
        there_is_rule = TRUE
        rulesCounter<-rulesCounter+1
        #produce assoication rule
        
        rules<-dplyr::bind_rows(rules,prune[row_index,])
        Confcol<-dplyr::add_row(Confcol,Confidence=confidence)
        implycol<-dplyr::add_row(implycol,imply=possibleRules[length(idx),i])
        
        
      }
      
      #lift and leverage didn't change by changing which itemset implies other in the same rule
      if(i==length(idx) && there_is_rule)
      {
        #calculate lift
        lift <- ((rule$percent_freq[1]/100) / mult)
        #print(lift)
        
        #calculate leverage
        leverage <- ((rule$percent_freq[1]/100) - mult)
        #print(leverage)
        
        for(k in 1:rulesCounter){
          Liftcol<-dplyr::add_row(Liftcol,Lift=lift)
          Leveragecol<-dplyr::add_row(Leveragecol,Leverage=leverage)
        }
        
      }
      
    }
    
  }
  names(rules)[names(rules) == 'percent_freq'] <- 'Support'
  rules<-dplyr::bind_cols(rules,Confcol,Liftcol,Leveragecol,implycol)
  return(rules)
}

#Function for the outputted rules
Print_Rules<-function(rules){
  
  ruleAll<-data.frame()
  sink("output.txt")
  
  for(ruleIndex in 1:nrow(rules)){
    rule <- rules[ruleIndex,]
    consequent<-rule[1,"imply"]
    consequentName<-colnames(dplyr::select(rule,consequent))
    rule <- rule[ , colSums(is.na(rule)) == 0]
    support <- rule[1,"Support"]
    confidence <-rule[1,"Confidence"]
    lift<-rule[1,"Lift"]
    leverage<-rule[1,"Leverage"]
    
    RHSrule<-rule
    RHSrule<-dplyr::select(RHSrule,-freq)
    RHSrule<-dplyr::select(RHSrule,-consequentName)
    RHSrule<-dplyr::select(RHSrule,-Support)
    RHSrule<-dplyr::select(RHSrule,-Confidence)
    RHSrule<-dplyr::select(RHSrule,-Lift)
    RHSrule<-dplyr::select(RHSrule,-Leverage)
    RHSrule<-dplyr::select(RHSrule,-imply)
    
    antecedentNames<-colnames(RHSrule)
    for(antecedentIndex in 1:ncol(RHSrule)){
      cat(antecedentNames[antecedentIndex])
      cat("(")
      cat(RHSrule[1,antecedentIndex])
      cat(") ")
      if(antecedentIndex==ncol(RHSrule)){
        cat("---> ")
      }
      else{
        cat("& ")
      }
    }
    
    cat(consequentName)
    cat("(")
    cat(rule[1,consequentName])
    cat(")\t")
    cat("Support:")
    cat(support)
    cat("\tConfidence:")
    cat(confidence)
    cat("\tLift:")
    cat(lift)
    cat("\tLeverage:")
    cat(leverage)
    cat("\n")
  }
  sink()
  file.show("output.txt")
}

# Get the input from the user and check validity
error=TRUE
while(error==TRUE){
  minSupport=readline("Please Enter the minimum Support in percentage: ")
  minSupport<- as.numeric(minSupport)
  error=CheckInput(minSupport)
}

error=TRUE
while(error==TRUE){
  minConfidence=readline("Please Enter the minimum Confidence in percentage: ")
  minConfidence<- as.numeric(minConfidence)
  error=CheckInput(minConfidence)
}



prune<<- data.frame()

# Get the data
data=GetDataReady()

support <- calculate_Support(data,minSupport)

rules <- Calculate_Confidence(support,minConfidence)

Print_Rules(rules)