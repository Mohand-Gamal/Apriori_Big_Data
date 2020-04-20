CheckInput<-function(input){
  
  if (input>100 || input<0){
    print("Input out of range")
    return (TRUE)
  } 
  
  return (FALSE)
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

GetSupport<-function(data,minSupport){
  #for (col in 1:ncol(data)) {
  col=1;
    colRepeatition=table(data[col])
    print(colRepeatition[[col]])
    for(element in colRepeatition){
      elementSupport<-((element[1]/nrow(data))*100)
      if(elementSupport>=minSupport){
        l1Support<-(colnames(data)[col])
      }
    }
  #}
}

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

data=GetDataReady()
# Call head() on data
head(data)

GetSupport(data,minSupport)

