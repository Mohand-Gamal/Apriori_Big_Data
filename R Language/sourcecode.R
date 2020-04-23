GetSupport<-function(data,minSupport){
  Support <- data.frame(Col1= numeric(0),Col2= numeric(0),Col3= numeric(0),Col4= numeric(0),Element1=numeric(0),Element2=numeric(0),Element3=numeric(0),Element4=numeric(0),Support =numeric(0),Level=character(0))
  for (col in 1:ncol(data)) {
    colRepeatition<-as.data.frame(table(data[col]))
    for(element in 1:nrow(colRepeatition)){
      elementSupport<-((colRepeatition[element,2]/nrow(data))*100)
      if(elementSupport >= minSupport){
        Support<-rbind(Support, data.frame(Col1 = colnames(data[col]),Col2=NA,Col3=NA,Col4=NA, Element1 = colRepeatition[element,1],Element2=NA,Element3=NA,Element4=NA,Support=elementSupport,Level="L1"))
      
      }
    }
  }
  #print(Support)
  for(col1 in 1:(ncol(data)-1)){
    for (col2 in col1+1:(ncol(data)-col1)){
      col2Repeatition<-as.data.frame(table(data[,c(col1,col2)]))
      for(element in 1:nrow(col2Repeatition)){
        elementSupport<-((col2Repeatition[element,3]/nrow(data))*100)
        if(elementSupport >= minSupport){
          Support<-rbind(Support, data.frame(Col1 = colnames(data[col1]),Col2 = colnames(data[col2]),Col3=NA,Col4=NA, Element1 = col2Repeatition[element,1], Element2 = col2Repeatition[element,2],Element3=NA,Element4=NA,Support=elementSupport,Level="L2"))
        } 
      }
    }
  }
  #print(Support)
  for(col1 in 1:(ncol(data)-2)){
    for (col2 in col1+1:(ncol(data)-col1-1)){
      for (col3 in col2+1:(ncol(data)-col2)){
        col3Repeatition<-as.data.frame(table(data[,c(col1,col2,col3)]))
        for(element in 1:nrow(col3Repeatition)){
          elementSupport<-((col3Repeatition[element,4]/nrow(data))*100)
          if(elementSupport >= minSupport){
            Support<-rbind(Support, data.frame(Col1 = colnames(data[col1]),Col2 = colnames(data[col2]),Col3=colnames(data[col3]),Col4=NA, Element1 = col3Repeatition[element,1], Element2 = col3Repeatition[element,2],Element3=col3Repeatition[element,3],Element4=NA,Support=elementSupport,Level="L3"))
          }
        }
      }
    }
  }
  #print(Support)
  for(col1 in 1:(ncol(data)-3)){
    for (col2 in col1+1:(ncol(data)-col1-2)){
      for (col3 in col2+1:(ncol(data)-col2-1)){
        for (col4 in col3+1:(ncol(data)-col3)){
          col4Repeatition<-as.data.frame(table(data[,c(col1,col2,col3,col4)]))
          for(element in 1:nrow(col4Repeatition)){
            elementSupport<-((col4Repeatition[element,5]/nrow(data))*100)
          if(elementSupport >= minSupport){
            Support<-rbind(Support, data.frame(Col1 = colnames(data[col1]),Col2 = colnames(data[col2]),Col3=colnames(data[col3]),Col4=colnames(data[col4]), Element1 = col4Repeatition[element,1], Element2 = col4Repeatition[element,2],Element3=col4Repeatition[element,3],Element4=col4Repeatition[element,4],Support=elementSupport,Level="L3"))
            }
          }
        }
      }
    }
  }
  #print(tail(Support))
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

