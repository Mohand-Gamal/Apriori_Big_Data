#Function for checking the validity of input to be from 1 till 100 %#
CheckInput <- function(input) {
  if (input > 100 || input < 0) {
    print("Input out of range")
    return (TRUE)
  }
  
  return (FALSE)
}

#Function to implement cyclic shift on a list#
Shifter <- function(x, n = 1) {
  if (n == 0)
    x
  else
    c(tail(x,-n), head(x, n))
}

#Function to extract our 12 columns form the whole data#
GetDataReady <- function() {
  # Path to the ticdata2000.txt file: path
  path <- file.path("ticdata2000.txt")
  
  # Import the ticdata2000.txt file: allData
  allData <- read.table(path, header = F, sep = "\t", )
  
  #extract the data we will cork on from allData
  ourData <- allData[, 11:22]
  
  #Setting the Column Names
  colnames(ourData) <- c(
    "MRELSA",
    "MRELOV",
    "MFALLEEN",
    "MFGEKIND",
    "MFWEKIND",
    "MOPLHOOG",
    "MOPLMIDD",
    "MOPLLAAG",
    "MBERHOOG",
    "MBERZELF",
    "MBERBOER",
    "MBERMIDD"
  )
  #return ourData
  return(ourData)
}


#Function to calculate the support for the whole data#
CalculateSupport <- function(data, minSupport)
{
  #copy data to local data frame y#
  y <- as.data.frame(data)
  
  #initialization level if support#
  level_Num = 0
  
  #Names of column of support data frame#
  c_Names = c(
    'MRELSA',
    'MRELOV',
    'MFALLEEN',
    "MFGEKIND",
    "MFWEKIND",
    "MOPLHOOG",
    "MOPLMIDD",
    "MOPLLAAG",
    "MBERHOOG",
    "MBERZELF",
    "MBERBOER",
    "MBERMIDD"
  )
  
  #initialization of data frame used to calculate support#
  pre_support <- data.frame()
  support <- data.frame()
  
  #Loop to generate all possible support levels#
  while (level_Num <= 11) {
    #increment support level to calculate different level each time#
    level_Num <- level_Num + 1
    
    #you get vector of vectors containing all combinations[x---> combinations]#
    x = combn(c_Names, level_Num, simplify = TRUE)
    
    #calculate frequency in every combination in X#
    for (i in 1:(length(x) / level_Num)) {
      #calculate the support in every combination#
      pre_support = plyr::count(y, x[, i])
      pre_support$percent_freq = ((pre_support$freq) / nrow(y)) * 100
      
      #choose only the combinations with support >= min support #
      pre_support <-
        subset(pre_support, pre_support$percent_freq >= minSupport)
      
      #Add to the support dataframe#
      support <- dplyr::bind_rows(support, pre_support)
      
      if (level_Num >= 2) {
        #prune data frame contains all possible assoication rules (all support levels except l1)#
        prune <<- dplyr::bind_rows(prune, pre_support)
      }
    }
    
  }
  
  #return Support data frame#
  return(support)
}


#Function for calculating association rules (confidence ,lift and leverage)
CalculateRules <- function(support, minConfidence)
{
  #initialization of rules data frame to hold all valid association rules#
  rules <- data.frame(matrix(ncol = 14, nrow = 0))
  
  #check if there were supports (other than l1) that are more than the minimum support#
  if (nrow(prune) > 0) {
    #initialization of rules column names with the same column names as support data frame#
    colnames(rules) <- colnames(support)
    
    #initialization of lift data frame to hold the lift for all valid association rules#
    Liftcol <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(Liftcol) <- c("Lift")
    
    #initialization of confidence data frame to hold the confidence for all valid association rules#
    Confcol <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(Confcol) <- c("Confidence")
    
    #initialization of leverage data frame to hold the leverage for all valid association rules#
    Leveragecol <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(Leveragecol) <- c("Leverage")
    
    #initialization of imply data frame to hold the index of the column of the RHS of each of the valid association rules#
    implycol <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(implycol) <- c("imply")
    
    #get level one support only to use it confidence rule #
    l1_support <- dplyr::setdiff(support, prune)
    
    
    #loop on every row in prune data frame
    for (row_index in 1:nrow(prune)) {
      #counter for every valid rule in same row#
      rulesCounter = 0
      
      #variable for calculating lift and leverage
      mult = 1
      
      #flag for detecting the presence of a rule
      there_is_rule = FALSE
      
      #get first rule only seperately in a row without nulls
      rule <- prune[row_index, ]
      rule <- rule[, colSums(is.na(rule)) == 0]
      
      #ids of specified columns in the row
      temp <- c("freq", "percent_freq")
      id  <- setdiff(as.vector(colnames(rule)), temp)
      idx  <- match(id, names(support))
      
      #get all possible combinations of these indices for different association rules#
      possibleRules <- as.data.frame(idx)
      idx1 <- idx
      for (i in 2:length(idx)) {
        #apply cyclic shift on column indices#
        idx1 <- Shifter(idx1)
        possibleRules <-
          dplyr::bind_cols(possibleRules, as.data.frame(idx1))
      }
      
      #Loop on all possible rules in this row#
      for (i in 1:length(possibleRules)) {
        #Get values of these columns by dropping un-needed columns#
        cols <- dplyr::select(rule, -freq)
        cols <- dplyr::select(cols, -percent_freq)
        
        #Get name of column of first row in possible rules data frame#
        coln <-
          colnames(dplyr::select(support, possibleRules[1, i]))
        
        #Get only from support dataframe the values of the first column where it matches the value of the column in the rule#
        den_support <-
          subset(support, support[, coln] == cols[1, coln])
        
        #If more than L2  use this loop to iterate and keep filtering the support data frame with the values that matches the values of the rule columns#
        if (length(idx) > 2) {
          for (r in 2:(length(idx) - 1)) {
            coln <- colnames(dplyr::select(den_support, possibleRules[r, i]))
            den_support <-
              subset(den_support, den_support[, coln] == cols[1, coln])
          }
        }
        
        #Get denominator of the confidence equation as the support of LHS part of the rule#
        den <- den_support[1, "freq"]
        
        #Get the l1 support of each column to use in lift and leverage equations#
        coln <-
          colnames(dplyr::select(den_support, possibleRules[1, i]))
        colnSupport <-
          dplyr::select(l1_support, coln, "percent_freq")
        colnsupport <-
          subset(colnSupport, colnSupport[, coln] == cols[1, coln])
        l1s <- colnsupport[1, "percent_freq"]
        
        #Get numerator of the confidence rule equation as the support of the intersection between LHS and RHS parts of the rule#
        num   <- rule$freq[1]
        
        #Calculate multiplication of each column l1 support to be used in lift and leverage equations#
        mult  <- mult * (l1s / 100)
        
        #calculate the confidence
        confidence <- (num / den) * 100
        
        #check for valid of invalid confidence#
        if (confidence >= minConfidence)
        {
          #Flag is set that there is at least one rule in this row#
          there_is_rule = TRUE
          
          #increment number of rules in this row#
          rulesCounter <- rulesCounter + 1
          
          #add the valid rule to the rules dataframe#
          rules <- dplyr::bind_rows(rules, prune[row_index, ])
          
          #add the confidence to the confidence data frame#
          Confcol <-
            dplyr::add_row(Confcol, Confidence = confidence)
          
          #add the index of the RHS column to the imply data frame#
          implycol <-
            dplyr::add_row(implycol, imply = possibleRules[length(idx), i])
          
          
        }
        
        #lift and leverage didn't change by changing which itemset implies other in the same rule#
        if (i == length(idx) && there_is_rule)
        {
          #calculate lift#
          lift <- ((rule$percent_freq[1] / 100) / mult)
          
          #calculate leverage#
          leverage <- ((rule$percent_freq[1] / 100) - mult)
          
          #add to the lift and leverage dataframes as much rows as the valid rules in this combination#
          for (k in 1:rulesCounter) {
            #add the lift to the lift data frame#
            Liftcol <- dplyr::add_row(Liftcol, Lift = lift)
            
            #add the leverage to the leverage data frame#
            Leveragecol <-
              dplyr::add_row(Leveragecol, Leverage = leverage)
          }
          
        }
        
      }
      
    }
    
    #change name of percent_freq column in rules dataframe to "Support"#
    names(rules)[names(rules) == 'percent_freq'] <- 'Support'
    
    #bind the rules,confidence,lift,leverage and imply dataframes into the rules dataframe by columns#
    rules <-
      dplyr::bind_cols(rules, Confcol, Liftcol, Leveragecol, implycol)
  }
  #return the final rules dataframe#
  return(rules)
}

#Function for the outputted rules
PrintRules <- function(rules) {
  #output the following in output.txt file#
  sink("output.txt")
  
  #check if there were any valid association rules#
  if (nrow(rules) > 0) {
    #Loop for each rule in the rules dataframe#
    for (ruleIndex in 1:nrow(rules)) {
      #Get one rule only into a one row dataframe#
      rule <- rules[ruleIndex, ]
      
      #Save the index and name of the RHS column of the rule into a variable#
      consequent <- rule[1, "imply"]
      consequentName <- colnames(dplyr::select(rule, consequent))
      
      #remove all columns that don't carry value (NA) in the rule#
      rule <- rule[, colSums(is.na(rule)) == 0]
      
      #save the support of the rule in a variable#
      support <- rule[1, "Support"]
      
      #save the support of the rule in a variable#
      confidence <- rule[1, "Confidence"]
      
      #save the support of the rule in a variable#
      lift <- rule[1, "Lift"]
      
      #save the support of the rule in a variable#
      leverage <- rule[1, "Leverage"]
      
      #copy the rule data frame into a new data frame to extract the LHS side of the rule#
      LHSrule <- rule
      
      #drop any un-needed columns from the dataframe inorder to extract the columns for the LHS side of the rule only#
      LHSrule <- dplyr::select(LHSrule, -freq)
      LHSrule <- dplyr::select(LHSrule, -consequentName)
      LHSrule <- dplyr::select(LHSrule, -Support)
      LHSrule <- dplyr::select(LHSrule, -Confidence)
      LHSrule <- dplyr::select(LHSrule, -Lift)
      LHSrule <- dplyr::select(LHSrule, -Leverage)
      LHSrule <- dplyr::select(LHSrule, -imply)
      
      #Get names of the columns for the LHS side of the rule#
      antecedentNames <- colnames(LHSrule)
      
      #Loop on the columns for the LHS side of the rule#
      for (antecedentIndex in 1:ncol(LHSrule)) {
        #print the column name#
        cat(antecedentNames[antecedentIndex])
        
        #print the column value#
        cat("(")
        cat(LHSrule[1, antecedentIndex])
        cat(") ")
        
        #if it's the last column#
        if (antecedentIndex == ncol(LHSrule)) {
          #print implication sign#
          cat("---> ")
        }
        
        #if it's not the last column#
        else{
          #print & sign#
          cat("& ")
        }
      }
      
      #print the column name of the RHS side of the rule#
      cat(consequentName)
      
      #print the column value of the RHS side of the rule#
      cat("(")
      cat(rule[1, consequentName])
      cat(")\t")
      
      #print the support of the rule#
      cat("Support:")
      cat(support)
      
      #print the confidence of the rule#
      cat("\tConfidence:")
      cat(confidence)
      
      #print the Lift of the rule#
      cat("\tLift:")
      cat(lift)
      
      #print the Leverage of the rule#
      cat("\tLeverage:")
      cat(leverage)
      
      #go to the next line#
      cat("\n")
    }
  }
  
  #If there were no vallid rules#
  else{
    cat("There was no valid association rules in the data to satisfy the conditions given")
  }
  
  #finish writing in file#
  sink()
  
  #show the finished output.txt file containing all the valid association rules along with there support,confidence,lift and leverage#
  file.show("output.txt")
}

# Get the input from the user and check validity#
error = TRUE

#loop until the inputted support is valid#
while (error == TRUE) {
  minSupport = readline("Please Enter the minimum Support in percentage: ")
  minSupport <- as.numeric(minSupport)
  error = CheckInput(minSupport)
}

error = TRUE

#loop until the inputted confidence is valid#
while (error == TRUE) {
  minConfidence = readline("Please Enter the minimum Confidence in percentage: ")
  minConfidence <- as.numeric(minConfidence)
  error = CheckInput(minConfidence)
}

#initializa an empty global variable to store the all support levels except the l1 support for the confidence calculations#
prune <<- data.frame()

# Get the data
data = GetDataReady()

#get the support dataframe by calling the function#
support <- CalculateSupport(data, minSupport)

#get the valid association rules dataframe by calling the function#
rules <- CalculateRules(support, minConfidence)

#print the valid rules in a text file by calling the function#
PrintRules(rules)