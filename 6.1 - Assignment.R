
# Question - 1: Function to check prime numbers
# ---------Function Definition --------------------------------------------------------------
fn.checkprime<-function(vecNum){
  retVal<-seq(1:length(vecNum)) # Create a dummy vector to capture the result to be returned
  intI<-1 #Declare and initilize a loop variable
  while(intI <= length(vecNum)) { #Set the while loop to iterate for all the elements of vector
    intInput<-vecNum[intI] #Access the vector element corresponding to iteration cycle
    if(!is.numeric(intInput)){ # Validation check for non numeric values & displays a conversion warning
      warning('The character values being converted to numeric', call. = TRUE, immediate. = FALSE)
      intInput<-as.numeric(intInput) #Convert to numeric
      if(is.na(intInput)){ #Validation check for failure in conversion & throws the exception 
        stop('Invalid input. Enter a numeric value.', call. = TRUE)
      }
    }
    
    intJ<-2
    while(intJ < intInput){ #While loop condition to test the vector element value
      if(intInput %% intJ == 0){ # checking if the remainder is zero
        break # breaks the loop if remainder 0 is obtained, result can be concluded and control jumps to pick next element value
      } else {
        intJ<-intJ+1 #incrementing the dinominator value for checking the division result
      }
    }
    if(intJ==intInput){ # checking If the input number is same as the dinnominator
      #print(cat(intInput,'Prime number', sep = ' -->', fill = FALSE))
      retVal[intI]<-paste(intInput,'Prime number\n', sep = '-->') # Capturing/adding the conclusion to a vector
    } else { 
      #print(cat(intInput,'Not a prime number', sep = ' -->', fill = FALSE))
      retVal[intI]<-paste(intInput,'Not a prime number\n', sep = ' -->') # Capturing/adding the conclusion to a vector
    }
    intI<-intI+1 #To incrementing the index value to refer to next element of input vector
  }
  return(retVal) # Return the result as vector
}
# Demo example for using the function
# ------------------------------------------
fn.checkprime(c('103','82','179'))
fn.checkprime(c(103,82,179))
fn.checkprime(103)
fn.checkprime(82)
fn.checkprime(179)
# Commented to enable compilation of R - markdown
#fn.checkprime('a')
#fn.checkprime(c('a','b','c'))


# Question - 2:
# install.packages('stringr') ## Prerequisites
# -----------------------------------------------------------------------

fn.detectchar<-function(srcString,schString,blnTogether){
  library('stringr') # Loading the stringr parckage
  
  intSchCount = 0 # Initializing program variable - capture count of match cases
  intIndexSrc = 1 #used a index to refer to source string vector elements
  schChar<-schString[1] #picking the first search pattern from the vector
  
  while(length(srcString) >= intIndexSrc ){ #Iterate for all elements of source string vector
    
    #cat('Checking element: ',trimws(paste(intIndexSrc,'(',srcString[intIndexSrc],')'),which = 'both'),'\n')
    
    #Displays the header title for the match operation being performed
    cat('Checking element: ',intIndexSrc,trimws('(',which='both'),srcString[intIndexSrc],trimws(')',which = 'both'),'\n')
    for(schChar in schString){ #iterating for each element of search pattern vector
      #Checking if there is a match, note, the use of blnFlag argumnent--> reserved for further enhancements
      if(TRUE == blnTogether && TRUE == (str_detect(srcString[intIndexSrc], schChar ))){
        cat(schChar, ' - search pattern APPEARS in given string:', '\n') #displaying the match result as formatted string mat
        intSchCount<-intSchCount+1 #Capturing the count of match cases
      }else if(blnTogether == FALSE){ #else case reserved for future implementation
        cat(schChar, ' To be implemented...', '\n')
      } else { # if the match is not found, displays the result as formatted string
        cat(schChar, ' - search pattern DOES NOT APPEAR in string:', '\n')
      }
    }
    #Investigating the total match cases out of provisionedand and displaying the conclusion for each element of source vector
    if(intSchCount == length(schString)){
      cat('ALL CHARACTERS IN SEARCH PATTERN APPEAR IN STRING - ',trimws(srcString[intIndexSrc],which='both'),'\n')
    }else if(intSchCount > 0){
      cat('NOT ALL, BUT ', intSchCount , 'CHARACTERS IN SEARCH PATTERN APPEAR IN STRING - ',trimws(srcString[intIndexSrc],which='both'),'\n')
    }else{
      cat('NONE OF THE CHARACTERS IN SEARCH PATTERN APPEAR IN STRING - ',trimws(srcString[intIndexSrc],which='both'),'\n')    
    }
    cat('\n') #adding a new line as separator
    intSchCount<-0 #resetting the count for capturing the summary of next element/match case
    intIndexSrc<-intIndexSrc+1 #increase the index for referring to next element  
  }
}
# Demo example for using the function
# ------------------------------------------
words<-c("above", "unit", "Under", "argument")
schStr<-c('a','u','o')
fn.detectchar(srcString = words, schString = schStr, blnTogether = T)

words<-c("above", "unit", "Under", "argument")
schStr<-c('abo','un','ooo')
fn.detectchar(srcString = words, schString = schStr, blnTogether = T)        

# Question - 3:
# ---------Function Definition --------------------------------------------------------------
fn.checkBMI<-function(weight, height){
    return(weight/(height^2))*703
}

# Demo example for using the function
# ------------------------------------------
fn.checkBMI(50, 1.75)
fn.checkBMI(72, 1.65)


# Question - 4:
# ---------Function Definition --------------------------------------------------------------
fn.sumofcubes<-function(x){ # function definition
  #validations - if the input is <1 or a non numeric value, returns zero
  if(x<=0 || !is.numeric(x)) return(0) 
  else{ #performing a recursive call to return the sum of cubes as described in the problem statement
    return(x^3 + fn.sumofcubes(x-1))
  }
}
# Demo example for using the function
# ------------------------------------------
fn.sumofcubes(0)
fn.sumofcubes('0')
fn.sumofcubes(6)
fn.sumofcubes(3)
fn.sumofcubes(2)

# Question - 5:
# ---------Function Definition --------------------------------------------------------------
fn.Mode<-function(x){ # function definition
  #validations - if the input is <1 or a non numeric value, returns zero
  if(length(x)<=1 || !is.numeric(x)) return('Insufficient or invalid arguments supplied!!!') 
  else{ 
    retVal<-unique(x) #checking the uniqueness
    
    # matches the unique set to value within the supplied vector to return the a vector of frequency or occurence
    retVal<-tabulate(match(x, retVal)) 
    
    #Returns the value from the vector where the vector element holds highest value (e of which.max()) 
    return(retVal[which.max(retVal)])
  }
}

# Demo example for using the function
# ------------------------------------------
x = c(2,3,3,4,4,4,4,4,5,6,7,9,10)
fn.Mode(x)


# question - 6:
# ---------Function Definition --------------------------------------------------------------
## Already created a functiona in response to question-1, please refer to the demo call section 
#for the given vector

# -------------------------Demo Call's----------------------------------------------
x <- c(2,2,3,3,4,5,7,11,15,19,24,29)
fn.checkprime(x)

#Question - 7:
# ---------Function Definition --------------------------------------------------------------
# Steps to create a package are - 
# 1. Create a folder to the package files/skeleton files and R command files 
#    and set it a home directory using the setwd() command
# 2. Download and install RTools component - required for compiling the package
# 3. As part of installation, edit the PATH environmental variable to setup RTools commands in environment
# 4. Create a function / set of functions thats needs to be enclosed within a package
# 5. Create a package skeleton using the command - package.skeleton() - this command generates the skeleton 
#    structure for the package to be created and published. it contains Description, namespace files in additions 
#    to R, man and data directories 
# 6. Edit the description and help files with details
# 7. Build the package using the command - R CMD build <package name>
# 8. Install the package using the command - R CMD INSTALL <Package>
# 9. Refresh/Reload RStudio and locate the package under the packages menu
# 10. Load the package using library() command and exdperiment around functions included in the package

# The package mentioned in the assignment question will be compiled and uploaded  to the git url. 


#Question - 8:
# inspall.packages('dplyr') # Prerequisite for the fucntion
# ---------Function Definition --------------------------------------------------------------
# Need to solve the questions with the help of DATA FRAMES

#8(a)
dsBoys<-read.csv(file.choose(), header = TRUE);head(dsBoys) #performed with data file Boys-top100.csv (included in repository)
dsGirls<-read.csv(file.choose(), header = TRUE);head(dsGirls) #performed with data file Girls-top100.csv (included in repository)

dfStu<-data.frame(roll_no = c(3,1,2,5,4,7,8), names = c('peter', 'jack', 'david', 'james', 'john','Sanjeev', 'Anshul'));dfStu
dfMarks<-data.frame(roll_no = c(4,12,13,6,15), maths = c(89,92,76,67,90), science = c(98,92,88,91,92));dfMarks

# 8(b) Exampe of inner\equi join
jnInner<-merge(dfStu, dfMarks);jnInner

# (8c) Exampe of left outer join
jnLouter <- merge(dfStu, dfMarks, by = 'roll_no', all.x=TRUE);jnLouter

# (8d) Exampe of right outer join
jnRouter <- merge(dfStu, dfMarks, by = 'roll_no', all.y=TRUE);jnRouter

# 8(e) Exampe of outer join
jnFouter<-merge(dfStu, dfMarks, by = 'roll_no', all = TRUE);jnFouter

# Exampe of cross join
jnCross<-merge(dfStu, dfMarks, by = NULL);jnCross

# 8(f) - Filter operation
library(dplyr) # using dplyr package to call filter function

# using filter() to return all rows where is.na() is TRUE
filter(jnFouter,is.na(jnFouter$maths)) 


# 8(g) - group by, sum and average operations
# example of group by
group_by(jnFouter,jnFouter$maths, add = FALSE)

# example of sum
summ<-sum(jnFouter$maths);summ
sums<-sum(jnFouter$science);sums

# example of average
avgm<-mean(jnFouter$maths);avgm
avgs<-mean(jnFouter$science);avgs

# example of summary grouping
group_by(jnFouter,jnFouter$maths, add = TRUE) %>% 
  summarise(sum = sum(jnFouter$maths),average = mean(jnFouter$maths))
          
# 8(h) -- need more clarity on the problem statement
# 8(i) -- need more clarity on the problem statement
# 8(j) -- need more clarity on the problem statement


# Question - 9:
# inspall.packages('dplyr') # Prerequisite for the fucntion
# vignette("programming") # displays documentations "Programming with dplyr Package"
# ---------Function Definition --------------------------------------------------------------

# Creating the data variables
StNames<-c('Sanjeev', 'Vishwanath', 'Mohit', 'Anbu', 'Prabhat', 'Siddharth', 'Satyam', 'Syandeep', 'Yogesh')
StdID<-seq(1:length(StNames));StdID
Class<-c(2,2,3,2,3,3,5,6,5);length(Class)
Section<-c(1,1,1,2,2,1,1,3,2);length(Section)

#Create the data frame using the data variables
dfStudents<-data.frame(StNames,StdID,Class,Section);dfStudents

# Displaying the unique data based on cloumns - Class and Section
distinct(.data = dfStudents, Class, Section, keep_all = TRUE) 

# Displaying the unique data based on cloumns - Class 
distinct(.data = dfStudents, Class, keep_all = TRUE) 

library(dplyr) #using the library for performing grouping and summary operations
# Grouping the data set values on two variables - Class and Section and displaying the 
# count of StdID values as summary
dfStudents %>% group_by(Class) %>% summarise(Summary = n_distinct(StdID))
dfStudents %>% group_by(Class, Section) %>% summarise(Summary = n_distinct(StdID))

#function to perform summary operation on a data frame
fn.countSumm<-function(df,...) { #using ... to signify acceptance of any number of arguments
  # capturing all the arguments in ... as list of formulas
  group_var<-quos(...) 
  
  #Performing a groupby on data frame and summarising by of number of rows
  return(df %>% group_by(!!!group_var) %>% summarise(Summary = n()))
}
# Calling the function example
fn.countSumm(dfStudents,Class,Section)

#function to detect unique & duplicate rows from data frame
fn.checkRows<-function(df,...,unqFlag, blnLevels=FALSE) { 
  if(blnLevels==TRUE){ # check flag to displaying the levels
    # using the if else construct to return the result 
    return(ifelse(unqFlag==T, df[!duplicated(df[...]),], df[duplicated(df[...]),]))
  }else{
    if(unqFlag==T){
      return(df[!duplicated(df[...]),]) #Returns the unique rows
    }else{
      return(df[duplicated(df[...]),])  #Returns the duplicate rows
    }
  }
}
# Demo example for using the function
# ------------------------------------------
# Calling without specifying the optional parameter
fn.checkRows(dfStudents,c(3,4),unqFlag = FALSE) # Display Duplicates
fn.checkRows(dfStudents,c(3,4),unqFlag = TRUE) # display Unique rows

# Calling specifying the optional parameter
fn.checkRows(dfStudents,c(3,4),unqFlag = FALSE,blnLevels = TRUE) # Display Duplicates
fn.checkRows(dfStudents,c(3,4),unqFlag = TRUE,blnLevels = TRUE) # display Unique rows

#Question - 10:
# ---------Function Definition --------------------------------------------------------------
# 10(a) - Function to check the dF for complete cases or checking presence of NA in specified columns
fn.checkNullDS<-function(df, blnChkDS=TRUE, ...=NULL){
  
  if(blnChkDS==T){ # check if data set check for complete cases required
    #retVal<-(!complete.cases(df)) # check for complete cares and return the result
    
    #(!complete.cases(jnFouter))
    retVal<-df[!complete.cases(df),]  # check for complete cares and return the rows 
  }else{
    #group_vars<-quos(...) # format the arguments as list of formulas, not required for is.na()
    retVal<-df[is.na(df[...]),] # return the s where NA is present
  }
  return(retVal)
}
# Demo example for using the function
# ------------------------------------------
# Calling the function - example
fn.checkNullDS(jnFouter, blnChkDS=TRUE)
fn.checkNullDS(jnFouter, blnChkDS=FALSE, c('names','maths'))

#10(b) - Need more clarity on problem statement
# Demo example for using the function
# ------------------------------------------

#Question - 11
# ---------Function Definition --------------------------------------------------------------
# 11(a) - remove duplicates
poise <- function(x){
  viz <- x[duplicated(x)]
  print(viz)
  return(x)
}
x <- c(8,9,9,7,5,4,4,3,2,6,6,2,1)
poise(x)

# 11(b)
Uni<-function(x){
  
  #sun<-(unique(x)
  sun<-n_distinct(x)
  return(sun)
}
x <- c(8,9,9,7,5,4,4,3,2,6,6,2,1)
Uni(x)

# 11(c)
clip <- function(){
  j <- "Planet"
  w <- "World"
  paste(j,w, sep="::")
}
clip()

# 11(d)
Melt <- function(){
  mat <- matrix(c(1:10), nrow=5,ncol=2);
  mat
  cat("Sum column wise :", apply(mat, 2, sum), "\n")
  cat("Sum row wise :", apply(mat, 1, sum))
}
Melt()

# 11(e) -- Need to explore hdfs 
# 11(f) -- Need to explore hdfs

#Question - 12
# ---------Function Definition --------------------------------------------------------------
seat <- function(){
  
  #12(a) -- Please refer the example from preevious questions
  #12(b) -- Need to explore hdfs
  
  seat<-Seatbelts;
  
  # 12(c) Rename the column names of a matrix object
  colnames(seat) <- c("driverkilled", "Drivers", "Front",
                      "Rear", "KM/S", "PetrolPrice",
                      "vankill", "LAW")
  print(seat)
  
  # 12(d) Example of dropping a column name
  seat <- seat[,(names(seat)) %in% c("law", "vankill")]
  print(seat)
  
  # 12(e) Examples of handling NA values
  x <- c(NA, NaN)
  print(is.na(x))
  print(is.nan(x))
  print(class(NA))
  print(class(NaN))
  print(class(NULL))
  
  # 12(f)
  vec <- c(1,2,3,4,5)
  if(is.numeric(vec)){
    print(TRUE)
  }
  ## g -- Compute number of unique combinations in a data frame grouped by certain columns
  library(dplyr)
  Orange
  distinct(Orange, Tree)
  
}
seat()


