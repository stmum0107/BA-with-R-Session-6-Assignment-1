#Assignment-1 of Session-6
# -----------------------------------------------------------------------
# question - 1:
# Write a R program using control operators to test whether following values are prime
# numbers or not by providing a PRIME or NOT PRIME message as output :
# A. 103
# B. 82
# C. 179

# Answer: Please refer the following function code. As a demo of function call 
# please try following commands after compiling the function code
# -----------------------------------------------------------------------
# Demo-1 : fn.checkprime(c('103','82','179')) ,
# Demo-2 : fn.checkprime(c(103,82,179))
# Demo-3 : fn.checkprime(103)
# Demo-4 : fn.checkprime(82)
# Demo-5 : fn.checkprime(179)
# Demo-6 : fn.checkprime('a')
# Demo-7 : fn.checkprime(c('a','b','c'))
# -----------------------------------------------------------------------

fn.checkprime<-function(vecNum){

  retVal<-seq(1:length(vecNum))
  
  intI<-1
  while(intI <= length(vecNum)) {
    intInput<-vecNum[intI]
    if(!is.numeric(intInput)){
      warning('The character values being converted to numeric', call. = TRUE, immediate. = FALSE)
      intInput<-as.numeric(intInput)
      if(is.na(intInput)){
        stop('Invalid input. Enter a numeric value.', call. = TRUE)
      }
    }
    
    intJ<-2
    while(intJ < intInput){
      if(intInput %% intJ == 0){
        break
      } else {
        intJ<-intJ+1
      }
    }
    if(intJ==intInput){
      #print(cat(intInput,'Prime number', sep = ' -->', fill = FALSE))
      retVal[intI]<-paste(intInput,'Prime number\n', sep = '-->')
    } else {
      #print(cat(intInput,'Not a prime number', sep = ' -->', fill = FALSE))
      retVal[intI]<-paste(intInput,'Not a prime number\n', sep = ' -->')
    }
    intI<-intI+1
  }
  return(retVal)
}
fn.checkprime(c('103','82','179'))
fn.checkprime(c(15,77,19))
 # question - 2:
# Write a R program using control operators to identify letter u and a both occur in the
# following words:
#  1. above
#  2. unit
#  3. Under

# Answer: 
# 
# -----------------------------------------------------------------------
# Demo-1 : 
# Demo-2 : 

# -----------------------------------------------------------------------
