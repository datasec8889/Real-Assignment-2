Marasigan, Raquel MP.R
###	1. Define an R function that removes NA values from a vector.
Answer {Easy way}
d <- c(1, 100, NA, 10)
max(d, na.rm=TRUE)

####???2. 	Define an R function that computes the factorial of given an integer argument.The output should be a vector of length 1.
##The output should be a vector of lenght 1  recursive calculation which make the code shorter and looks clean.(easy way)
#sample 1 (easy way)
recursive.factorial(0)
#sample 2 (easy way)
recursive.factorial(4)
###{hard way }
factorial = function(x) {
  # if x in [0,1], return 1
  if (x == 0 || x == 1) {
    return (1)
  } 
  else {
    return (x * factorial(x - 1)) 
  } 
}



#### 3. Sorts a given vector in decreasing order. This should accept numeric vector and character vectors

# arr = c(1,2,10,7,9, 100, 1000, -1, 3,4) # numeric
arr = c('ac','bd','ee','xy', 'gh') # characters
# insertion sort algorithm 
ins_sort <- function(arr){
  for (j in 2:length(arr)) {
    init = arr[j] 
    
    i = j - 1 
    while (i > 0 && arr[i] < init) {
      arr[(i + 1)] = arr[i]
      i = i - 1 
    }
    arr[(i + 1)] = init
  }
  return (arr)
} 
# ins_sort(arr)

####4. Return the nth highest number
##Here is the easy way to find the highest vector using sort

c <- c(4,2,44,2,1,45,34,2,4,22,244)
sort(unique(c), decreasing = TRUE)[1:5]



### 6 Look for the Prime Number
####Verify if the interger is prime or not use Boolean (TRUE AND FALSE)
isPrime <- function(num){ return (sum(num/1:num == num %/% 1:num) == 2 )}

### 7. Computes the compound interest of an investment.
###Given the rate, time and initial amount or principal

interest = function(principal, interest_rate = 0.01, n_cpd_periods = 1){
  return (principal * ((1 + interest_rate)**n_cpd_periods - 1))
}
interest(1000)

#####8. Define an R function that accepts a POSIXct as argument and outputs the day of the week as characters.
# Let the date as.POSIXct 

date_nw = as.POSIXct(as.Date("01/01/1970", format = "%m/%d/%Y"))

# Accepts POSIXct as input
which_weekday = function(date_nw) {c("Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday")[((unclass(date_nw)/86400) %% 7) + 1]}

which_weekday(date_nw)

