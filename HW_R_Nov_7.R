1. WHO datasets-DATA LOADING and initila inspection
###Use the read.csv(0 function and store the resulting value)
WHO <- read.csv('WHO.csv')
### a.) Inspect the first five rows of the loaded data using the "head" function
head(WHO,5)

### b.) Find the country with the biggest population-
#######look for the max value using the max function
pop_max <- max(WHO$Population)
#######look for the row of the country with a population equal to the pop_max using the subset function
row_with_max_pop <- subset(WHO, Population == pop_max)

#####Get the name of the country get the value of the Country column using the $ sign
country_with_max_pop_name <- row_with_max_pop$Country
####Print out the countr name
print(country_with_max_pop_name)

#### c.) Get the population of Malaysia - look the first row which Malaysia and use subset function
malaysia <- subset(WHO, Country == 'Malaysia')
###Print the population of Malysia and use $ sign
malaysia$Population

### d.) Find  the country with the lowest literacy rate.
####Look for the minimum value of literacy rate using the min function; use the $ sign in combination 
###with the dataframe name to access the  LiteracyRate column; to be safe, specify in the function to remove all NA values.
literacy_min <- min(WHO$LiteracyRate, na.rm = TRUE)
####Create a subset to select the row which contains the lowest literacy values
row_lowest_literacy <- subset(WHO, LiteracyRate == literacy_min)
####Print the sign to show the name of the country with lowest literacy rate
print(row_lowest_literacy$Country)
####Then it will reduce the single lines.
subset(WHO, LiteracyRate == min(WHO$LiteracyRate, na.rm = TRUE))$Country

#### e.) Look for the richest country in Europe based on GNI
### Create a subset the contines of europe
europe <- subset(WHO, Region == 'Europe')
###Pick up the max value of GNI in the dataframe europe
max_gni_europe <-max(europe$GNI, na.rm = TRUE)
### The use subset to select the row which contains the lowest literacy values.
row_with_max_gni_europe <- subset(europe,GNI == max_gni_europe)
#### Use $ sign to show the name of the country with the lowest literacy rate
print(row_with_max_gni_europe$Country)

####f.) The mean life expectancy of countries in AAfrica
###Create a subset or region of AFRICA
africa <- subset(WHO, Region == 'Africa')
###tTo print the result the mean of the life expectancy use $ to select the column; set to TRUE so there will no NULL values
mean_life_expectancy_africa <- mean(africa$LifeExpectancy, na.rm = TRUE)
mean_life_expectancy_africa

#### g.) Find the number of countries with population over 10M
#####Create a subset to hold the who dataframe
country_with_max_pop_name_over_10M <- subset(WHO,Population > 10000)
#####Print the result using the dim function
dim(country_with_max_pop_name_over_10M)[1]

####h.) Find names of the country (top 5) in the Americas with the highest child mortality rate
####Create a subset and order the americas data frame using order function then set to decreasing value to TRUE
###****Note: Run one line script at time to get the result****
americas <- subset(WHO, Region == 'Americas')
index_ordered_childmortality_americas <- order(americas$ChildMortality, decreasing = TRUE)
#####Order the americas dataframe using the result of the operation above and the list top 5 countried using indexing por head function
###****Note: Run one line script at time to get the result****
americas_ordered_by_childmortality <- americas[index_ordered_childmortality_americas,]
top_5_childmortality_americas_head <- head(americas_ordered_by_childmortality,5)
top_5_childmortality_americas_indexing <- americas_ordered_by_childmortality[1:5,]
#######Finally, print the result
top_5_childmortality_americas_indexing$Country

#####2. The NBA Historical Performance Dataset 
###ReadYEAR_Output the data from Historical NBA Performance
Use the readxl library. For more information type ?readxl on the terminal.
library('readxl')
######Then it will read the excel file and store it into a dataframe.
historical_nba <- read_excel('Historical NBA Performance.xlsx')
#####a. Find the year the Chicago Bulls has its highest winning percentage
######Subset the Bulls.
bulls <- subset(historical_nba, Team == 'Bulls')
#######Next get the Bulls' highest winning percentage.
bulls_highest_winpct <- max(bulls$`Winning Percentage`)
######Subset the row when the bulls got its highest winning percentage; for a column name with space use the grave accent sign `.
bulls_row_highest_winpct <- subset(bulls, `Winning Percentage` == bulls_highest_winpct)
#####Print the year.
bulls_row_highest_winpct$Year


###b.) Find teams with an even win-loss record (i.e. the teams whose recorded win pct for the year are 0.500).
###create the subset for historical_nba dataframe 0.5 win percentage. Print the result.
teams_even <- subset(historical_nba, 'Winning Percentage' == 0.5)
teams_even

####3.) The seasons stats datasets
####You need to do initial and load the data
######Note: ran one line script at time******
season_stats <- read.csv('Seasons_Stats.csv')
head(season_stats)
#####a.) Find the player with the highest three point attempt rates
####I use the import tidyverse(googled) then account he playsers during operations
library('tidyverse')
####I create the following select the year, player ,fil the yrs 199-996roup and summary the X3PAr(googled) below 
##statements to be use
season_stats_combined <- season_stats %>%
  select(Year, Player, X3PA, X2PA) %>%
  filter(Year >1979) %>%
  group_by(Year, Player) %>%
  summarize(X3PA = sum(X3PA), X2PA = sum(X2PA)) %>%
  mutate(X3PAr = X3PA / (X3PA+X2PA))

####Find the palyers wiht highest free throes rate in season (follow same procedure above)
season_stats_combined_ftr <- season_stats %>%
  select(Year, Player, FTA, FGA) %>%
  group_by(Year, Player) %>%
  summarize(FTA= sum(FTA), FGA = sum(FGA)) %>%
  mutate(FTr = FTA/FGA)
season_stats_combined_ftr <- season_stats_combined_ftr[!is.infinite(season_stats_combined_ftr$FTr),]
subset(season_stats_combined_ftr, FTr == max(season_stats_combined_ftr$FTr, na.rm = TRUE))

#####c.) Find the year LeBron James recorded his highest number of points
#####Need to create a subset for the seasons totals LBJ and get the maX total number of points he scored.
lbj <- subset(season_stats, Player == 'LeBron James')
lbj_max_pts <- max(lbj$PTS, na.rm = TRUE)
####Then subset the season when he tally the most number of points
subset(lbj, PTS == lbj_max_pts)$Year

####d. The year the Michaeel Jordan scored his most number of points
###Just follow the c.) all good to run the script
mj <- subset(season_stats, Player == 'Michael Jordan*')
subset(mj, PTS == max(mj$PTS))$Year

######e.) Get Kobe Bryant's PER when his MP (minutes played) is the lowest
####Initial is create a subset to find the min MP and print the PER
kobe <- subset(season_stats, Player =='Kobe Bryant')
subset(kobe, MP == min(kobe$MP))$PER

####4. The National Universities Rankings load the dataframe
####reade the using read.csv() function
universities <-read.csv('National Universities Rankings.csv')
#####a. find the university with the most number of undergrad
###I uset the which.max() function (googled) it returns the index value, subset the row of the university [] and get the name using 
####$ then output is the print result.
print(universities[which.max(universities$Undergrad.Enrollment),]$Name)
#####b. Find the average tuition of the top ten universities
####For this task I created a subset for universities then use the order() function on the rank column(googled)
####then select the first ten rows.
top_10 <- universities[order(universities$Rank),][1:10,]
####my challenge is I need to change the numeric I use gsub() function (googled) to remove $ it was convert then print.
top_10$tuition_no_dollar <- gsub(pattern = "\\$|\\,",replacement = "", top_10$Tuition.and.fees)
mean(as.numeric(top_10$tuition_no_dollar))


