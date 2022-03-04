#######Exploring Sales Data set############################
##########################################################
library(tidyverse)

#Read data in
#setwd
getwd()
sales <- read.csv("Sales.csv",
                  header=TRUE, stringsAsFactors = FALSE)

#Check data types, distribution, categories, NULL values, and extraneous values

head(sales, 10)

sum(is.na(sales$Customer.Number)) #26 NULL
sum(is.na(sales$CustomerName))
sum(is.na(sales$Sales)) #3 NULL

sort(sales$Customer.Number, na.last = TRUE, decreasing = FALSE)
sort(sales$Customer.Number, na.last = TRUE, decreasing = TRUE)

sort(sales$CustomerName, na.last = TRUE, decreasing = FALSE) #5 non-alphanumeric characters
sort(sales$CustomerName, na.last = TRUE, decreasing = TRUE)

sort(sales$Sales, na.last = TRUE, decreasing = FALSE) #5 non-alphanumeric characters
sort(sales$Sales, na.last = TRUE, decreasing = TRUE)

#sales_tbl <- as_tibble(sales)

##Investigating Duplicate Entries

##exact duplicates
sales2 <-distinct(sales) #Exact duplicates = 27

##duplicate customer names
sales2 %>% 
  count(CustomerName) %>% 
  filter(n > 1) %>% 
  arrange(-n) 

name_dup<- sales2 %>% 
  count(CustomerName) %>% 
  filter(n > 1) %>% 
  arrange(-n) 

#Duplicate customer Names with different sales
sales2 %>% 
  filter(CustomerName %in% name_dup$CustomerName) %>% 
  arrange (CustomerName, -Sales)

#duplicate customer number

num_dup<-sales2 %>% 
  count(Customer.Number) %>% 
  filter(n > 1) %>% 
  arrange(-n) 

sales2 %>% 
  filter(Customer.Number %in% num_dup$Customer.Number) %>% 
  arrange (Customer.Number, -Sales)

#company ID is unique to each company
sales2 %>% 
  select(1,2) %>% 
  distinct() %>% 
  count(Customer.Number, CustomerName) %>% 
  arrange(CustomerName)

#Distribution
#skewed to right
options(scipen = 999)
boxplot(sales2$Sales) 
hist(sales2$Sales,
     xlim = c(0,100000), breaks = 1000)
options(scipen = 0)

###############Exploring External Company Data Set

#Read data in
#setwd
getwd()
Company <- read.csv("ExternalCompanyData.csv",
                  header=TRUE, stringsAsFactors = FALSE)

#Check association to Sales data set

##customer number
length(unique(sales$Customer.Number))
length(unique(Company$FirmID))

sum(sales$Customer.Number %in% Company$FirmID)

##customer name
length(unique(sales$CustomerName))
length(unique(Company$Firm.Name))

sum(gsub("[^a-zA-Z0-9]","",toupper(sales$CustomerName)) %in%
      gsub("[^a-zA-Z0-9]","",toupper(Company$Firm.Name)))

##
#check if summary() also works


##Explore variable: is.importer
sum(is.na(Company$Is.Importer))

sort(Company$Is.Importer, na.last = TRUE, decreasing = FALSE)
sort(Company$Is.Importer, na.last = TRUE, decreasing = TRUE) #blanks

table(Company$Is.Importer) #balanced yes/no

##Explore variable: is.exporter
sum(is.na(Company$Is.Exporter))

sort(Company$Is.Exporter, na.last = TRUE, decreasing = FALSE)
sort(Company$Is.Exporter, na.last = TRUE, decreasing = TRUE) #blanks

table(Company$Is.Exporter) #balanced yes/no

##Explore variable: is.Subsidiary
sum(is.na(Company$Is.Subsidiary)) #Nulls

sort(Company$Is.Subsidiary, na.last = TRUE, decreasing = FALSE)
sort(Company$Is.Subsidiary, na.last = TRUE, decreasing = TRUE) 

table(Company$Is.Subsidiary) #moderately balanced yes/no

##Explore variable: Location.Type
sum(is.na(Company$Location.Type))

sort(Company$Location.Type, na.last = TRUE, decreasing = FALSE)
sort(Company$Location.Type, na.last = TRUE, decreasing = TRUE) #blanks

table(Company$Location.Type) #not well-balanced: branch(28), headquaters(78), single_location(550)

##Explore variable: Is.Manufacturing
sum(is.na(Company$Is.Manufacturing))

sort(Company$Is.Manufacturing, na.last = TRUE, decreasing = FALSE)
sort(Company$Is.Manufacturing, na.last = TRUE, decreasing = TRUE) #blanks

table(Company$Is.Manufacturing) #well-balanced Yes/No

##Explore variable: Revenue
sum(is.na(Company$Revenue..US.Dollars..million.))

options(scipen = 999)
sort(Company$Revenue..US.Dollars..million., na.last = TRUE, decreasing = FALSE)
sort(Company$Revenue..US.Dollars..million., na.last = TRUE, decreasing = TRUE) #blanks
options(scipen = 0)
# some companies without revenue reported...valid

#Highly right-skewed
#could be transformed
boxplot(Company$Revenue..US.Dollars..million.) 
hist(Company$Revenue..US.Dollars..million.,
     xlim = c(0,500), breaks = 200) 

##Explore variable: Total.Employees
sum(is.na(Company$Total.Employees))

sort(Company$Total.Employees, na.last = TRUE, decreasing = FALSE)
sort(Company$Total.Employees, na.last = TRUE, decreasing = TRUE) #blanks

#some companies reporting 0 employees...valid

#Highly right-skewed
#could be transformed
boxplot(Company$Total.Employees) 
hist(Company$Total.Employees,
     xlim = c(0,100), breaks = 2000) 

##Explore variable: year.of.founding
sum(is.na(Company$Year.of.Founding))

sort(Company$Year.of.Founding, na.last = TRUE, decreasing = FALSE)
sort(Company$Year.of.Founding, na.last = TRUE, decreasing = TRUE) #blanks

#Left-Skewed, though not drastically
#could be transformed
boxplot(Company$Year.of.Founding) 
hist(Company$Year.of.Founding,
     xlim = c(1870,2021)) 

##Explore variable: primary.industry
sum(is.na(Company$Primary.Industry))

sort(Company$Primary.Industry, na.last = TRUE, decreasing = FALSE)
sort(Company$Primary.Industry, na.last = TRUE, decreasing = TRUE) #blanks

table(Company$Primary.Industry) 
Company %>% 
  count(Primary.Industry) %>% 
  arrange(-n)

Company %>% 
  filter(Is.Manufacturing == "No") %>% 
  count(Primary.Industry) %>% 
  #summarise(sum = sum(n))
  arrange(-n)

Company %>% 
  filter(Is.Manufacturing == "No") %>% 
  count(Primary.Industry) %>% 
  summarise(sum = sum(n))
  #arrange(-n)

Company %>% 
  filter(Is.Manufacturing == "No" & grepl("Wholesale",Primary.Industry)) %>% 
  count(Primary.Industry) %>% 
  summarise(sum = sum(n))

#Create new variable....is.wholesale
Company %>% 
  filter(grepl("Wholesale",Primary.Industry)) %>% 
  count(Primary.Industry) %>% 
  summarise(sum = sum(n))

Company %>% 
  filter(Is.Manufacturing == "No" & !grepl("Wholesale",Primary.Industry)) %>% 
  count(Primary.Industry) %>% 
  #summarise(sum = sum(n))
  arrange(-n)

##Explore variable: region
sum(is.na(Company$Region))

sort(Company$Region, na.last = TRUE, decreasing = FALSE)
sort(Company$Region, na.last = TRUE, decreasing = TRUE) #blanks and misspelling

table(Company$Region) #Balanced across five regions

