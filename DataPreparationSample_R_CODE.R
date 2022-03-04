library(tidyverse)

#setwd()

#Read data in and recode non-alphanumeric as NULL
getwd()
sales <- read.csv("Sales.csv",
                  header=TRUE, stringsAsFactors = FALSE,
                  na.strings = c("NA",NA, NULL, "","."))

company <- read.csv("ExternalCompanyData.csv",
                    header=TRUE, stringsAsFactors = FALSE,
                    na.strings = c("NA",NA, NULL, "","."))

#Remove exact duplicates
sales <- distinct(sales)
company <- distinct(company)

#summarize
summary(sales)
summary(company)

sales %>% 
  count(CustomerName) %>% 
  filter(n > 1) %>% 
  arrange(-n) 

dups<- company %>% 
  count(FirmID) %>% 
  filter(n > 1) %>% 
  arrange(-n) 

company %>% 
  filter(FirmID %in% dups$FirmID)

#join data sets

sales$CustomerName_format <- tolower(gsub("[^A-Za-z0-9]","",sales$CustomerName))
company$Firm.Name_format <- tolower(gsub("[^A-Za-z0-9]","",company$Firm.Name))

merged<- sales %>% 
  left_join(company, by = c("CustomerName_format" = "Firm.Name_format"),
            keep = TRUE) %>% 
  arrange("CustomerName")

#pulling rows for companies with number ids but no names
num4missingname<- merged %>% 
  filter(is.na(CustomerName)) %>% 
  select(Customer.Number)

temp <- sales %>% 
  filter(Customer.Number %in% num4missingname$Customer.Number) %>% 
  left_join(company, by = c("Customer.Number"="FirmID"),
            keep = TRUE) %>%   
  arrange(CustomerName)

final <- rbind(
  merged %>% filter(!is.na(CustomerName)),
  temp 
)

#check
final %>% 
  filter(is.na(CustomerName)) %>% 
  select(Customer.Number, CustomerName, Is.Importer, Sales)

merged <- final

remove(final)
length(merged$Customer.Number)
#transformations on continuous variables

#sales
range(merged$Sales, na.rm=TRUE)
a<-sqrt(merged$Sales) #skewed
b<-log10(merged$Sales) # looks normal
c<-1/merged$Sales #skewed

hist(merged$Sales, xlim = c(0,200000), breaks = 200)
boxplot(merged$Sales)
hist(a)
hist(b)
hist(c)

merged$log10_sales <- log10(merged$Sales)

remove(a)
remove(b)
remove(c)

#revenue
range(merged$Revenue..US.Dollars..million., na.rm=TRUE)
d<-sqrt(merged$Revenue..US.Dollars..million.) 
e<-log10(merged$Revenue..US.Dollars..million.) #normal
f<-1/merged$Revenue..US.Dollars..million. 

hist(merged$Revenue..US.Dollars..million.,
      breaks = 200) 
hist(d)
hist(e)
hist(f)

merged$log10_revenue <- log10(merged$Revenue..US.Dollars..million.+1)
sort(merged$log10_revenue) #check for INF values
remove(d)
remove(e)
remove(f)

#employees
range(merged$Total.Employees, na.rm=TRUE)
g<-sqrt(merged$Total.Employees) 
h<-log10(merged$Total.Employees) #normalish
i<-1/merged$Total.Employees 

hist(merged$Total.Employees,
     breaks = 200) 
hist(g)
hist(h) #may have an outlier
boxplot(h)
hist(i)

merged$log10_employees <- log10(merged$Total.Employees+1)
sort(merged$log10_employees) #check for INF values
remove(g)
remove(h)
remove(i)

#founding
range(merged$Year.of.Founding, na.rm=TRUE)
j<-sqrt(merged$Year.of.Founding) 
k<-log10(merged$Year.of.Founding) #
l<-1/merged$Year.of.Founding 

hist(merged$Year.of.Founding) 
hist(j)
hist(k) #
hist(l)

#no transformation selected

remove(j)
remove(k)
remove(l)

#recode year of founding: all values < to 1950 as 1940
sort(merged$Year.of.Founding, decreasing = FALSE)
sort(merged$Year.of.Founding, decreasing = TRUE)

merged$Year.of.Founding_coded <- ifelse(
  merged$Year.of.Founding < 1950,
  1940,
  merged$Year.of.Founding
)

range(merged$Year.of.Founding_coded, na.rm=TRUE)

hist(merged$Year.of.Founding_coded) 


#Recode primary.industry into manufacturing, whole_sale, or other
review <- merged %>% 
  select(Primary.Industry, Primary.US.NAICS.Code) %>% 
  arrange(Primary.Industry, Primary.US.NAICS.Code) %>% 
  distinct()

wholesale<-merged %>% 
  filter(grepl("Wholesale",Primary.Industry)) %>% 
  select(Primary.Industry)

merged$Primary.Industry_coded <- ifelse(
  merged$Is.Manufacturing == "Yes", 
  "Manufacturing",
    ifelse(
      merged$Is.Manufacturing == "No" & grepl("Wholesale",merged$Primary.Industry)
      & !is.na(merged$Is.Manufacturing) & !is.na(merged$Primary.Industry),
      "Wholesale",
      "Other")
    )

sort(merged$Primary.Industry_coded, na.last = TRUE, decreasing = TRUE) #blanks

table(merged$Primary.Industry_coded, useNA = "always")


##Recode region: blackwater Bae

unique(merged$Region)

merged$Region <- gsub("Blackwater Bae", "Blackwater Bay", merged$Region)

unique(merged$Region)

#####Export table to csv#######
write.csv(merged, "merged.csv",
          quote=TRUE, na = "NA", row.names = FALSE)


















