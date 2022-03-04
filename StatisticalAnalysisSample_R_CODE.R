getwd()
merged <- read.csv("merged.csv",
                  header=TRUE, stringsAsFactors = FALSE,
                  na.strings = "NA")

dataset <- merged %>% 
  select(Customer.Number, CustomerName, log10_sales,
         Is.Importer, Is.Exporter, Is.Subsidiary,
         Location.Type, Is.Manufacturing,log10_revenue,
         log10_employees, Year.of.Founding_coded,
         Primary.Industry_coded, Region)

#Review missing data

#remove 3 salary fields that are blank
dataset<- dataset %>% 
  filter(!is.na(log10_sales))

dataset %>% 
  summarise(across(everything(),is.na))

dataset %>% 
  slice(1:6)

#Count of missing
rowsMiss<-transmute(rowwise(dataset[,4:13]),total = sum(across(everything(), is.na)))
table(rowsMiss)

#Where is bulk of missing?
sum(is.na(dataset$Customer.Number))
sum(is.na(dataset$CustomerName))
sum(is.na(dataset$log10_sales))
sum(is.na(dataset$Is.Importer))
sum(is.na(dataset$Is.Exporter))
sum(is.na(dataset$Is.Subsidiary))
sum(is.na(dataset$Location.Type_f))
sum(is.na(dataset$Is.Manufacturing_f))
sum(is.na(dataset$log10_revenue))
sum(is.na(dataset$log10_employees))
sum(is.na(dataset$Year.of.Founding_coded)) #97 Missing
sum(is.na(dataset$Primary.Industry_coded))
sum(is.na(dataset$Region))

plot(dataset$Year.of.Founding_coded, dataset$log10_sales)
#relationship between sales and year of founding - keep variable
cor.test(dataset$Year.of.Founding_coded, dataset$log10_sales)

#remove rows with missing data
dataset <- dataset[rowsMiss$total == 0,]

#Number of Contrasts 
table(dataset$Is.Importer,  useNA = "always") #1 Yes/no 
table(dataset$Is.Exporter,  useNA = "always") #1 Yes/no
table(dataset$Is.Subsidiary, useNA = "always") #1 Yes/no
table(dataset$Location.Type, useNA = "always") #2 Branch, headquarters, single_location
table(dataset$Is.Manufacturing, useNA = "always") #Yes/no
table(dataset$Primary.Industry_coded,  useNA = "always") #1 Manufactur, wholesale, other
table(dataset$Region) #4 for 5 regions

#Importer
dataset$Is.Importer_f <- factor(
  dataset$Is.Importer, levels = c("No", "Yes"))

levels(dataset$Is.Importer_f)

#Exporter
dataset$Is.Exporter_f <- factor(
  dataset$Is.Exporter, levels = c("No", "Yes"))

levels(dataset$Is.Exporter_f)

#Subsidiary
sort(dataset$Is.Subsidiary)

dataset$Is.Subsidiary_f <- factor(
  dataset$Is.Subsidiary, levels = c("FALSE", "TRUE"))

levels(dataset$Is.Subsidiary_f)

#Location Type
dataset$Location.Type_f <- factor(
  dataset$Location.Type, levels = c("SINGLE_LOCATION",# "BRANCH",
                                    "HEADQUARTERS"))
levels(dataset$Location.Type_f)

#Manufacturing
dataset$Is.Manufacturing_f <- factor(
  dataset$Is.Manufacturing, levels = c("No", "Yes"))

levels(dataset$Is.Manufacturing_f)

#Primary Industry
contrast_primary_industry_1 <- c(-2,1,1)
contrast_primary_industry_2 <- c(0,-1,1)

dataset$Primary_Industry_coded_f <- factor(
  dataset$Primary.Industry_coded, levels = c("Manufacturing", "Other",
                                    "Wholesale"))

levels(dataset$Primary_Industry_coded_f)

contrasts(dataset$Primary_Industry_coded_f) <- cbind(contrast_primary_industry_1,
                                                     contrast_primary_industry_2)

#Region
contrast_region_1 <- c(-4,1,1,1,1)
contrast_region_2 <- c(0,-3,1,1,1)
contrast_region_3 <- c(0,0,-2,1,1)
contrast_region_4 <- c(0,0,0,-1,1)

dataset$Region_f <- factor(
  dataset$Region, levels = c("The Wall", "Blackwater Bay",
                                    "Braavos", "Dorne", "King's Landing"))
levels(dataset$Region_f)

contrasts(dataset$Region_f) <- cbind(contrast_region_1,
                                     contrast_region_2,
                                     contrast_region_3,
                                     contrast_region_4)


#Check for multicollinearity
library(car)

model <- lm(log10_sales ~ Is.Importer_f + 
              Is.Exporter_f +
              Is.Subsidiary_f +
            #Is.Manufacturing_f +
              log10_revenue +
              log10_employees +
              Year.of.Founding_coded +
              Primary_Industry_coded_f +
              Region_f,  dataset)

vif(model)

#no cause for concern with VIF < 10

#Performing stepwise regression
variables <- dataset[,c(3,9,10,11,14,15,16,17,19,20)]

intercept_only <- lm(log10_sales ~ 1, variables)

model <- lm(log10_sales ~ .,  variables)

summary(model)

result <- step(intercept_only, direction = "both", 
               scope = formula(model),
                trace = 1)

result$anova
summary(result)
log(2.72)
##High value clients

hist(variables$log10_sales)
boxplot(variables$log10_sales)

value <- ifelse(
              variables$log10_sales <= quantile(variables$log10_sales, probs = 0.75),
              1,2)

variables$value_i <- value
table(variables$value_i)
dataset
#variation in sales
sort(variables[variables$value_i ==1,1])
variables_value <- variables[,2:11]
10^6.3

library(ISLR)

summary(variables_value$value_i)
sort(variables_value$value_i)

names(variables_value)

log_intercept_only <- glm(value_i ~ 1, data = variables_value)

logistic_model <- glm(value_i ~ ., data = variables_value)

log_result <- step(log_intercept_only, direction = "both", 
               scope = formula(logistic_model),
               trace = 1)

#odds ratios
2.71 ^ .082

#testing overall model significance
#https://www.r-bloggers.com/2015/08/evaluating-logistic-regression-models/
log_resunulldeviance <- 105.062
residualdeviance <- 61.935

anova(log_intercept_only, log_result)
1-pchisq(nulldeviance - residualdeviance, df = 11) #overall model is significant
summary(log_result)

library(pscl)
pR2(log_result)

#Visuals
library(ggplot2)

variables$Is.Importer_i <- ifelse(
  variables$Is.Importer_f == "No",
  0,1)

variables$Is.Exporter_i <- ifelse(
  variables$Is.Exporter_f == "No",
  0,1)

variables$Is.Subsidiary_i <- ifelse(
  variables$Is.Subsidiary_f == "FALSE",
  0,1)

variables$Location_Type_i <- ifelse(
  variables$Location.Type_f == "SINGLE_LOCATION",
  0,1)

variables$Primary_Industry_coded_i1 <- ifelse(
  variables$Primary_Industry_coded_f == "Manufacturing", 0,
 1)

variables$Primary_Industry_coded_i2 <- ifelse(
  variables$Primary_Industry_coded_f == "Other", 0, 
  ifelse(
      variables$Primary_Industry_coded_f == "Wholesale",
  1,2))

variables$Primary_Industry_coded_i2 <- ifelse(
  variables$Primary_Industry_coded_f == "Other", 0, 
  ifelse(
    variables$Primary_Industry_coded_f == "Wholesale",
    1,2))

variables$Region_i1 <- ifelse(
  variables$Region_f == "The Wall", 0,
  1)

variables$Region_i2 <- ifelse(
  variables$Region_f == "Blackwater Bay", 0,
  ifelse(
      variables$Region_f != "The Wall",
    1, 2
  )
  )

variables$Region_i3 <- ifelse(
  variables$Region_f == "Braavos", 0,
  ifelse(
      variables$Region_f != "The Wall" &
      variables$Region_f != "Blackwater Bay",
    1, 2
  )
)

variables$Region_i4 <- ifelse(
  variables$Region_f == "Dorne", 0,
  ifelse(
    variables$Region_f != "The Wall" &
      variables$Region_f != "Blackwater Bay" &
      variables$Region_f != "Braavos",
    1, 2
  )
)


plot(variables$log10_revenue,variables$log10_sales)
abline(model_revenue)
plot(variables$log10_employees,variables$log10_sales)
abline(model_employees)
plot(variables$Year.of.Founding_coded,variables$log10_sales)
abline(model_Year)
plot(variables$Is.Importer_i,variables$log10_sales)
abline(model_import)
plot(variables$Is.Exporter_i,variables$log10_sales)
abline(model_export)
plot(variables$Is.Subsidiary_i,variables$log10_sales)
abline(model_subsid)
plot(variables$Location_Type_i,variables$log10_sales)
abline(model_location)
plot(variables$Primary_Industry_coded_i1,variables$log10_sales)
abline(model_industry1)
plot(variables[variables$Primary_Industry_coded_i2 <2,19],
     variables[variables$Primary_Industry_coded_i2 <2,1])
abline(model_industry2)
plot(variables$Region_i1,variables$log10_sales)
abline(model_region1)
plot(variables[variables$Region_i2 <2,21],
     variables[variables$Region_i2 <2,1])
abline(model_region2)
plot(variables[variables$Region_i3 <2,22],
     variables[variables$Region_i3 <2,1])
abline(model_region3)
plot(variables[variables$Region_i4 <2,23],
     variables[variables$Region_i4 <2,1])
abline(model_region4)

model_revenue <- lm(variables$log10_sales ~ variables$log10_revenue)
summary(model_revenue)
model_employees <- lm(variables$log10_sales ~ variables$log10_employees)
summary(model_employees)
model_year <- lm(variables$log10_sales ~ variables$Year.of.Founding_coded)
summary(model_year)
model_import <- lm(variables$log10_sales ~ variables$Is.Importer_i)
summary(model_import)
model_export <- lm(variables$log10_sales ~ variables$Is.Exporter_i)
summary(model_export)
model_subsid <- lm(variables$log10_sales ~ variables$Is.Subsidiary_i)
summary(model_subsid)
model_location <- lm(variables$log10_sales ~ variables$Location_Type_i)
summary(model_location)
model_region1 <- lm(variables$log10_sales ~ variables$Region_i1)
summary(model_region1)
model_region2 <- lm(variables$log10_sales ~ variables$Region_i2)
summary(model_region2)
model_region3 <- lm(variables$log10_sales ~ variables$Region_i3)
summary(model_region3)
model_region4 <- lm(variables$log10_sales ~ variables$Region_i4)
summary(model_region4)






