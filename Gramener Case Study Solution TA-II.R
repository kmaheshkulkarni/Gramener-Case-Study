###################################################################################################
#################################### Gramener Case Study Solution #################################

# loading the required libraries

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(scales)
library(Hmisc)
library(gridExtra)

options("scipen"=100, "digits"=4)

###################################################################################################

loan <- read.csv("loan.csv", header = T, stringsAsFactors = FALSE)
head(loan)
str(loan)

# Basic data cleaning

# Treating invalid values, any variable having more than 15% of data points
# missing is not eligible for imputation hence it makes sense to compute
# and drop those variables

missing_values <- loan %>%
  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()

# there are some reduntant features 
# finding out relevant/good features where the missing % < 15%

good_features <- filter(missing_values,missing_percentage<0.15)

good_features <- (good_features$feature) #the reason for deleting remaining features
# we cannot impute missing values with more than 15% obs

# Removing all the columns which have redundant information. 
loan <- loan[,(colnames(loan) %in% good_features)]

# Let's summarise the data

summary(loan)

# You can see that some variables such as "acc_now_delinq","chargeoff_within_12_mths","delinq_amnt"
# "pub_rec_bankruptcies","collections_12_mths_ex_med" & "tax_liens" have 
# large number of zeros. 

# "policy_code", "initial_list_status" and "pymnt_plan" have the same value for 
# all the observations. So these variables are not meaningful feature for the analysis
# Let's get rid of these variables from the analysis. 

red_variables <- c("acc_now_delinq","chargeoff_within_12_mths","pymnt_plan","initial_list_status","delinq_amnt","pub_rec_bankruptcies", "tax_liens","collections_12_mths_ex_med","policy_code")

loan <- loan[,!(colnames(loan) %in% red_variables)]

#count of NA values by column: just to verify if the NA values cleared
loan %>%
  summarise_all(funs(sum(is.na(.))))

sum(is.na(loan))
###################################################################################################
# Data Understanding: 

# If you glance through the meta data, you will find three types of variables: 
# 1. variables related to customers demographics 
# 2. variables related to the loan characteristics
# 3. variables related to the customers behaviour characteristic (after they were granted a loan).

## Customer's Demographic and application variables: 

# emp_title 
# emp_length
# home_ownership
# annual_inc
# verification_status
# addr_state
# zip_code
# title
# purpose
# desc
# url

## Loan related information & characteristics

# loan amount
# funded amount
# funded amount invested
# interest rate
# loan status
# loan grade
# loan sub-grade
# dti
# loan issue date
# loan term
# installment

## Credit information/ Customer Behaviour variables 

#  delinq_2yrs
#  earliest_cr_line
#  inq_last_6mths
#  open_acc
#  pub_rec
#  revol_bal
#  revol_util 
#  total_acc
#  out_prncp 
#  out_prncp_inv
# total_pymnt"             
# total_pymnt_inv
# total_rec_prncp
# total_rec_int 
# total_rec_late_fee 
# recoveries             
# collection_recovery_fee
# last_pymnt_d
# last_pymnt_amnt
# next_pymnt_d
# last_credit_pull_d
# application_type       


###################################################################################################
# Business Model 
# How does the lending process work?

# Business Objective
# The company wants to understand the driving factors behind default. 
# If one is able to identify these risky loan applicants,
# then such loans can be reduced thereby cutting down the amount of credit loss. 
# Identification of such applicants using EDA is the aim of this case study. 

###################################################################################################

# Now, think of mapping the business problem with the dataset.The variables related to the customer
# behaviour will not be available at the time of application. 
# Thus analysing these variables is not useful. 

# In general, want to understand the variables that are available
# at the time of the application stage, when only 
# the demographic and loan characteristic variables will be available. 

# So, going forward, we will do the analysis on the remaining two types of variables:

# 1. variables related to customer demographics
# 2. variables related to the loan characteristics

###################################################################################################
# So we remove the 3rd type of variables # 

behaviour_var<- c( 
  "delinq_2yrs",
  "earliest_cr_line",
  "inq_last_6mths",
  "open_acc",
  "pub_rec",
  "revol_bal",
  "revol_util",
  "total_acc",
  "out_prncp",
  "out_prncp_inv",
  "total_pymnt",
  "total_pymnt_inv",
  "total_rec_prncp",
  "total_rec_int",
  "total_rec_late_fee",
  "recoveries",
  "collection_recovery_fee",
  "last_pymnt_d",
  "last_pymnt_amnt",
  "next_pymnt_d",
  "last_credit_pull_d",
  "application_type")

# Let's remove the customer behaviour variables from the analysis. 
loan <- loan[,!(colnames(loan) %in% behaviour_var)]
View(loan)


###################################################################################################
# Data Cleaning
# int_rate - it should be numeric / percentage
str(loan$int_rate)

# Let's remove % from int_rate variable
# loan$int_rate <- extract_numeric(loan$int_rate)
loan$int_rate <- str_replace_all(loan$int_rate, "%", "")
loan$int_rate <- as.numeric(loan$int_rate)
summary(loan$int_rate)

####################################################################################################
# emp_title: It is a nominal variable having lots of blank values. 
# Let's remove this variable.
loan$emp_title <- NULL

####################################################################################################
# emp_length: Let's extract numeric values only
loan$emp_length <- extract_numeric(loan$emp_length)

####################################################################################################
# issue_date 
# Converting this variable to date format 

loan$issue_d <- paste("01-",loan$issue_d,sep="")
loan$issue_d <- as.Date(loan$issue_d,"%d-%B-%y")

####################################################################################################
# url 
head(loan$url)

# This variable is also a nomial variable, let's remove this variable as well
loan$url <- NULL

####################################################################################################
# desc : Description for loan application
head(loan$desc)
loan$desc_length <- str_count(loan$desc)
# Let's remove this as well
# loan$desc <- NULL
str(loan)

####################################################################################################
# Zip code & addr_state :Let's remove this variable as well
loan$zip_code <- NULL
loan$addr_state <- NULL

####################################################################################################
# title, It's also a nominal variable and purpose variable is derived from this variable Thus, it is better
# get rid of this variable
loan$title <- NULL

####################################################################################################
# Let's convert all the character type of variables to factor

loan[sapply(loan, is.character)] <- lapply(loan[sapply(loan, is.character)], 
                                           as.factor)

####################################################################################################
# Let's separate out the current status from the dataset. 

current_loan <- filter(loan,loan_status %in% c("Current"))
current_loan$loan_status <- factor(current_loan$loan_status)

# Let's consider Fully Paid & Charged Off levels in the loan variable
loan <- filter(loan,loan_status %in% c("Fully Paid","Charged Off"))
loan$loan_status <- factor(loan$loan_status)
summary(loan$loan_status)
table(loan$loan_status)[1]/nrow(loan)# approx. 14.6% default rate 

# Let's change "Charged Off" level to "1" and "Fully Paid" to "0"
loan$loan_status <- ifelse(loan$loan_status=="Charged Off",1,0)

# Yearly Application Distribution 
# Let's first create a derived metric "year"

loan$year <- as.factor(format(loan$issue_d,"%Y"))
univariate_categorical(loan,loan$year,"Application Distribution yearly")

#############################################################################
###### Business and Data Understanding - Some important things to understand

# Users and Products
# 1. What are the types of loans/products lending club offers? 
# How much does each contribute to the revenue?
loan %>% group_by(purpose) %>% summarise(n())
ggplot(loan, aes(x=purpose)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

principal_by_purpose <- loan %>% group_by(purpose) %>% 
  summarise(num=n(), total_principal = sum(funded_amnt, na.rm=T)) %>% 
  arrange(desc(total_principal))

principal_by_purpose

ggplot(principal_by_purpose, aes(x=purpose, y=total_principal))+
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

principal_by_purpose$fraction_lending <- 
  round(principal_by_purpose$total_principal / sum(principal_by_purpose$total_principal), 2)
principal_by_purpose

# Debt consolidation (53%), credit cards (14%), home improvement (8%) 
# and small business (5%) contribute to almost 80% of the business

# Hence, it makes sense to narrow down the analysis to these four products 

# Performance
# 1. What has been the overall default rate? 
# 2. What has been the default rate across products?  
# 3. Which time-period does the data belong to? Has the default rate varied over time?

# Overall default rate 
mean(loan$loan_status) #14.6%

# Default rate across products (purpose)
default_rates = loan %>% group_by(purpose) %>% 
  summarise(avg_default = round(mean(loan_status), 2)) %>%
  arrange(desc(avg_default))
default_rates

# small business has 27% default rate, debt-consolidation 15%, 
# home improvement 12%, credit cards 11% etc.



# Some products have high lending amount but lower default rates, and vice-versa.
# So how do you decide which products are more 'important'?
# We can take weighted average of % amount lent and default rate

principal_by_purpose

# If you look at the 'weighted effect' of amount lent and default rate:

metrics_by_purpose = merge(principal_by_purpose, default_rates, by="purpose")
metrics_by_purpose

metrics_by_purpose$net_impact <- 
  round((metrics_by_purpose$fraction_lending)*(metrics_by_purpose$avg_default), 2)

arrange(metrics_by_purpose, desc(net_impact))

# Debt consolidation contibutes the highest credit loss
# Credit cards are the second highest, home improvement the third highest
# This is expected, since about 57% of the business comes from 
# debt consolidation and credit card loans


# Thus, going forward, these few product types should be the focus of analysis
# Because reducing the debt-consolidatiion default rate even by a small amount
# would have a larger reduction in credit loss than other categories of loan purpose

loan$purpose <- as.character(loan$purpose)
loan <- filter(loan, 
               purpose == "debt_consolidation" | 
                 purpose == "credit_card" | 
                 purpose == "home_improvement")

loan$purpose <- factor(loan$purpose)
summary(loan$purpose)

#####################################################################################################
######################### Univariate Analysis ########################################

# Function for distribution of categorical variables (plotting bar charts)

univariate_categorical <- function(dataset,var,var_name){
  
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + 
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank()
    ) 
}

#####################################################################################################
# Demographic variables - income, home ownership status etc.

# employment length
univariate_categorical(loan,loan$emp_length,"Emp Length")
# two peaks - 1 (<1, >1) year and 10 years
# thus, approx. 40% applicants are either freshers or about 10 years work-exp. 

# We'll get a better idea if we compare the univariate plots across the three products
ggplot(loan, aes(x=emp_length))+geom_bar()+facet_wrap(~purpose)
# debt and CCs have a significant number of freshers applying, who are expectedly 
# less in home ownership
# This is, in a way, bivariate analysis (comparing trends across three types of sub-populations)

# facet for categorical vars
categorical_bivariate <- function(dataset, var, var_name){
  plot_bi = ggplot(dataset, aes(x=var))+geom_bar()+facet_wrap(~loan$purpose)
  return(plot_bi)
}

# facet for continuous vars
continuous_bivariate <- function(dataset, var, var_name){
  plot_cont = ggplot(dataset, aes(x=var))+geom_histogram()+facet_wrap(~loan$purpose)
  return(plot_cont)
}

categorical_bivariate(loan, loan$emp_length, "emp_length")

# Home ownership
categorical_bivariate(loan, loan$home_ownership, "home_ownership")
# most are mortgage and rent, roughly similar across the three categories

# annual income
continuous_bivariate(loan, loan$annual_inc, "annual_income")
# seems to contain outliers
summary(loan$annual_inc)
quantile(loan$annual_inc, seq(0, 1, 0.01))
# replacing outliers (95th percentile approx., only for better visualisation)
loan$annual_inc[which(loan$annual_inc > 140000)] <- 140000 
continuous_bivariate(loan, loan$annual_inc, "annual_income")
# the income distribution is roughly similar - with a peak at high incomes 

loan %>% group_by(purpose) %>% summarise(mean(annual_inc))
# CC and debt consold'n attract a slightly lower income group than home_improvement

# debt to income ratio
continuous_bivariate(loan, loan$dti, "DTI")
# we can compare the average DTI 
loan %>% group_by(purpose) %>% summarise(mean(dti))
# CC and debt consolidation seem to be attracting similar applicants

# Home improvement: high incomes, work experience, low DTI ratios 
# compared to CCs and debt consolidation


########  Loan characteristics
#####
#0. Purpose of loan 
univariate_categorical(loan,loan$purpose,"Purpose Distribution")
# we have already seen this

#1. Term Distribution
univariate_categorical(loan,loan$term,"Term Distribution")
# 36 months is the more common type
categorical_bivariate(loan, loan$term, "term")
# no 60 months in home improvement


#2. Grade Distribution
univariate_categorical(loan,loan$grade,"Grade Distribution")
categorical_bivariate(loan, loan$grade, "grade")

# #3. Sub-Grade Distribution
# univariate_categorical(loan,loan$sub_grade,"Sub-Grade Distribution")

# 4. Loan amount
continuous_bivariate(loan, loan$loan_amnt, "amnt")
loan %>% group_by(purpose) %>% summarise(median(loan_amnt))
# few loans  in home improvement exceed 20k dollars 

# Interest rate
continuous_bivariate(loan, loan$int_rate, "Interest")
loan %>% group_by(purpose) %>% summarise(mean(int_rate))
# debt consolidation has slightly higher interest rates

# Installment
continuous_bivariate(loan, loan$installment, "Instalment")
loan %>% group_by(purpose) %>% summarise(mean(installment))
# slightly lower monthly instalments in home_improvement


# Verification status distribution
univariate_categorical(loan,loan$verification_status,"verification_status Distribution")
prop.table(table(loan$verification_status))
# total 41% not verified

categorical_bivariate(loan, loan$verification_status, "verfication")
loan %>% group_by(purpose) %>% summarise(n())

# verified percentages
# credit cards 
tapply(loan$verification_status, loan$purpose, summary)[[1]]/tapply(loan$verification_status, loan$purpose, length)[1]

# debt
tapply(loan$verification_status, loan$purpose, summary)[[2]]/tapply(loan$verification_status, loan$purpose, length)[2]

# home improvement
tapply(loan$verification_status, loan$purpose, summary)[[3]]/tapply(loan$verification_status, loan$purpose, length)[3]

# Credit card loans have highest fraction of not verfied, followed by home ownership and debt

# Loan characteristic variables summary:
# Home improvement loans are relatively smaller loan amounts, lower installments and 
# lower interest rates
# Also, home improvement applicants have higher incomes and lower DTIs (lesser financial distress)
# compared to CCs and debt 


######## Segmented Univariate Analysis - Comparing default rates across variables########
# Analysing default rates separately for the three products
loan %>% group_by(purpose) %>% summarise(mean(loan_status))
# debt 15%, home 12%, CC 10%


segmented_defaults <- function(dataset,cat_var, var_name){
  a <- aggregate(loan_status~cat_var, dataset, mean)
  b <- data.frame(prop.table(table(cat_var))*100)
  b[,2] <- paste(round(b[,2], 2), "%", sep="")
  colnames(a)[1] <- var_name
  colnames(b)[1] <- var_name
  agg_default <- merge(a, b, by = var_name)
  agg_default <- data.frame(agg_default)
  colnames(agg_default) <- c(var_name, "Default","count")
  agg_default[, 2] <- round(agg_default[, 2], 2)
  agg_default <- arrange(agg_default, desc(Default))
  
  p.plot <- ggplot(agg_default, aes(agg_default[, 1], Default, label = count)) +
    geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_text(size = 3, vjust = -0.5) + xlab(var_name) 
  
  return(list((agg_default[1, 2] - agg_default[nrow(agg_default), 2]),p.plot))
  
}

# Demographic variables

# Employment length
# Creating three separate data frames
credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

# credit
segmented_defaults(credit, credit$emp_length, "Emp Length")
segmented_defaults(debt, debt$emp_length, "Emp Length")
segmented_defaults(home, home$emp_length, "Emp Length")

grid.arrange(segmented_defaults(credit, credit$emp_length, "Emp Length")[[2]],
             segmented_defaults(debt, debt$emp_length, "Emp Length")[[2]], 
             segmented_defaults(home, home$emp_length, "Emp Length")[[2]], 
             ncol = 3)

# In credit, medium work exp and 10 years applicants tend to default more 
# In debt, 10 years work exp default is extremely high
# In home imp, ~ 1 years work exp default more

## home ownership
grid.arrange(segmented_defaults(credit, credit$home_ownership, "Home")[[2]],
             segmented_defaults(debt, debt$home_ownership, "Home")[[2]], 
             segmented_defaults(home, home$home_ownership, "Home")[[2]], 
             ncol = 3)

# In credit and debt, there's almost no effect
# In home improvement, the 9% applicants who have rented houses default 15% 
# (the avg default of home-imp is only 12%)
# seems like people who live in rented houses tend to default, which makes sense
# One wouldn't be as willing to improve someone else's house 

## annual income
# we should bin the annual income into categories for better plots
summary(loan$annual_inc)

?cut
loan$binned_income = factor(cut(loan$annual_inc, breaks = seq(0, 140000, 20000)))

credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

grid.arrange(segmented_defaults(credit, credit$binned_income, "Income")[[2]],
             segmented_defaults(debt, debt$binned_income, "Income")[[2]], 
             segmented_defaults(home, home$binned_income, "Income")[[2]], 
             ncol = 3)
median(loan$annual_inc)

# 60k is the median income
# In credit cards, income between 20k and 40k dollars default the highest
# In debt, low income groups (< 60k) default more than 15%
# In home imp, low income groups (< 60k) default more than 15%

# DTI
summary(loan$dti)
loan$binned_dti = factor(cut(loan$dti, breaks = seq(0, 30, 5)))

credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

grid.arrange(segmented_defaults(credit, credit$binned_dti, "DTI")[[2]],
             segmented_defaults(debt, debt$binned_dti, "DTI")[[2]], 
             segmented_defaults(home, home$binned_dti, "DTI")[[2]], 
             ncol = 3)
mean(loan$dti)

# In credit, DTI > 15 is high risk (> 10% default)
# In debt, DTI > 15 is > 15% default rate
# In home imp, DTI > 15 is > 12.5% default


### Loan variables

# grade
grid.arrange(segmented_defaults(credit, credit$grade, "grade")[[2]],
             segmented_defaults(debt, debt$grade, "grade")[[2]], 
             segmented_defaults(home, home$grade, "grade")[[2]], 
             ncol = 3)

# as expected, As default less than Gs and Hs

# loan amount
summary(loan$loan_amnt)
loan$binned_amnt = factor(cut(loan$loan_amnt, breaks = seq(0, 35000, 5000)))

credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

grid.arrange(segmented_defaults(credit, credit$binned_amnt, "amount")[[2]],
             segmented_defaults(debt, debt$binned_amnt, "amount")[[2]], 
             segmented_defaults(home, home$binned_amnt, "amount")[[2]], 
             ncol = 3)
# no clear trend

# int rate
summary(loan$int_rate)
loan$binned_int = factor(cut(loan$int_rate, breaks = seq(5, 25, 2)))

credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

grid.arrange(segmented_defaults(credit, credit$binned_int, "int")[[2]],
             segmented_defaults(debt, debt$binned_int, "int")[[2]], 
             segmented_defaults(home, home$binned_int, "int")[[2]], 
             ncol = 3)
# In debt and home imp, there's a clear upward trend
# In credit, it is not that clear

summary(loan$installment)
loan$binned_instalment = factor(cut(loan$installment, breaks = seq(20, 1400, 100)))

credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

grid.arrange(segmented_defaults(credit, credit$binned_instalment, "instalment")[[2]],
             segmented_defaults(debt, debt$binned_instalment, "instalment")[[2]], 
             segmented_defaults(home, home$binned_instalment, "instalment")[[2]], 
             ncol = 3)
# no clear trends

# verification
grid.arrange(segmented_defaults(credit, credit$verification_status, "verification")[[2]],
             segmented_defaults(debt, debt$verification_status, "verfication")[[2]], 
             segmented_defaults(home, home$verification_status, "verification")[[2]], 
             ncol = 3)

# funny - not verified tend to default lesser than verified applicants

#####################################################################

##########################  Kshitij End    ###########################

