#Project EMC
#1-	In RStudio , read the table you have created in Workflow1 
tbl=read.table(file.choose(),sep ="|",header = TRUE);
attach(tbl)
#, turn some of the codes into factors for those variables highlighted in Workflow1
#One of the most important uses of factors is in statistical 
#modeling; since categorical variables
#Factors are categorical variables that are super useful in summary statistics, plots,
#and regressions. 
loanTypefctr=as.factor(loan_type)
appEthnfctr=as.factor(applicant_ethnicity)
loanPurftr=as.factor(loan_purpose)
#Plot some of the variables you will create
plot(loanTypefctr)
plot(loanPurftr)
plot(appEthnfctr)

#3-	How many people were accepted in the Adams county ?4327
AdamsCounty=subset(tbl,county_name=="Adams")
accepted=subset(AdamsCounty,AdamsCounty$action_type=="Originated")
length(AdamsCounty$county_name)
length(accepted$county_name)
#as.numeric(tbl$applicant_income_ink)
tbl$rate_spread=as.numeric(as.character(tbl$rate_spread))
tbl$applicant_income_ink=as.numeric(as.character(tbl$applicant_income_ink))
tbl$number_of_owner_occupied_units=as.numeric(as.character(tbl$number_of_owner_occupied_units))
tbl$tract_to_msamd_income_pct=as.numeric(as.character(tbl$tract_to_msamd_income_pct))
tbl$hud_median_family_income=as.numeric(as.character(tbl$hud_median_family_income))
tbl$minority_population_pct=as.numeric(as.character(tbl$minority_population_pct))
tbl$hoepa_status=as.numeric(as.character(tbl$hoepa_status))
#. Make sure that they are numeric using a function
class(tbl$rate_spread)
class(tbl$applicant_income_ink)
class(tbl$number_of_owner_occupied_units)
class(tbl$tract_to_msamd_income_pct)
class(tbl$hud_median_family_income)
class(tbl$minority_population_pct)
class(tbl$hoepa_status)

#5-	Remove records without income info.
tbl=subset.data.frame(tbl,is.na(as.numeric(as.character(tbl$applicant_income_ink)))==FALSE)
sum(is.na(as.numeric(as.character(tbl$applicant_income_ink)))==TRUE) 

#6-	Remove rows with nulls (NA) in Tract_To_MSAMD_Income_pct, Minority_Population_pct
tbl$tract_to_msamd_income_pct=tbl$tract_to_msamd_income_pct[complete.cases(as.numeric(as.character(tbl$tract_to_msamd_income_pct)))]
tbl$minority_population_pct=tbl$minority_population_pct[complete.cases(as.numeric(as.character(tbl$minority_population_pct)))]
# sum(is.na(b)==TRUE) 
#or
tbl=subset.data.frame(tbl,is.na(as.numeric(as.character(tbl$tract_to_msamd_income_pct)))==FALSE)
tbl=subset.data.frame(tbl,is.na(as.numeric(as.character(tbl$minority_population_pct)))==FALSE)
sum(is.na(as.numeric(tbl.character(c$applicant_income_ink)))==TRUE) 

# -------- Data Analysis phase ----------

#1-	Visualize at least 5 variables and elaborate the results for each plot 
#++++++++++++++++++++++++++++++++
plot(tbl$action_type) 
#hist(as.numeric( tbl$loan_type))
#plot is right skewed so most of data located in the left

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
plot(tbl$loan_type)
#plot is left skewed so most of data located in the right

plot(tbl$applicant_sex)
#ana mesh 3aref hena hya right wla left
plot(tbl$county_name)
#ana mesh 3aref hena hya right wla left

plot(tbl$applicant_race_1)
#ana mesh 3aref hena hya right wla left

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
loan_amount = log10( tbl$loan_amount_ink)
library(MASS)
with (tbl , {hist ( loan_amount , main= "Loan Amount" , freq=FALSE) } )
# histogram is left skewed so most of data located in the right as mean wil be in the left of the peak
#++++++++++++++++++++++++++++++++++++++++++
applicant_income = as.numeric( tbl$applicant_income_ink)
library(MASS)
with (tbl , {hist ( applicant_income , main= "Loan Amount" , freq=FALSE) } )
# histogram is right skewed so most of data located in the right as mean wil be in the right of the peak


#2-	Is there a relation between the applicant income and the loan amount, elaborate? 
#there is no relation when use cor function as there is NA in app-income
with(tbl, cor(as.numeric(as.character(loan_amount_ink)), as.numeric(as.character(applicant_income_ink)) ))
#but here there is a cor with 0.4059191 value
DF <- data.frame(x =as.numeric( as.character(tbl$applicant_income_ink)), y = as.numeric(as.character(tbl$loan_amount_ink)))
DF=na.omit(DF)


#3- 
plot(density(tbl$loan_amount_ink))
#from plot lt's a uni-model

#4- 
hist (as.numeric(tbl$loan_purpose))
plot (tbl$loan_purpose)
# Home improvement has the least data 

#5-
lam = log10(tbl$loan_amount_ink)
plot (lam)
sample <- subset (tbl , tbl$loan_amount_ink < 4000 )
plot (sample$loan_amount_ink)
# We take log10 to make sure of the sample condition "4000"
# modo3 el models dh b2a mesh 3aref bsra7a :) 
