#------------------------------Data processing phase-------------------------------
#1- 
padattable<-read.table(file.choose(),sep = "|",header = TRUE)

#2- 
#used to convert the data to categorical data.

loan_type<- as.factor(padattable$loan_type)
loan_purpose<-as.factor(padattable$loan_purpose)
preaproval<-as.factor(padattable$preapproval)
actoin_type<-as.factor(padattable$action_type)
country_Name<- as.factor(padattable$county_name)
Application_ethin<- as.factor(padattable$applicant_ethnicity)
Applicant_Race_1<-as.factor(padattable$applicant_race_1)
Applicant_Sex<- as.factor(padattable$applicant_sex)
Lien_Status<- as.factor(padattable$lien_status)


plot(loan_type)
plot(loan_purpose) 
plot(preaproval)
plot(actoin_type) 

#3-///////////////////////

var<-subset(padattable,padattable$county_name =="Adams")
var2<-subset(var,var$action_type=="Originated")
length(var2$loan_type)

# num of accepted people  [2661]

#4-//////////////////


padattable$number_of_owner_occupied_units<-as.numeric(levels(padattable$number_of_owner_occupied_units))[padattable$number_of_owner_occupied_units]

padattable$tract_to_msamd_income_pct<-as.numeric(levels(padattable$tract_to_msamd_income_pct))[padattable$tract_to_msamd_income_pct]

padattable$hud_median_family_income<-as.numeric(levels(padattable$hud_median_family_income))[padattable$hud_median_family_income]

padattable$minority_population_pct<-as.numeric(levels(padattable$minority_population_pct))[padattable$minority_population_pct]

padattable$applicant_income_ink<-as.numeric(levels(padattable$applicant_income_ink))[padattable$applicant_income_ink]

padattable$rate_spread<-as.numeric(levels(padattable$rate_spread))[padattable$rate_spread]

#//////////function to get the class of attribute
NumericFunction<-function(x)class(x)
NumericFunction(padattable$applicant_income_ink)



#5-////////////////

padattable=subset.data.frame(padattable,is.na(padattable$applicant_income_ink)==FALSE)

View(padattable)


#6-//////////////////

##remove Nulls from ##Tract_To_MSAMD_Income_pct

padattable=subset.data.frame(padattable,is.na(padattable$tract_to_msamd_income_pct)==FALSE)

## remove nulls from ##Minority_Population_pct

padattable=subset.data.frame(padattable,is.na(padattable$minority_population_pct)==FALSE)

#------------------------------------------------------------------------------------------------------------------------------------------------------

#------------------------------------Data Analysis phase ---------------------------------------------




#summary(padattable$loan_amount_ink)
#1--

plot(as.factor(padattable$loan_purpose))
summary(padattable$loan_purpose)
# we have 3 categories of loan_purpose
#most of loan_purpose is Refinancing with count of  286020
#meduim count of Home purchase with 99558
#less of loan_purpose is Home improvement with count of 42389

plot(as.factor(padattable$preapproval))
summary(padattable$preapproval)
# we have 3 categories of preapproval
#most of preapproval is Not applicable with count of  377458 
#meduim count of Home purchase with 44240 
#less of loan_purpose is Home improvement with count of  6269 

plot(as.factor(padattable$action_type))
summary(padattable$action_type)
# we have 4 categories of action_type
#most of action_type is Originated with count of  277025
#meduim count of Denied with 81631
#meduim count of Withdrawn with 48036
#less of action_type is Approved Not Accepted  with count of  21275 


plot(as.factor(padattable$applicant_ethnicity))
summary(padattable$applicant_ethnicity)
# we have 4 categories of applicant_ethnicity
#most of applicant_ethnicity is Non Hispanic.Latino  with count of  371402
#less of applicant_ethnicity is No Info with count of  47954
#less of applicant_ethnicity is Hispanic.Latino   with count of  8562
#less of applicant_ethnicity is  Not Applicable  with count of  49


plot(as.factor(padattable$applicant_sex))
summary(padattable$applicant_sex)
# we have 4 categories of applicant_sex
#most of applicant_sex is Male  with count of  288126
#meduim count of Female with 111852
#less of applicant_sex  is No Info   with count of 27935
#less of applicant_sex  is  Not Applicable  with count of 54

##2-

cor(padattable$applicant_income_ink,padattable$loan_amount_ink)

##--> 0.4058973
## a little relathionship


##3-
plot(density(padattable$loan_amount_ink))
# we need only one odd distribution
#4- 
barplot(table(padattable$loan_purpose) ,table( padattable$loan_amount_ink))
# the relation between loan purpose and loan amount 
#we need to remove home improvement of loan purpose


#5-

library(MASS) 


with(padattable,{hist(padattable$loan_amount_ink, main="Ditribution of loan amount inc",freq = FALSE)
  lines(density(padattable$loan_amount_ink), lty=2, lwd=2) 
  xvals = seq(from=min(padattable$loan_amount_ink), to=max(padattable$loan_amount_ink), length=100) 
  param = fitdistr(padattable$loan_amount_ink, "lognormal") 
  lines( xvals, dlnorm(xvals, meanlog=param$estimate[1], sdlog=param$estimate[2]), col="blue") 
})







      