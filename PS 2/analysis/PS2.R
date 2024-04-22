#Problem Set 2
#Load Libraries
library(MASS)


#Problem 2
#Import Data
path.data <- "/Users/katieleinenbach/Downloads/"
df_hh_plan <- read.csv(file=paste(path.data, "household_plan_year777.csv",sep=""), header=T)
df_hh <- read.csv(file=paste(path.data, "households777.csv", sep=""), header = T)
df_plan <- read.csv(file=paste(path.data, "plans777.csv", sep=""), header = T)
df_iv <- read.csv(file=paste(path.data, "df_logit.csv", sep=""), header = T)

#Drop Columns
df_hh <- df_hh[ -c(1) ]
df_hh_plan <- df_hh_plan[ -c(1) ]
df_plan <- df_plan[ -c(1) ]

#Rename columns to merge
colnames(df_hh)[4] <- "plan_name"

#Part A
#Combine data 
df <- merge(df_hh_plan,df_plan,by="plan_name")
df_utility <- df %>% group_by(household_id,year)
df <- merge(df,df_hh,by=c("plan_name","household_id","year"))

#Calculate alpha values
iv <- ivreg(delta~ -1 + payment + Insurer + AV_CSR + HMO | hausman_iv + Insurer + AV_CSR + HMO, data = df_iv)
alpha <- iv$coefficients[1]

#Calculate premiums with penalty
df['premium_with_penalty'] <- df['premium'] - df['subsidy']
df['premium_with_penalty'][df['premium_with_penalty'] < 0] <- 0 
df['premium_with_penalty'] <- df['premium_with_penalty'] - df['monthly_penalty']

#Calculate premiums with penalty
df['premium_without_penalty'] <- df['premium'] - df['subsidy']
df['premium_without_penalty'][df['premium_without_penalty'] < 0] <- 0 

#Calculate change in payment
average_change_in_payment <- mean(df$premium_without_penalty - df$premium_with_penalty)
  
#Create insurer dummies
insurer_dummy <- dummy_cols(df$Insurer)
insurer_dummy <- insurer_dummy[ -c(1) ]
names(insurer_dummy) <- c('Anthem_dummy', 'Blue_Shield_dummy','Health_Net_dummy', 'Kaiser_dummy', 'Small_Insurer_dummy' )

#Add insurer dummies
df <- cbind(df,insurer_dummy)
  
#Calculate utility for chosen plans for household before and after mandate
df['utility_without_penalty'] <- alpha*df$premium_without_penalty + exp(iv$coefficients[2])*df$Anthem_dummy + exp(iv$coefficients[3])*df$Blue_Shield_dummy + exp(iv$coefficients[4])*df$Health_Net_dummy + exp(iv$coefficients[5])*df$Kaiser_dummy + exp(iv$coefficients[6])*df$Small_Insurer_dummy + iv$coefficients[7]*df$AV_CSR + iv$coefficients[8]*df$HMO
df['utility_with_penalty'] <- alpha*df$premium_with_penalty + exp(iv$coefficients[2])*df$Anthem_dummy + exp(iv$coefficients[3])*df$Blue_Shield_dummy + exp(iv$coefficients[4])*df$Health_Net_dummy + exp(iv$coefficients[5])*df$Kaiser_dummy + exp(iv$coefficients[6])*df$Small_Insurer_dummy + iv$coefficients[7]*df$AV_CSR + iv$coefficients[8]*df$HMO

average_change_in_utility <- mean(df$utility_without_penalty - df$utility_with_penalty)

#Determine if consumer would choose plan with new utility
df <- df %>% group_by(household_id) %>% mutate(choice = ifelse(utility_with_penalty == max(utility_with_penalty),1,0))
df <- df %>% group_by(household_id) %>% mutate(pred_choice = ifelse(utility_without_penalty == max(utility_without_penalty),1,0))

#Calculate new market share
df <- df %>% 
  group_by(rating_area,year) %>% 
  mutate(share=choice/sum(choice),pred_share=pred_choice/sum(pred_choice))
  
#Calculate averages by plan, rating area, and year 
df_summary <- df %>% group_by(plan_name,rating_area, year) %>% summarise(avg_premium_with_penalty=mean(premium_with_penalty),avg_premium_without_penalty=mean(premium_without_penalty), total_pred_share=sum(pred_share),total_share=sum(share), Metal_level=first(Metal_Level),Insurer=first(Insurer))

# Eliminate products with no share
df_summary$log_share <- log(df_summary$total_share)
df_summary <- df_summary[is.finite(df_summary$log_share),]

#Calculate the original marginal cost and D matrix with penalty
n <- length(df_summary$year)
D_with_penalty <- matrix(0,n,n)

for(i in 1:n){
  D_with_penalty[,i] <- -alpha*outer(df_summary$total_share,t(df_summary$total_share))[i,,]
}

diag(D_with_penalty) <- alpha * df_summary$total_share * (1-df_summary$total_share)
df_summary$mc_with_penalty <- df_summary$avg_premium_with_penalty + inv(D_with_penalty)%*%df_summary$total_share

#Calculate the new  D matrix without penalty
df_summary_predicted <- df_summary

# Eliminate products with no share
df_summary_predicted$log_pred_share <- log(df_summary_predicted$total_pred_share)
df_summary_predicted <- df_summary_predicted[is.finite(df_summary_predicted$log_pred_share),]

#Calculate the original marginal cost and D matrix with penalty
m <- length(df_summary_predicted$year)
D_without_penalty <- matrix(0,m,m)

for(i in 1:m){
  D_without_penalty[,i] <- -alpha*outer(df_summary_predicted$total_pred_share,t(df_summary_predicted$total_pred_share))[i,,]
}

diag(D_without_penalty) <- alpha * df_summary_predicted$total_pred_share * (1-df_summary_predicted$total_pred_share)

#Calculate prices assuming constant marginal costs
df_summary_predicted$new_premiums <- df_summary_predicted$mc_with_penalty - inv(D_without_penalty)%*%df_summary_predicted$total_pred_share

#Calculate change in price
change_in_price_from_repeal <- 100*(df_summary_predicted$new_premiums - df_summary_predicted$avg_premium_with_penalty)/df_summary_predicted$avg_premium_with_penalty
 
#Part B
#Find new penalty
minpositive = function(x) min(x[x > 0])
new_penalty <- minpositive(df_hh_plan$premium)

#Calculate alpha values
iv <- ivreg(delta~ -1 + payment + Insurer + AV_CSR + HMO | hausman_iv + Insurer + AV_CSR + HMO, data = df_iv)
alpha <- iv$coefficients[1]


#Create new dataframe for part b
df_new <- merge(df_hh_plan,df_plan,by="plan_name")
df_new <- merge(df_new,df_hh,by=c("plan_name","household_id","year"))


#Calculate premiums with penalty
df_new['premium_with_penalty'] <- df_new['premium'] - df_new['subsidy']
df_new['premium_with_penalty'][df_new['premium_with_penalty'] < 0] <- 0 
df_new['premium_with_penalty'] <- df_new['premium_with_penalty'] - df_new['monthly_penalty']

#Calculate premiums with penalty
df_new['premium_with_new_penalty'] <- df_new['premium'] - df_new['subsidy']
df_new['premium_with_new_penalty'][df_new['premium_with_new_penalty'] < 0] <- 0 
df_new['premium_with_new_penalty'] <- df_new['premium_with_new_penalty'] - new_penalty

#Calculate change in payment
average_change_in_payment_new <- mean(df_new$premium_with_new_penalty - df_new$premium_with_penalty)

#Create insurer dummies
insurer_dummy <- dummy_cols(df_new$Insurer)
insurer_dummy <- insurer_dummy[ -c(1) ]
names(insurer_dummy) <- c('Anthem_dummy', 'Blue_Shield_dummy','Health_Net_dummy', 'Kaiser_dummy', 'Small_Insurer_dummy' )

#Add insurer dummies
df_new <- cbind(df_new,insurer_dummy)

#Calculate utility for chosen plans for household before and after mandate
df_new['utility_with_new_penalty'] <- alpha*df_new$premium_with_new_penalty + exp(iv$coefficients[2])*df_new$Anthem_dummy + exp(iv$coefficients[3])*df_new$Blue_Shield_dummy + exp(iv$coefficients[4])*df_new$Health_Net_dummy + exp(iv$coefficients[5])*df_new$Kaiser_dummy + exp(iv$coefficients[6])*df_new$Small_Insurer_dummy + iv$coefficients[7]*df_new$AV_CSR + iv$coefficients[8]*df_new$HMO
df_new['utility_with_penalty'] <- alpha*df_new$premium_with_penalty + exp(iv$coefficients[2])*df_new$Anthem_dummy + exp(iv$coefficients[3])*df_new$Blue_Shield_dummy + exp(iv$coefficients[4])*df_new$Health_Net_dummy + exp(iv$coefficients[5])*df_new$Kaiser_dummy + exp(iv$coefficients[6])*df_new$Small_Insurer_dummy + iv$coefficients[7]*df_new$AV_CSR + iv$coefficients[8]*df_new$HMO

average_change_in_utility_new <- mean(df_new$utility_with_new_penalty - df_new$utility_with_penalty)

#Determine if consumer would choose plan with new utility
df_new <- df_new %>% group_by(household_id) %>% mutate(choice = ifelse(utility_with_penalty == max(utility_with_penalty),1,0))
df_new <- df_new %>% group_by(household_id) %>% mutate(pred_choice = ifelse(utility_with_new_penalty == max(utility_with_new_penalty),1,0))

#Calculate new market share
df_new <- df_new %>% 
  group_by(rating_area,year) %>% 
  mutate(share=choice/sum(choice),pred_share=pred_choice/sum(pred_choice))

#Calculate averages by plan, rating area, and year 
df_new_summary <- df_new %>% group_by(plan_name,rating_area, year) %>% summarise(avg_premium_with_penalty=mean(premium_with_penalty),avg_premium_with_new_penalty=mean(premium_with_new_penalty), total_pred_share=sum(pred_share),total_share=sum(share), Metal_level=first(Metal_Level),Insurer=first(Insurer))
 
# Eliminate products with no share
df_new_summary$log_share <- log(df_new_summary$total_share)
df_new_summary <- df_new_summary[is.finite(df_new_summary$log_share),]

#Calculate the original marginal cost and D matrix with penalty
k <- length(df_new_summary$year)
D_with_penalty <- matrix(0,k,k)

for(i in 1:k){
  D_with_penalty[,i] <- -alpha*outer(df_new_summary$total_share,t(df_new_summary$total_share))[i,,]
}

diag(D_with_penalty) <- alpha * df_new_summary$total_share * (1-df_new_summary$total_share)
df_new_summary$mc_with_penalty <- df_new_summary$avg_premium_with_penalty + inv(D_with_penalty)%*%df_new_summary$total_share

#Calculate the new  D matrix without penalty
df_new_summary_predicted <- df_new_summary

# Eliminate products with no share
df_new_summary_predicted$log_pred_share <- log(df_new_summary_predicted$total_pred_share)
df_new_summary_predicted <- df_new_summary_predicted[is.finite(df_new_summary_predicted$log_pred_share),]

#Calculate the original marginal cost and D matrix with penalty
j <- length(df_new_summary_predicted$year)
D_without_penalty <- matrix(0,j,j)

for(i in 1:j){
  D_without_penalty[,i] <- -alpha*outer(df_new_summary_predicted$total_pred_share,t(df_new_summary_predicted$total_pred_share))[i,,]
}

diag(D_without_penalty) <- alpha * df_new_summary_predicted$total_pred_share * (1-df_new_summary_predicted$total_pred_share)

#Calculate prices assuming constant marginal costs
df_new_summary_predicted$new_premiums <- df_new_summary_predicted$mc_with_penalty - inv(D_without_penalty)%*%df_new_summary_predicted$total_pred_share

#Calculate change in price
change_in_price_from_repeal_new <- 100*(df_new_summary_predicted$new_premiums - df_new_summary_predicted$avg_premium_with_penalty)/df_new_summary_predicted$avg_premium_with_penalty


#Problem 3
#Create Merge Data
df_summary$merge <- df_summary$Insurer
df_summary$merge<-replace(df_summary$merge, df_summary$merge=='Anthem','BCBS')
df_summary$merge<-replace(df_summary$merge, df_summary$merge=='Blue_Shield','BCBS')

#Calculate premiums before merger 
average_premium_before_merge<- df_summary %>% group_by(merge) %>% summarise(avg_premium_merge=mean(avg_premium_with_penalty))

#Calculate new market share
merge_values <- df_summary %>% group_by(year, rating_area, Metal_level, merge) %>% summarise(merge_mc = sum(mc_with_penalty), 
                                                                                  merge_share = sum(total_share),avg_premium_before_merge = mean(avg_premium_with_penalty))
#Remove values with no share
merge_values$log_share <- log(merge_values$merge_share)
merge_values <- merge_values[is.finite(merge_values$log_share),]

#Calculate new D Matrix for Merge
a <- length(merge_values$year)
D_merge <- matrix(0,a,a)

for(i in 1:a){
  D_merge[,i] <- -alpha*outer(merge_values$merge_share,t(merge_values$merge_share))[i,,]
}

diag(D_merge) <- alpha * merge_values$merge_share * (1-merge_values$merge_share)

#Calculate new premiums for merge
merge_values$premiums_after_merge <- merge_values$merge_mc - inv(D_merge)%*%merge_values$merge_share
average_premium_after_merge<- merge_values %>% group_by(merge) %>% summarise(avg_premium_after_merge=mean(premiums_after_merge))
change_in_premiums <- 100*(average_premium_after_merge$avg_premium_after_merge - average_premium_before_merge$avg_premium_merge)/average_premium_before_merge$avg_premium_merge

#Calculate MC
merge_values$mc_after_merge <- merge_values$avg_premium_before_merge + inv(D_merge)%*%merge_values$merge_share
mc_after_merge<- merge_values %>% group_by(merge) %>% summarise(mc_after_merge=mean(mc_after_merge))
mc_before_merge<- merge_values %>% group_by(merge) %>% summarise(mc_before_merge=mean(mc_merge))
mc_change <- 100*(mc_after_merge$mc_after_merge - mc_before_merge$mc_before_merge)/mc_before_merge$mc_before_merge
















  