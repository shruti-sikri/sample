#Showing USA Map of NPS type using the Google Maps API
setwd('/Users/shruti/Desktop/Project')
library(ggmap)
library(tidyverse)

# dfentire <- read.csv("out-201412.csv", sep=",")
# dfny <- dfentire %>% 
#   filter(STATE_R == 'NY')
# 
# dfca <- dfentire %>% 
#   filter(STATE_R == 'CA')
# 
# dftx <- dfentire %>% 
#   filter(STATE_R == 'TX')
#   
# write_csv(dfny, 'NY.csv')
# write_csv(dfca, 'CA.csv')
# write_csv(dftx, 'TX.csv')

dfny <- read.csv("NY.csv", stringsAsFactors = F)
dfca <- read.csv("CA.csv", stringsAsFactors = F)
dftx <- read.csv("TX.csv", stringsAsFactors = F)

dfall<-rbind(dfny,dfca,dftx)

str(dfall)

dfprom<-dfall %>% 
  filter(NPS_Type=="Promoter")

dfpass<-dfall %>% 
  filter(NPS_Type=="Passive")

dfdetr<-dfall %>% 
  filter(NPS_Type=="Detractor")

#Correlation between price and likelihood of recommendation
dfny_linechart1<-dfny1
dfny_linechart1<-dfny_linechart1[-which(is.na(dfny_linechart1$Likelihood_Recommend_H)),]
length(dfny_linechart1)
#Refer page 131, Saltz textbook

#Number of people from different countries
dfall_country<-dfall1
dfall_country1<-as.data.frame(table(dfall_country$COUNTRY_CODE_R))
dfall_country1

dfall_country1<-dfall_country1[-97,] #Removing US, as it is too high and not helpful in my analysis
dfall_country1<-dfall_country1[order(dfall_country1$Freq),]
dfall_country1
barplot(dfall_country1$Freq,names.arg=dfall_country1$Var1,las=2,col=c("darkblue","red"))

#People from countries more likely to fill survey
dfall_survey<-dfall1[-which(is.na(dfall1$Survey_ID_H)),]
dfall_survey1<-as.data.frame(table(dfall_survey$COUNTRY_CODE_R))
dfall_survey1

dfall_survey1<-dfall_survey1[-47,] #Removing US, as it is too high and not helpful in my analysis
dfall_survey1<-dfall_survey1[order(dfall_survey1$Freq),]
dfall_survey1
barplot(dfall_survey1$Freq,names.arg=dfall_survey1$Var1,las=2,col=c("darkblue","red"))

#As we can see from the plots, we can see that 
#the people most inclined to fill surveys are predominantly from English speaking countries.
#The Dutch don't seem to be inclined to filling surveys, and thus we would recommend having a Dutch language survey for them.
#Also to service our East Asian customers better (particularly from Thailand, Japan, Singapore) we should make our survey in Thai, Japanese, and Chinese respectively.

#Linear models by state and country with respect to their likelihood of recommendation
state_likelihood <- lm(formula = dfall1$Likelihood_Recommend_H~dfall1$STATE_R, data = dfall1)
summary(state_likelihood)
plot(x=dfall1$Likelihood_Recommend_H,y=dfall1$STATE_R, na.rm=TRUE)
abline(state_likelihood)

country_likelihood <- lm(formula = dfall1$Likelihood_Recommend_H~dfall1$COUNTRY_CODE_R, data = dfall1)
summary(country_likelihood)

revenue_likelihood<-lm(formula = dfall1$Likelihood_Recommend_H~dfall1$REVENUE_USD_R, data = dfall1)
summary(revenue_likelihood)
#There seems to be very low (practically zero) correlation between how much a customer pays and how likely the customer is to recommend,
#looking at the R-squared value and p-value

#Number of visitors
#Checking out-
date_intervals_chkin<-dfall1[order(as.Date(dfall1$CHECK_OUT_DATE_C, format="%Y-%m-%d")),]
date_intervals_chkin
visitors_by_day_df1<-as.data.frame(table(date_intervals$CHECK_OUT_DATE_C))
visitors_by_day_df1
visitors_by_day_df1<-visitors_by_day_df[order(visitors_by_day_df$Var1),]
visitors_by_day_df1<-visitors_by_day_df[-c(1:7),]
visitors_by_day_df1
barplot(visitors_by_day_df1$Freq,names.arg=visitors_by_day_df1$Var1,las=2,col=c("darkblue","red"))

#Checking in-
date_intervals_chkout<-dfall1[order(as.Date(dfall1$CHECK_IN_DATE_C, format="%Y-%m-%d")),]
date_intervals_chkout
visitors_by_day_df2<-as.data.frame(table(date_intervals$CHECK_IN_DATE_C))
visitors_by_day_df2
visitors_by_day_df2<-visitors_by_day_df2[order(visitors_by_day_df2$Var1),]
visitors_by_day_df2<-visitors_by_day_df2[-c(1:6),]
visitors_by_day_df2
barplot(visitors_by_day_df2$Freq,names.arg=visitors_by_day_df2$Var1,las=2,col=c("darkblue","red"))

#Barplot for number of visitors per state
#NY-
dfny_hist<-dfny

dfny_hist<-dfny_hist[-which(is.na(dfny_hist$Guest_State_H)),]

hist_ny_states<-as.data.frame(table(dfny_hist$Guest_State_H))
hist_ny_states<-hist_ny_states[order(hist_ny_states$Freq),]
barplot(hist_ny_states$Freq, 
        names.arg =hist_ny_states$Var1, las=2,  
        main = "Number of Visitors to NYC" , 
        col=c("darkblue","red"))

#NY Without NY-
hist_ny_states<-hist_ny_states[1:(nrow(hist_ny_states)-1),]
barplot(hist_ny_states$Freq, names.arg =hist_ny_states$Var1,main= "Number of Visitors to NYC not from NY state" , las=2, col=c("darkblue","red"))

#LA-
dfla_hist<-dfla1
dfla_hist<-dfla_hist[-which(is.na(dfla_hist$Guest_State_H)),]

hist_la_states<-as.data.frame(table(dfla_hist$Guest_State_H))
hist_la_states<-hist_la_states[order(hist_la_states$Freq),]
barplot(hist_la_states$Freq, names.arg =hist_la_states$Var1,  main = "Number of Visitors to LA" ,las=2, col=c("darkblue","red"))

#LA Without CA-
hist_la_states<-hist_la_states[1:(nrow(hist_la_states)-1),]
barplot(hist_la_states$Freq, names.arg =hist_la_states$Var1,  main= "Number of Visitors to LA not from CA", las=2, col=c("darkblue","red"))

#CHI-
dfch_hist<-dfch1
dfch_hist<-dfch_hist[-which(is.na(dfch_hist$Guest_State_H)),]

hist_ch_states<-as.data.frame(table(dfch_hist$Guest_State_H))
hist_ch_states<-hist_ch_states[order(hist_ch_states$Freq),]
barplot(hist_ch_states$Freq, names.arg =hist_ch_states$Var1, main= "Number of Visitors to Chicago", las=2, col=c("darkblue","red"))

#CHI Without IL-
hist_ch_states<-hist_ch_states[1:(nrow(hist_ch_states)-1),]
barplot(hist_ch_states$Freq, names.arg =hist_ch_states$Var1, main= "Number of Visitors to Chicago not from Illinois", las=2, col=c("darkblue","red"))

#POV by age
pov_age <- table(dfall1$POV_CODE_C, dfall$Age_Range_H)
View(pov_age)
barplot(pov_age, main="Purpose of Visit by age",
        xlab="Age Ranges", col=c("darkblue","red"),
        legend = rownames(pov_age), beside=TRUE)


#aRules
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("grid")
library(grid)
ruleset<-apriori(dfall1, parameter=list (support=0.35,confidence=0.50))   #support and confidence set higher
summary(ruleset)
inspect(ruleset)


#Linear models - Guest satisfaction metrics v/s likelihood to recommend

lm_net_rev<-lm(formula = dfall1$Likelihood_Recommend_H~dfall1$Net_Rev_H, data = dfall1)
summary(lm_net_rev)
plot(x=dfall1$Likelihood_Recommend_H, y=dfall1$Net_Rev_H)

lm_sat_h<-lm(formula = dfall1$Likelihood_Recommend_H~dfall1$Overall_Sat_H, data = dfall1)
summary(lm_sat_h)
plot(x=dfall1$Overall_Sat_H, y=dfall1$Likelihood_Recommend_H)

lm_guest_room_h<-lm(formula = dfall1$Likelihood_Recommend_H~dfall1$Guest_Room_H, data = dfall1)
summary(lm_guest_room_h)
plot(y=dfall1$Likelihood_Recommend_H, x=dfall1$Guest_Room_H)

###---Working here----###
lm_tranq_h<-lm(formula = dfall1$Likelihood_Recommend_H~dfall1$Tranquility_H, data = dfall1)
summary(lm_tranq_h)
plot(y=dfall1$Likelihood_Recommend_H, x=dfall1$Tranquility_H)

table(dfall1$POV_CODE_C)
df_tranq<-dfall1[-which(dfall1$POV_CODE_C=="LEISURE"),]
View(df_tranq)
lm_tranq_h<-lm(formula = df_tranq$Likelihood_Recommend_H~df_tranq$Tranquility_H, data = df_tranq)
summary(lm_tranq_h)

df_tranq2<-dfall1[-which(dfall1$POV_CODE_C=="BUSINESS"),]
lm_tranq_h<-lm(formula = df_tranq2$Likelihood_Recommend_H~df_tranq2$Tranquility_H, data = df_tranq2)
summary(lm_tranq_h)

#Both Business and Leisure visitors don't like tranquility, as both have 37% and 38% correlation with LTR.

lm_cond_hotel<-lm(formula = dfall1$Likelihood_Recommend_H~dfall1$Condition_Hotel_H, data = dfall1)
summary(lm_cond_hotel)
plot(y=dfall1$Likelihood_Recommend_H, x=dfall1$Condition_Hotel_H)

lm_cust_svc<-lm(formula = dfall1$Likelihood_Recommend_H~dfall1$Customer_SVC_H, data = dfall1)
summary(lm_cust_svc)
plot(y=dfall1$Likelihood_Recommend_H, x=dfall1$Customer_SVC_H)

lm_staff_cared<-lm(formula = dfall1$Likelihood_Recommend_H~dfall1$Staff_Cared_H, data = dfall1)
summary(lm_staff_cared)
plot(y=dfall1$Likelihood_Recommend_H, x=dfall1$Staff_Cared_H)

lm_internet_sat<-lm(formula = dfall1$Likelihood_Recommend_H~dfall1$Internet_Sat_H, data = dfall1)
summary(lm_internet_sat)
plot(y=dfall1$Likelihood_Recommend_H, x=dfall1$Internet_Sat_H)

lm_checkin<-lm(formula = dfall1$Likelihood_Recommend_H~dfall1$Check_In_H, data = dfall1)
summary(lm_checkin)
plot(y=dfall1$Likelihood_Recommend_H, x=dfall1$Check_In_H)

##--WORK HERE--##
View(dfall1)
df_lm<-dfall1[,c("Likelihood_Recommend_H","Net_Rev_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H",
                 "Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H")]
View(df_lm)

lm_combined<-lm(formula=df_lm$Likelihood_Recommend_H~df_lm$Guest_Room_H+df_lm$Tranquility_H,data=df_lm)
summary(lm_combined)

#Use-64%
lm_combined<-lm(formula=df_lm$Likelihood_Recommend_H~df_lm$+df_lm$Condition_Hotel_H,data=df_lm)
summary(lm_combined)

#Use-69%
lm_combined<-lm(formula=df_lm$Likelihood_Recommend_H~df_lm$Guest_Room_H+df_lm$Customer_SVC_H,data=df_lm)
summary(lm_combined)
plot(y=df_lm$Likelihood_Recommend_H, x=df_lm$Guest_Room_H+df_lm$Customer_SVC_H)

#Use-67%
lm_combined<-lm(formula=df_lm$Likelihood_Recommend_H~df_lm$Guest_Room_H+df_lm$Staff_Cared_H,data=df_lm)
summary(lm_combined)

#Use-67%
lm_combined<-lm(formula=df_lm$Likelihood_Recommend_H~df_lm$Guest_Room_H+df_lm$Staff_Cared_H,data=df_lm)
summary(lm_combined)

#lm_combined<-lm(formula=df_lm$Likelihood_Recommend_H~df_lm$Condition_Hotel_H+df_lm$,data=df_lm)
#summary(lm_combined)

for(i in 1:ncol(df_lm)){
  for(j in 1:ncol(df_lm)){
    for(k in 1:ncol(df_lm)){
      lm_combined<-lm(formula=df_lm$Likelihood_Recommend_H~df_lm$(names(df_lm)[i])+df_lm$(names(df_lm)[j])+df_lm$(names(df_lm)[k]),data=df_lm)
      summary(lm_combined)
      names(df_lm)[i]
      names(df_lm)[j]
      names(df_lm)[k]
    }
  }
}
for(i in 1:2){
  lm_combined<-lm(formula=df_lm$Likelihood_Recommend_H~df_lm$(names(df_lm)[i]),data=df_lm)
  summary(lm_combined)
  names(df_lm)[i]
}

for(i in 1:ncol(df_lm)){
  lm_combined<-lm(formula=df_lm$Likelihood_Recommend_H~df_lm$names(df_lm)[i],data=df_lm)
  summary(lm_combined)
  print(names(df_lm)[i])
}

##--END--##

#Linear models from promoters-
lm_net_rev<-lm(formula = dfprom$Likelihood_Recommend_H~dfprom$Net_Rev_H, data = dfprom)
summary(lm_net_rev)
plot(x=dfprom$Likelihood_Recommend_H, y=dfprom$Net_Rev_H)

lm_sat_h<-lm(formula = dfprom$Likelihood_Recommend_H~dfprom$Overall_Sat_H, data = dfprom)
summary(lm_sat_h)
plot(x=dfprom$Overall_Sat_H, y=dfprom$Likelihood_Recommend_H)

lm_guest_room_h<-lm(formula = dfprom$Likelihood_Recommend_H~dfprom$Guest_Room_H, data = dfprom)
summary(lm_guest_room_h)
plot(y=dfprom$Likelihood_Recommend_H, x=dfprom$Guest_Room_H)

lm_tranq_h<-lm(formula = dfprom$Likelihood_Recommend_H~dfprom$Tranquility_H, data = dfprom)
summary(lm_tranq_h)
plot(y=dfprom$Likelihood_Recommend_H, x=dfprom$Tranquility_H)

lm_cond_hotel<-lm(formula = dfprom$Likelihood_Recommend_H~dfprom$Condition_Hotel_H, data = dfprom)
summary(lm_cond_hotel)
plot(y=dfprom$Likelihood_Recommend_H, x=dfprom$Condition_Hotel_H)

lm_cust_svc<-lm(formula = dfprom$Likelihood_Recommend_H~dfprom$Customer_SVC_H, data = dfprom)
summary(lm_cust_svc)
plot(y=dfprom$Likelihood_Recommend_H, x=dfprom$Customer_SVC_H)

lm_staff_cared<-lm(formula = dfprom$Likelihood_Recommend_H~dfprom$Staff_Cared_H, data = dfprom)
summary(lm_staff_cared)
plot(y=dfprom$Likelihood_Recommend_H, x=dfprom$Staff_Cared_H)

lm_internet_sat<-lm(formula = dfprom$Likelihood_Recommend_H~dfprom$Internet_Sat_H, data = dfprom)
summary(lm_internet_sat)
plot(y=dfprom$Likelihood_Recommend_H, x=dfprom$Internet_Sat_H)

lm_checkin<-lm(formula = dfprom$Likelihood_Recommend_H~dfprom$Check_In_H, data = dfprom)
summary(lm_checkin)
plot(y=dfprom$Likelihood_Recommend_H, x=dfprom$Check_In_H)

#Linear model - Passive

lm_net_rev<-lm(formula = dfpass$Likelihood_Recommend_H~dfpass$Net_Rev_H, data = dfpass)
summary(lm_net_rev)
plot(x=dfpass$Likelihood_Recommend_H, y=dfpass$Net_Rev_H)

lm_sat_h<-lm(formula = dfpass$Likelihood_Recommend_H~dfpass$Overall_Sat_H, data = dfpass)
summary(lm_sat_h)
plot(x=dfpass$Overall_Sat_H, y=dfpass$Likelihood_Recommend_H)

lm_guest_room_h<-lm(formula = dfpass$Likelihood_Recommend_H~dfpass$Guest_Room_H, data = dfpass)
summary(lm_guest_room_h)
plot(y=dfpass$Likelihood_Recommend_H, x=dfpass$Guest_Room_H)

lm_tranq_h<-lm(formula = dfpass$Likelihood_Recommend_H~dfpass$Tranquility_H, data = dfpass)
summary(lm_tranq_h)
plot(y=dfpass$Likelihood_Recommend_H, x=dfpass$Tranquility_H)

lm_cond_hotel<-lm(formula = dfpass$Likelihood_Recommend_H~dfpass$Condition_Hotel_H, data = dfpass)
summary(lm_cond_hotel)
plot(y=dfpass$Likelihood_Recommend_H, x=dfpass$Condition_Hotel_H)

lm_cust_svc<-lm(formula = dfpass$Likelihood_Recommend_H~dfpass$Customer_SVC_H, data = dfpass)
summary(lm_cust_svc)
plot(y=dfpass$Likelihood_Recommend_H, x=dfpass$Customer_SVC_H)

lm_staff_cared<-lm(formula = dfpass$Likelihood_Recommend_H~dfpass$Staff_Cared_H, data = dfpass)
summary(lm_staff_cared)
plot(y=dfpass$Likelihood_Recommend_H, x=dfpass$Staff_Cared_H)

lm_internet_sat<-lm(formula = dfpass$Likelihood_Recommend_H~dfpass$Internet_Sat_H, data = dfpass)
summary(lm_internet_sat)
plot(y=dfpass$Likelihood_Recommend_H, x=dfpass$Internet_Sat_H)

lm_checkin<-lm(formula = dfpass$Likelihood_Recommend_H~dfpass$Check_In_H, data = dfpass)
summary(lm_checkin)
plot(y=dfpass$Likelihood_Recommend_H, x=dfpass$Check_In_H)

#Linear model - Detractor
lm_net_rev<-lm(formula = dfdetr$Likelihood_Recommend_H~dfdetr$Net_Rev_H, data = dfdetr)
summary(lm_net_rev)
plot(x=dfdetr$Likelihood_Recommend_H, y=dfdetr$Net_Rev_H)

lm_sat_h<-lm(formula = dfdetr$Likelihood_Recommend_H~dfdetr$Overall_Sat_H, data = dfdetr)
summary(lm_sat_h)
plot(x=dfdetr$Overall_Sat_H, y=dfdetr$Likelihood_Recommend_H)

lm_guest_room_h<-lm(formula = dfdetr$Likelihood_Recommend_H~dfdetr$Guest_Room_H, data = dfdetr)
summary(lm_guest_room_h)
plot(y=dfdetr$Likelihood_Recommend_H, x=dfdetr$Guest_Room_H)

lm_tranq_h<-lm(formula = dfdetr$Likelihood_Recommend_H~dfdetr$Tranquility_H, data = dfdetr)
summary(lm_tranq_h)
plot(y=dfdetr$Likelihood_Recommend_H, x=dfdetr$Tranquility_H)

lm_cond_hotel<-lm(formula = dfdetr$Likelihood_Recommend_H~dfdetr$Condition_Hotel_H, data = dfdetr)
summary(lm_cond_hotel)
plot(y=dfdetr$Likelihood_Recommend_H, x=dfdetr$Condition_Hotel_H)

lm_cust_svc<-lm(formula = dfdetr$Likelihood_Recommend_H~dfdetr$Customer_SVC_H, data = dfdetr)
summary(lm_cust_svc)
plot(y=dfdetr$Likelihood_Recommend_H, x=dfdetr$Customer_SVC_H)

lm_staff_cared<-lm(formula = dfdetr$Likelihood_Recommend_H~dfdetr$Staff_Cared_H, data = dfdetr)
summary(lm_staff_cared)
plot(y=dfdetr$Likelihood_Recommend_H, x=dfdetr$Staff_Cared_H)

lm_internet_sat<-lm(formula = dfdetr$Likelihood_Recommend_H~dfdetr$Internet_Sat_H, data = dfdetr)
summary(lm_internet_sat)
plot(y=dfdetr$Likelihood_Recommend_H, x=dfdetr$Internet_Sat_H)

lm_checkin<-lm(formula = dfdetr$Likelihood_Recommend_H~dfdetr$Check_In_H, data = dfdetr)
summary(lm_checkin)
plot(y=dfdetr$Likelihood_Recommend_H, x=dfdetr$Check_In_H)


install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("grid")
library(grid)

#Scatter plot
scatterplot_revenue_length_of_stay<-plot(dfall1$Length_Stay_H, dfall1$REVENUE_USD_R, main="Revenue v/s Length of Stay", 
                                         xlab="Length of Stay ", ylab="Revenue ", pch=10,col=c("blue"))

#Association rules for guest satisfaction metrics

arules_df<- dfall1[,c(24:31,33)]

na.omit(arules_df)

str(arules_df)

arules_df$NPS_Type <- as.factor(arules_df$NPS_Type)

arules_df$Overall_Sat_H <- as.factor(arules_df$Overall_Sat_H)

arules_df$Guest_Room_H <- as.factor(arules_df$Guest_Room_H)

arules_df$Tranquility_H <- as.factor(arules_df$Tranquility_H)

arules_df$Condition_Hotel_H <- as.factor(arules_df$Condition_Hotel_H)

arules_df$Customer_SVC_H <- as.factor(arules_df$Customer_SVC_H)

arules_df$Staff_Cared_H <- as.factor(arules_df$Staff_Cared_H)

arules_df$Internet_Sat_H <- as.factor(arules_df$Internet_Sat_H)

arules_df$Check_In_H <- as.factor(arules_df$Check_In_H)

names(arules_df)

guestsatisfy <-  apriori(arules_df,parameter = list(support=.02,confidence=.5))
summary(guestsatisfy)
inspect(guestsatisfy)

# Likelihood to recommend with 7 variables
# Overall satisfaction, Guest Room Satisfaction, Tranquility, Condition of hotel,
# Quality of customer service, Staff cared, Internet satisfaction, Quality of check in
lm.hotel<-lm(data = df.final, formula = df.final$Likelihood_Recommend_H~
               df.final$Overall_Sat_H+df.final$Overall_Sat_H+df.final$Guest_Room_H
             +df.final$Condition_Hotel_H+df.final$Customer_SVC_H+df.final$Staff_Cared_H
             +df.final$Internet_Sat_H+df.final$Tranquility_H)
summary(lm.hotel)
# p-value for this model is less than 2.2e-16, which explains the model is significant.
#Also, the R-square value is 0.7994 so that this model explains about 80% of the variations.
#Most predictors are significant except "Internet_Sat_H".

# Association Rules with all of the variables, which some of them needed to turn into factor
df.final$NPS_Type[which(df.final$NPS_Type=="")]<-NA
index<-which(is.na(df.final$NPS_Type)==TRUE)
df.final<-df.final[-index,]

df.final[sapply(df.final, is.integer)] <- lapply(df.final[sapply(df.final, is.integer)], as.factor)
df.final[sapply(df.final, is.numeric)] <- lapply(df.final[sapply(df.final, is.numeric)], as.factor)
df.final[sapply(df.final, is.character)] <- lapply(df.final[sapply(df.final, is.character)], as.factor)

dff<-apriori(df.final)
summary(dff)
dfff<-inspect(dff)
str(dff)
goodrules<-dff[order(-dff@quality$lift),]
plot(goodrules)
plot(goodrules)
plot(goodrules,main="Scatter Plot for 251,662 Association Rules")
inspect(head(goodrules,50))
# In 25th result, STATE_R=IL is 6.68 times more tendency to be with NPS_Type=Promoter, COUNTRY_CODE_R=UNITED STATES, Guest_State_H=IL,
#which means people in IL prefer spending time in Chicago to New York or Los Angeles.


# sVM with "NPS_Type", "Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H"
install.packages("kernlab")
library(kernlab)
install.packages("e1071")
library("e1071")
install.packages("ggplot2")
library("ggplot2")
df.fake<- dfall1

df.fake <- df.fake[df.fake$NPS_Type!="",] 

df.fake$Overall_Sat_H[which(is.na(df.fake$Overall_Sat_H)=="TRUE")]<-round(mean(df.fake$Overall_Sat_H,na.rm=TRUE),2)
df.fake$Guest_Room_H[which(is.na(df.fake$Guest_Room_H)=="TRUE")]<-round(mean(df.fake$Guest_Room_H,na.rm=TRUE),2)
df.fake$Condition_Hotel_H[which(is.na(df.fake$Condition_Hotel_H)=="TRUE")]<-round(mean(df.fake$Condition_Hotel_H,na.rm=TRUE),2)
df.fake$Customer_SVC_H[which(is.na(df.fake$Customer_SVC_H)=="TRUE")]<-round(mean(df.fake$Customer_SVC_H,na.rm=TRUE),2)
df.fake$Staff_Cared_H[which(is.na(df.fake$Staff_Cared_H)=="TRUE")]<-round(mean(df.fake$Staff_Cared_H,na.rm=TRUE),2)
df.fake$Tranquility_H[which(is.na(df.fake$Tranquility_H)=="TRUE")]<-round(mean(df.fake$Tranquility_H,na.rm=TRUE),2)

index<-c("NPS_Type","Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H")
df.fake<-df.fake[,index]

randIndex<-sample(1:dim(df.fake)[1])
summary(randIndex)
length(randIndex)
head(randIndex)
cutPoint2_3<-floor(2*dim(df.fake)[1]/3)
cutPoint2_3
trainData<-df.fake[randIndex[1:cutPoint2_3],]
testData<-df.fake[randIndex[(cutPoint2_3+1):dim(df.fake)[1]],]

goodOzone<-ifelse(trainData$NPS_Type=="Promoter",2,
                  ifelse(trainData$NPS_Type=="Passive",1,0))
trainData<-data.frame(trainData,goodOzone)
goodOzone<-ifelse(testData$NPS_Type=="Promoter",2,
                  ifelse(trainData$NPS_Type=="Passive",1,0))
testData<-data.frame(testData,goodOzone)

trainData$goodOzone <- as.factor(trainData$goodOzone)
testData$goodOzone <- as.factor(testData$goodOzone)
str(trainData)

svmGood<-ksvm(goodOzone~., data=trainData, kernel="rbfdot", kpar="automatic", C=5, cross=13, prob.model=TRUE)

goodPred <- predict(svmGood, testData)
compGood1 <- data.frame(testData[,8], goodPred)
colnames(compGood1) <- c("test","Pred")
perc_ksvm <- length(which(compGood1$test==compGood1$Pred))/dim(compGood1)[1]
perc_ksvm

compGood1$correct <- ifelse(compGood1$test==compGood1$Pred,"correct","wrong")
Plot_ksvm <- data.frame(compGood1$correct,testData$Customer_SVC_H,testData$Staff_Cared_H,testData$goodOzone,compGood1$Pred)
colnames(Plot_ksvm) <- c("correct","Customer_SVC_H","Staff_Cared_H","goodOzone","Predict")
ggplot.ksvm<-ggplot(Plot_ksvm, aes(x=Customer_SVC_H,y=Staff_Cared_H)) + geom_point(aes(size=correct,color=goodOzone,shape = Predict))

## nb
nb <- naiveBayes(goodOzone~., data=trainData)
Pred_nb <- predict(nb, testData)
compNb <- data.frame(testData[,8], Pred_nb)
colnames(compNb) <- c("test","Pred")
perc_nb <- length(which(compNb$test==compNb$Pred))/dim(compNb)[1]
perc_nb

compNb$correct <- ifelse(compNb$test==compNb$Pred,'correct','wrong')
Plot_nb <- data.frame(compNb$correct,testData$Customer_SVC_H,testData$Staff_Cared_H,testData$goodOzone,compNb$Pred)
colnames(Plot_nb) <- c("correct","Customer_SVC_H","Staff_Cared_H","Predict")
ggplot.nb<-ggplot(Plot_nb,aes(x=Customer_SVC_H,y=Staff_Cared_H)) + geom_point(aes(size=correct,color=goodOzone,shape=Predict))

# Two models created are working well. 
# The percentage of correct cases for ksvm, nb are displayed.


