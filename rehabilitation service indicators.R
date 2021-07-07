library(tidyverse)
library(fst)
library(table1)
library(psych)
# read the data
psychology<- read.fst("....\\psychology.fst")
services <-psychology
## factor geographical locations and level them in the appropriate orders
services$area <- factor(services$area,levels=c("Major urban area","Large urban area","Medium urban area","Small urban area","Rural"),
                        labels=c("Major urban area","Large urban area","Medium urban area","Small urban area","Rural")) 

services$dhb_domicile <- factor(services$dhb_domicile,levels=c("Auckland","Northland","Waitemata","Counties Manukau",
                                                               "Waikato", "Bay of Plenty","Tairawhiti","Lakes","Taranaki",
                                                               "Capital and Coast","Hawkes Bay","Whanganui","MidCentral","Hutt Valley","Wairarapa",
                                                               "Canterbury","Nelson Marlborough","West Coast","South Canterbury","Southern"),
                                labels=c("Auckland","Northland","Waitemata","Counties Manukau",
                                         "Waikato", "Bay of Plenty","Tairawhiti","Lakes","Taranaki",
                                         "Capital and Coast","Hawkes Bay","Whanganui","MidCentral","Hutt Valley","Wairarapa",
                                         "Canterbury","Nelson Marlborough","West Coast","South Canterbury","Southern")) 

services$region <- factor(services$region,levels=c("Northern","Midland","Central","South Island"),
                          labels=c("Northern","Midland","Central","South Island")) 

# Summary statistics
table1(~age_group+sex+Ethnicity+time_acci_to_service+time_discharge_to_service+service_length+contact_numbers+region+area|Ethnicity,data=services)
#### service indicators by different group
## by gender
table1(~time_acci_to_service+service_length+contact_numbers|sex, data=services)
## by age group
table1(~time_acci_to_service+service_length+contact_numbers|age_group, data=services)
## stbi
table1(~time_acci_to_service+service_length+contact_numbers|s_tbi, data=services)

##Median of service access time
quantile(services$time_acci_to_service,0.5) 


##################################################################################################
## Service access time variation among geographical locations#####################################
##################################################################################################

service_region <- services %>% 
  group_by(region) %>% summarise(Number=sum(!is.na(incident_number)),
                                 Min=min(time_acci_to_service),
                                 Q1=quantile(time_acci_to_service,0.25),
                                 Median=quantile(time_acci_to_service,0.5),
                                 Mean=mean(time_acci_to_service),
                                 Q3=quantile(time_acci_to_service,0.75),
                                 Max=max(time_acci_to_service,outlier=FALSE)) %>% mutate(IQR=Q3-Q1,Range=Max-Min)

service_dhb <- services %>% 
  group_by(dhb_domicile) %>% summarise(number=sum(!is.na(incident_number)),
                                       Min=min(time_acci_to_service),
                                       Q1=quantile(time_acci_to_service,0.25),
                                       Median=quantile(time_acci_to_service,0.5),
                                       Mean=mean(time_acci_to_service),
                                       Q3=quantile(time_acci_to_service,0.75),
                                       
                                       Max=max(time_acci_to_service,outlier=FALSE)) %>% mutate(IQR=Q3-Q1,Range=Max-Min)


service_area <- services %>% 
  group_by(area) %>% summarise(number=sum(!is.na(incident_number)),
                               Min=min(time_acci_to_service),
                               Q1=quantile(time_acci_to_service,0.25),
                               Median=quantile(time_acci_to_service,0.5),
                               Mean=mean(time_acci_to_service),
                               Q3=quantile(time_acci_to_service,0.75),
                               
                               Max=max(time_acci_to_service,outlier=FALSE)) %>% mutate(IQR=Q3-Q1,Range=Max-Min)


###########################################################################
## Service contact numbers variation among geographical locations#############
###########################################################################

summary(services$contact_numbers)
service_contact_region <- services %>% group_by(region) %>% summarise(people=sum(!is.na(incident_number)),n=sum(contact_numbers),contact_average=round(sum(contact_numbers)/sum(!is.na(incident_number)),1))


service_contact_dhb <- services %>% group_by(dhb_domicile) %>% summarise(people=sum(!is.na(incident_number)),n=sum(contact_numbers),contact_average=round(sum(contact_numbers)/sum(!is.na(incident_number)),1))



service_contact_area <- services %>% group_by(area) %>% summarise(people=sum(!is.na(incident_number)),n=sum(contact_numbers),contact_average=round(sum(contact_numbers)/sum(!is.na(incident_number)),1))

######################################################################################
#################### Length of services variations among different group##############
######################################################################################

## Length of services variations among areas
# Test normal distribution 
qqnorm(services$service_length)
qqline(services$service_length,col=2)
# Take log of the service duration
services$log_duration<- log(services$service_length)
qqnorm(services$log_duration)
# Anova analysis
model=aov(services$log_duration~services$area)
summary(model) 
# Use TukeyHSD test to confirm the differences
tk=TukeyHSD(model)
tk 
plot(tk)


### Length of services variations among regions

# Anova analysis
model=aov(services$log_duration~services$region)
summary(model) # psy: F=2.858 p= 0.0365  There is an evidence of the difference in mean of service duration among region
# Use TukeyHSD test to confirm the differences
tk=TukeyHSD(model)
tk #South Island-Central   (p adj)0.04 
plot(tk)
## Among age group
services$log_duration<- log(services$service_length)

model=aov(services$log_duration~services$age_group)
summary(model) 

tk=TukeyHSD(model)
tk 

## Length of services variations between gender 
t.test(services$service_length~services$sex)

## Length of services variations between ethnicity groups
t.test(services$service_length~services$Ethnicity) 

### Length of services variations between stbi group
t.test(services$service_length~services$s_tbi) 


#######################################################################################################
##############Correlation between services access time and service duration among different group######
#######################################################################################################

service_correlation <- services %>% select(log_access_time,log_duration)

pairs.panels(service_correlation[1:2],bg=c("blue"), pch=21,hist.col="red",stars=TRUE) 

###############################################################
########## The first and the last quantile#####################
###############################################################
quantile(services$time_acci_to_service,c(.25,.75, .90),na.rm=T)
# 3.8 15.6 26.5 months
services <- services%>% mutate(quantile=case_when(
  services$time_acci_to_service<3.8~"first_quantile",
  services$time_acci_to_service>15.6~"last_quantile",
  TRUE~"OTHER"
))
# Table of differences between people in the first and the last quantile
services_quantile <-table1(~age_group+sex+Ethnicity+time_acci_to_service+time_discharge_to_service+service_length+contact_numbers+region+area|quantile,data=services)


# Write the table
services_quantile1 <- data.frame(services_quantile)
tab_df(services_quantile1,file="....doc")

##############################################################
################ Box plots####################################
##############################################################

# calculate quantile of access time for reference line in boxplots
quantile(service$time_acci_to_service,0.5)

# Service access time by region#########

#Used boxplot to present the service access time by regions
boxplot(time_acci_to_service~region,
        
        las=1,ylab="Length of time (months)",xlab="Regions",col="light green",
        lwd = 1,  medianLwd = 2,
        pch = 10,
        pchCex = 1,cex.axis=1, main="Psychology service access time from the injury date",
        #ylim=c(0,120),
        data = services,outline=FALSE)
abline(h=8.5, col="red",lty = 5,lwd=2)

# Access time by dhbs###############

service_dhb <- boxplot(time_acci_to_service~dhb_domicile,
                       
                       las=2,ylab="Length of time (months)",xlab="",col="light green",
                       ylim=c(0,56),cex.axis=0.72,
                       main="Psychology service access time from the injury date",
                       data = services,outline=FALSE)
abline(v=c(4.5,9.5,15.5),col="blue")
abline(h=8.5, col="red",lty = 5,lwd=1.5)
text(x= 2.2, y= 55, labels= "Northern")
text(x= 7, y=55, labels= "Midland")
text(x= 12, y= 55, labels= "Central")
text(x= 18, y= 55, labels= "South Island")



# Access time by geographical locations and ethnicity groups
ggplot(service, aes(x=region, y=time_acci_to_service, fill=Ethnicity,outline=FALSE)) +labs(title = "Psychology service access time from the injury date", x="Region", y="Length of time(months)")+
  geom_boxplot(outlier.shape = NA)

ggplot(service, aes(x=area, y=time_acci_to_service, fill=Ethnicity,outline=FALSE)) +labs(title = "Psychology service access time from the injury date", x="Area", y="Length of time(months)")+
  geom_boxplot(outlier.shape = NA)

ggplot(service, aes(x=dhb_domicile, y=time_acci_to_service, fill=Ethnicity,outline=FALSE)) +labs(title = "Psychology service access time from the injury date", x="DHBs", y="Length of time(months)")+
  geom_boxplot(outlier.shape = NA)+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))










