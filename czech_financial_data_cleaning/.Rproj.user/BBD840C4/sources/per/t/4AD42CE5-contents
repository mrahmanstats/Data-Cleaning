library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)
library(skimr)

#Loading in 
account <- fread("account.csv")
card <- fread("card.csv")
client <- fread("client.csv")
disp <- fread("disp.csv")
district <- fread("district.csv")
loan <- fread("loan.csv")
order <- fread("order.csv")
trans <- fread("trans.csv")

#Descriptives for relation account data
skim(account); head(account);
account$date <-  as.Date(as.character(account$date), format = '%y%m%d') #Converting to a data variable
table(account$frequency)

#Descriptives for credit card data
skim(card); head(card);

#Descriptives for relation client data
skim(client); head(client);

#Descriptives for relation disposition data
skim(disp); head(disp);
table(disp$type)

#Descriptives for demographics data, need to rename the variables
skim(district); head(district);
distr <- district %>% rename("district_id" = A1,
                             "district_name" = A2,
                             "region" = A3,
                             "number_inhabitants"	= A4,
                             "number_of_cities"	= A9,
                             "ratio_urban_inhabitants" = A10,	
                             "average_salary" = A11,	
                             "unemployment_rate_95" = A12,
                             "unemployment_rate_96"	= A13,
                             "number_of_enterpreneurs_per_1000_inhabitants" = A14,	
                             "number_commited_crimes_95" = A15,	
                             "number_commited_crimes_96" = A16)

#Descriptives for relation loan data
skim(loan); head(loan);
loan$date <-  as.Date(as.character(loan$date), format = '%y%m%d');

#Descriptives for relation permanent order data
skim(order); head(order);
table(order$k_symbol)
order$k_symbol[order$k_symbol == " "] <- "OTHER"  #Missing values given new level as "Other"
table(order$bank_to)
order <- order %>% mutate(k_symbol = as.factor(k_symbol))

#Descriptives for relation transaction, again giving empty levels as "other"
skim(trans); head(trans);
trans$date <-  as.Date(as.character(trans$date), format = '%y%m%d');
trans$type[trans$type == " " | trans$type == ""] <- "OTHER"
trans$operation[trans$operation == " " | trans$operation == " "] <- "OTHER"
trans$k_symbol[trans$k_symbol == " " | trans$k_symbol == ""] <- "OTHER"
trans$bank[trans$bank == " " | trans$bank == "" ] <- "OTHER"
trans <- trans %>% mutate(type = as.factor(type), operation = as.factor(operation), k_symbol = as.factor(k_symbol))


#Joining the 7 datasets into one large datasets
client_district <- left_join(client, distr, by = "district_id")
cd_disp <- left_join(client_district, disp, by = "client_id")
cdd_card <- left_join(cd_disp, card, by = "disp_id")
cddc_account <- left_join(cdd_card, account, by = "account_id")

a <- cddc_account[duplicated(cddc_account$account_id),] %>% select(account_id) %>% distinct() 
cddc_account[a$account_id %in% cddc_account$account_id,] %>% View()

cddc_acc <- cddc_account %>% filter(type.x == "OWNER")

cddca_trans <- left_join(trans, cddc_acc, by = "account_id")
cddcat_loan <- left_join(cddca_trans, loan, by = "account_id")



ord <- order %>% select(account_id, amount) %>% group_by(account_id) %>% summarise("total_order_amount" = sum(amount))


cddcatl_order <- left_join(cddcat_loan, ord, by = "account_id")

#Dataset removing variables with more than 500k missing variables
df <- cddcatl_order %>% select(-payments, - total_order_amount, - loan_id, - duration, - card_id, - account, - date, - date.x, - type.y, - status, - issued)
df <- df %>% select(-district_id.x, - amount.y, - type.x) %>% rename("date" = date.y, "district_id" = district_id.y, "amount" = amount.x )

#Creating gender variable based off the birth_number variable (if the mm part of birthdate is over 31, the individual is female)
df$isFemale <- if_else(as.numeric(substr(df$birth_number, start = 3, stop = 4)) > 31, 1, 0)













