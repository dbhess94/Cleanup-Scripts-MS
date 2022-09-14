library(dplyr)
library(tidyverse)
library(eeptools)
library(readxl)
library(pdftools)
library(ggplot2)
library(rvest)
library(utils)

Sell_customers <- read.csv("N:\\Master Data\\Branch Files\\Sellersburg\\AS400 Data Extracts\\B5_CUST.csv")

#remove all double spaces and na's
Sell_customers <- Sell_customers %>%
  mutate_all(as.character)

Sell_customers[is.na(Sell_customers)] <- " "

Sell_customers <- Sell_customers %>%
  mutate_all(str_squish)

#create parent column if external_id doesn't contain
fc <- Sell_customers
fc$Parent <- ifelse(str_detect(fc$External_ID, ":",) == FALSE,"",fc$Parent)
#remove blank spaces and .'s
fc$PrimaryFAX <- gsub("[[:space:]]", "", fc$PrimaryFAX)
fc$Address1_FAX <- gsub("[[:space:]]", "", fc$Address1_FAX)
fc$Address2_FAX <- gsub("[[:space:]]", "", fc$Address2_FAX)
fc$PrimaryFAX <- gsub("\\.","",fc$PrimaryFAX)
fc$Address1_FAX <- gsub("\\.","",fc$Address1_FAX)
fc$Address2_FAX <- gsub("\\.","",fc$Address2_FAX)

#if credit limit is lower than 10 change to 0
fc$CreditLimit <- ifelse(fc$CreditLimit < 10, 0, fc$CreditLimit)

#create new column headers and columns to match uploads
fc$National_Account <- fc$NationalSalesGroup
fc$Legacy_Create_Date <- fc$StartDate
fc$ShipComplete <- fc$MustShipComplete
fc$ShipComplete <- FALSE
fc$Inside_Support <- fc$CSR

#remove nationalsalesgroup,startdate, and mustshipcomplete
fc <- subset(fc, select = -c(NationalSalesGroup,StartDate,MustShipComplete))
fc$GlobalSubscriptionStatus <- "SOFT OPT-OUT"

fc$Legacy_Account_No <- fc$External_ID
#remove everything to the right of :
fc$Vertex_Customer_ID <- gsub(':.*',"",fc$External_ID)
#
fc$FinanceApproved <- ifelse(str_detect(fc$External_ID, ":") == FALSE,FALSE,fc$FinanceApproved)

fc$Category <- paste(fc$MarketCode, fc$MarketCodeDescription, fc$CustomerCategory)

fc$TaxableItem <- "Vertex Tax Code"

fc$MFG_Order_Details <- ""

fc$Delivery_Instructions <- ""

fc$Invoices_Email <- ""

fc$Address1_Label <- "Bill-To"

fc$Credit_Hold_Status <- gsub("FALSE","OFF", fc$PerformCreditCheck)
fc$Credit_Hold_Status <- gsub("TRUE", "Always Validate", fc$Credit_Hold_Status)
#possibly try to code this later
fc$Buying_Group_Authorization_Required <- ""

fc$Customer_Comments <- ""

fc$Customer_MFG_Order_Details <- ""

fc$Customer_Delivery_Notes <- ""

fc$'Eclipse_Info(inExclipse, not NS)' <- ""

#make ship complete true if lowes or home depot
fc$ShipComplete <- ifelse(str_detect(fc$CompanyName,"LOWE'S")==TRUE,TRUE, fc$ShipComplete)
fc$ShipComplete <- ifelse(str_detect(fc$CompanyName,"HOME DEPOT")==TRUE,TRUE,fc$ShipComplete)


fc$Address1_City <- as.character(fc$Address1_City)
#fc$Address1_City <- ifelse(fc$Parent == "","",fc$Address1_City)
str(fc$Address1_City)

view <- data.frame(fc$Address1_City,fc$Parent)

fc$Legacy_ID <- fc$External_ID

fc$Vertex_Customer_ID <- ifelse(str_detect(fc$Legacy_Account_No,":") == TRUE, substring(fc$Vertex_Customer_ID,2),fc$Vertex_Customer_ID)


fc$count <- as.numeric(ave(fc$Vertex_Customer_ID,fc$Vertex_Customer_ID, FUN = length))
#make certain columns empty
fc$Address3_Attention <- ""
fc$Address3_Addressee <- ""
fc$Address3_Line1 <- ""
fc$Address3_Line2 <- ""
fc$Address3_Line3 <- ""
fc$Address3_City <- ""
fc$Address3_State <- ""
fc$Address3_ZipCode <- ""
fc$Address3_Country <- ""
fc$Address3_PrimaryPhone <- ""
fc$Address3_MobliePhone <- ""
fc$Address3_FAX <- ""
fc$Address3_EmailAddress <- ""

fc2 <- filter(fc, count == 2)


#create list of vertex customer id in order
cust_list <- list(fc2$Vertex_Customer_ID)
cust_vector <- unlist(cust_list)
cust_vector <- sort(cust_vector)

cust_vector <- unique(cust_vector)

cust_list2 <- strsplit(cust_vector," ")


test <- fc
test <- test[0,]

fc2 <- fc2[order(fc2$Vertex_Customer_ID),]
fc3 <- data.frame()
deleted <- data.frame()
fc2$Address1_Line1 <- as.character(fc2$Address1_Line1)
fc2$External_ID <- as.character(fc2$External_ID)



#make addres lines character data types
fc2$Address1_Line1 <- as.character(fc2$Address1_Line1)
fc2$Address2_Line1 <- as.character(fc2$Address2_Line1)
fc2$Address2_Line2 <- as.character(fc2$Address2_Line2)
#get rid of columns
fc2 <- subset(fc2, select = -c(IsPerson, ShipToContactName,Salutation,FirstName,MiddleName,LastName,JobTitle,AddressCode,
                               SalesRepName,NationalAcctCSR,Partner,MarketCode,MarketCodeDescription,CustomerCategory,
                               DefaultOrderPriority,SalesTerritory,LeadSource,AccountNumber,DefaultReceivablesAccount,
                               LegacyEndDate,LegacyStartDate,EndDate,ReminderDays,PurchaseOrderRequired,PreferredCreditCardName,
                               DunnsNumber,VatTaxRegistrationNumber,OpeningBalance,OpeningBalanceAccount,OpeningBalanceDate,
                               NumberFormat,NegativeNumberFormat,PrintOnCheckAs,AllowBackOrders,ShippingCarrier,
                               CC_LoginAccess,CC_LoginEmail,CC_LoginRole,CC_Password,CC_Password2,IsCustomerInactive,CustomField1,
                               CustomField2,CustomField3,CustomFlag1,CustomFlag2,CustomFlag3,BuyingGroup,LegacyBranch,LegacyWhse,
                               LegacyWhseLocation,TargetAccount,TrackSales,RelatedResource,CustomerComments,CustomerComments_01,
                               CustomerComments_02,CustomerComments_03,CustomerComments_04,CustomerComments_05,CustomerComments_06,
                               CustomerComments_07,CustomerComments_08,CustomerComments_09,CustomerComments_10,AddressTypeCode))

#change order of columns
fc2 <- fc2 %>%
  select(External_ID, Legacy_ID, Subsidiary, Entity_ID, Vertex_Customer_ID, Invoices_Email, FinanceApproved, CompanyName, Parent, SalesRep, National_Account, Category, PrimaryPhone,
         PrimaryFAX, Address1_Label, Address1_Attention, Address1_Addressee, Address1_Line1, Address1_Line2, Address1_Line3, Address1_City,
         Address1_State, Address1_ZipCode, Address1_Country, Address1_PrimaryPhone, Address1_MobilePhone, Address1_FAX, Address1_EmailAddress,
         Address1_DefaultBillTo, Address1_DefaultShipTo, Address2_Attention, Address2_Addressee, Address2_Line1, Address2_Line2, Address2_Line3,
         Address2_City, Address2_State, Address2_ZipCode, Address2_Country, Address2_PrimaryPhone, Address2_MobilePhone, Address2_FAX,
         Address2_EmailAddress, Address2_DefaultBillTo, Address2_DefaultShipTo, Address3_Attention, Address3_Addressee, Address3_Line1,
         Address3_Line2, Address3_Line3, Address3_City, Address3_State, Address3_ZipCode, Address3_Country, Address3_PrimaryPhone, 
         Address3_MobliePhone, Address3_FAX, Address3_EmailAddress, GlobalSubscriptionStatus, Legacy_Create_Date, PriceLevel,
         CurrencyCode, PaymentTerms, Credit_Hold_Status, Buying_Group_Authorization_Required, TaxableItem, Language, EmailPreference,
         SendTransactionsViaEmail, SendTransactionsViaPrint, SendTransactionsViaFAX, ShipComplete, ShippingMethod, ShippingAdjustmentItem,
         AssociatedBranch, ShippingInstructions, Inside_Support, ApplyServiceCharges, Customer_Comments, Customer_MFG_Order_Details,
         Customer_Delivery_Notes, `Eclipse_Info(inExclipse, not NS)`, everything())

#loop to remove row if external_id has MS and add to new data frame called deleted
for (x in 1:nrow(fc2)) {
  if (str_detect(fc2$External_ID[x], "MS")){
    (deleted <- rbind(deleted, fc2[x,]))
  }
}

#filter out external_IDs with MS
fc2 <- fc2 %>%
  filter(!str_detect(External_ID, "MS"))

#if po box in address 2 line 1 check if there is an address 2 line 2 with no po box
#if there is move it to address 2 line 1 and delete address 2 line 2
#check if the addresses are equal
#if they are remove child and make address 2 fields empty
#address 1 is true true
#if the addresses are different move child address 2 line 1 to parent address 2 line 1
#if we do this address 1 is default bill to true and ship to false and address 2 is false true

for (i in 1:nrow(fc2)) {
  # print(str_detect(fc2$Address1_Line1[i], "PO") || str_detect(fc2$Address1_Line1[i], "P.O") && fc2$Address2_Line2[i] != "")
  if ((str_detect(fc2$Address2_Line1[i], "PO BOX") || str_detect(fc2$Address2_Line1[i], "P.O. BOX")) && fc2$Address2_Line2[i] != "") {
    fc2$Address2_Line1[i] <- fc2$Address2_Line2[i]
    fc2$Address2_Line2[i] <- ""
  }
  num <- i
  if (num%%2 == 0) {
    # print(i)
    # print(fc2$CompanyName[i])
    #35 51
    if (fc2[i, 18] == fc2[i - 1, 33]) {
      fc2$Address1_DefaultBillTo[i] <- "TRUE"
      fc2$Address1_DefaultShipTo[i] <- "TRUE"
      fc2$Address2_DefaultBillTo[i] <- ""
      fc2$Address2_DefaultShipTo[i] <- ""
      fc2$FinanceApproved[i] <- "TRUE"
    }
    else {
      fc2$Address2_Line1[i] <- fc2$Address2_Line1[i-1]
      fc2$Address2_Attention[i] <- fc2$Address2_Attention[i-1]
      fc2$Address2_Addressee[i] <- fc2$Address2_Addressee[i-1]
      fc2$Address2_Line2[i] <- fc2$Address2_Line2[i-1]
      fc2$Address2_Line3[i] <- fc2$Address2_Line3[i-1]
      fc2$Address2_City[i] <- fc2$Address2_City[i-1]
      fc2$Address2_State[i] <- fc2$Address2_State[i-1]
      fc2$Address2_ZipCode[i] <- fc2$Address2_ZipCode[i-1]
      fc2$Address2_Country[i] <- fc2$Address2_Country[i-1]
      fc2$Address2_PrimaryPhone[i] <- fc2$Address2_PrimaryPhone[i-1]
      fc2$Address2_MobilePhone[i] <- fc2$Address2_MobilePhone[i-1]
      fc2$Address2_FAX[i] <- fc2$Address2_FAX[i-1]
      fc2$Address2_EmailAddress[i] <- fc2$Address2_EmailAddress[i-1]
      fc2$Address1_DefaultBillTo[i] <- "TRUE"
      fc2$Address1_DefaultShipTo[i] <- "FALSE"
      fc2$Address2_DefaultBillTo[i] <- "FALSE"
      fc2$Address2_DefaultShipTo[i] <- "TRUE"
      fc2$FinanceApproved[i] <- "TRUE"
    }
    fc3 <- rbind(fc3, fc2[i,])
  }
  else {
    deleted <- rbind(deleted, fc2[i,])
  }
}

fc3$Vertex_Customer_ID
test2 <- merge(fc3, deleted, by = "Vertex_Customer_ID")
fc3$Legacy_ID <- paste(fc3$Vertex_Customer_ID,",",test2$External_ID.y)

fc <- fc %>%
  filter(!count == 2)

#remove columns
fc4 <- subset(fc, select = -c(IsPerson, ShipToContactName,Salutation,FirstName,MiddleName,LastName,JobTitle,AddressCode,
                              SalesRepName,NationalAcctCSR,Partner,MarketCode,MarketCodeDescription,CustomerCategory,
                              DefaultOrderPriority,SalesTerritory,LeadSource,AccountNumber,DefaultReceivablesAccount,
                              LegacyEndDate,LegacyStartDate,EndDate,ReminderDays,PurchaseOrderRequired,PreferredCreditCardName,
                              DunnsNumber,VatTaxRegistrationNumber,OpeningBalance,OpeningBalanceAccount,OpeningBalanceDate,
                              NumberFormat,NegativeNumberFormat,PrintOnCheckAs,AllowBackOrders,ShippingCarrier,
                              CC_LoginAccess,CC_LoginEmail,CC_LoginRole,CC_Password,CC_Password2,IsCustomerInactive,CustomField1,
                              CustomField2,CustomField3,CustomFlag1,CustomFlag2,CustomFlag3,BuyingGroup,LegacyBranch,LegacyWhse,
                              LegacyWhseLocation,TargetAccount,TrackSales,RelatedResource,CustomerComments,CustomerComments_01,
                              CustomerComments_02,CustomerComments_03,CustomerComments_04,CustomerComments_05,CustomerComments_06,
                              CustomerComments_07,CustomerComments_08,CustomerComments_09,CustomerComments_10,AddressTypeCode))

#change order of columns
fc4 <- fc4 %>%
  select(External_ID, Legacy_ID, Subsidiary, Entity_ID, Vertex_Customer_ID, Invoices_Email, FinanceApproved, CompanyName, Parent, SalesRep, National_Account, Category, PrimaryPhone,
         PrimaryFAX, Address1_Label, Address1_Attention, Address1_Addressee, Address1_Line1, Address1_Line2, Address1_Line3, Address1_City,
         Address1_State, Address1_ZipCode, Address1_Country, Address1_PrimaryPhone, Address1_MobilePhone, Address1_FAX, Address1_EmailAddress,
         Address1_DefaultBillTo, Address1_DefaultShipTo, Address2_Attention, Address2_Addressee, Address2_Line1, Address2_Line2, Address2_Line3,
         Address2_City, Address2_State, Address2_ZipCode, Address2_Country, Address2_PrimaryPhone, Address2_MobilePhone, Address2_FAX,
         Address2_EmailAddress, Address2_DefaultBillTo, Address2_DefaultShipTo, Address3_Attention, Address3_Addressee, Address3_Line1,
         Address3_Line2, Address3_Line3, Address3_City, Address3_State, Address3_ZipCode, Address3_Country, Address3_PrimaryPhone, 
         Address3_MobliePhone, Address3_FAX, Address3_EmailAddress, GlobalSubscriptionStatus, Legacy_Create_Date, PriceLevel,
         CurrencyCode, PaymentTerms, Credit_Hold_Status, Buying_Group_Authorization_Required, TaxableItem, Language, EmailPreference,
         SendTransactionsViaEmail, SendTransactionsViaPrint, SendTransactionsViaFAX, ShipComplete, ShippingMethod, ShippingAdjustmentItem,
         AssociatedBranch, ShippingInstructions, Inside_Support, ApplyServiceCharges, Customer_Comments, Customer_MFG_Order_Details,
         Customer_Delivery_Notes, `Eclipse_Info(inExclipse, not NS)`, everything())

fc4 <- rbind(fc4, fc3)

write.csv(fc4,"N:\\Master Data\\Branch Files\\Sellersburg\\R extract\\Sellersburg_Customers_Extract.csv", row.names=FALSE)
write.csv(deleted,"N:\\Master Data\\Branch Files\\Sellersburg\\R extract\\Sellersburg_Customers_deleted_2_count.csv", row.names=FALSE)
