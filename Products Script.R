library(dplyr)
library(tidyverse)
library(eeptools)
library(readxl)
library(pdftools)
library(ggplot2)
library(rvest)
library(utils)

Sell_panels <- read.csv("N:\\Master Data\\Branch Files\\Sellersburg\\AS400 Data Extracts\\B5_ITYP2.csv")
Transfer_Rates <- read.csv("N:\\Master Data\\Transfer Rate and Transfer Rate UM\\Big_Break_Transfer_Charges_With_Raw_Code.csv")

#make itemweight empty if it's 0
fp <- Sell_panels %>%
  dplyr::mutate(ItemWeight = ifelse(ItemWeight == 0, "", ItemWeight))

#Color.	Clean up the Color Codes/Color Descriptions 
# ColorFast 45 -> CF45
fp$PaintType <- gsub("COLORFAST 45", "CF45",fp$PaintType)
fp$PaintType <- gsub("MS CF40", "CF40",fp$PaintType)
fp$PaintType <- gsub("MS CRINKLE", "CRINKLE",fp$PaintType)
# Bare - Galvalume -> Bare
fp$PaintType <- gsub("BARE - GALVALUME", "BARE",fp$PaintType)
# Bare - Galvanized -> Bare
fp$PaintType <- gsub("BARE - GALVANIZED", "BARE",fp$PaintType)
# Economy/PCDF -> Removed
#fp$PaintType <- gsub("ECONOMY","",fp$PaintType)
fp$PaintType <- gsub("PVDF","KYNAR",fp$PaintType)

fp$PaintType <- gsub("BARE - ALUMINUM", "BARE",fp$PaintType)

#replace 0's with empty cells for Weight, Width, PurchasePrice, Length

fp <- fp %>%
  mutate(Width = ifelse(Width == 0, "", Width))

fp <- fp %>%
  mutate(PurchasePrice = ifelse(PurchasePrice == 0, "", PurchasePrice))

fp <- fp %>%
  mutate(Length = ifelse(Length == 0, "", Length))


#sub " for INCH
fp$Profile <- gsub("INCH",'"', fp$Profile)

fp <- fp %>%
  mutate(Profile = gsub("0IN",'0"',Profile))
fp$Profile <- gsub("1IN",'1"',fp$Profile)
fp$Profile <- gsub("2IN",'2"',fp$Profile)
fp$Profile <- gsub("3IN",'3"',fp$Profile)
fp$Profile <- gsub("4IN",'4"',fp$Profile)
fp$Profile <- gsub("5IN",'5"',fp$Profile)
fp$Profile <- gsub("6IN",'6"',fp$Profile)
fp$Profile <- gsub("7IN",'7"',fp$Profile)
fp$Profile <- gsub("8IN",'8"',fp$Profile)
fp$Profile <- gsub("9IN",'9"',fp$Profile)
fp$Profile <- gsub("0in",'0"',fp$Profile)
fp$Profile <- gsub("1in",'1"',fp$Profile)
fp$Profile <- gsub("2in",'2"',fp$Profile)
fp$Profile <- gsub("3in",'3"',fp$Profile)
fp$Profile <- gsub("4in",'4"',fp$Profile)
fp$Profile <- gsub("5in",'5"',fp$Profile)
fp$Profile <- gsub("6in",'6"',fp$Profile)
fp$Profile <- gsub("7in",'7"',fp$Profile)
fp$Profile <- gsub("8in",'8"',fp$Profile)
fp$Profile <- gsub("9in",'9"',fp$Profile)

fp <- fp %>%
  mutate(Configuration = gsub("0IN",'0"',Configuration))
fp$Configuration <- gsub("1IN",'1"',fp$Configuration)
fp$Configuration <- gsub("2IN",'2"',fp$Configuration)
fp$Configuration <- gsub("3IN",'3"',fp$Configuration)
fp$Configuration <- gsub("4IN",'4"',fp$Configuration)
fp$Configuration <- gsub("5IN",'5"',fp$Configuration)
fp$Configuration <- gsub("6IN",'6"',fp$Configuration)
fp$Configuration <- gsub("7IN",'7"',fp$Configuration)
fp$Configuration <- gsub("8IN",'8"',fp$Configuration)
fp$Configuration <- gsub("9IN",'9"',fp$Configuration)
fp$Configuration <- gsub("0in",'0"',fp$Configuration)
fp$Configuration <- gsub("1in",'1"',fp$Configuration)
fp$Configuration <- gsub("2in",'2"',fp$Configuration)
fp$Configuration <- gsub("3in",'3"',fp$Configuration)
fp$Configuration <- gsub("4in",'4"',fp$Configuration)
fp$Configuration <- gsub("5in",'5"',fp$Configuration)
fp$Configuration <- gsub("6in",'6"',fp$Configuration)
fp$Configuration <- gsub("7in",'7"',fp$Configuration)
fp$Configuration <- gsub("8in",'8"',fp$Configuration)
fp$Configuration <- gsub("9in",'9"',fp$Configuration)

#create new display name for trim
#disable scientific notation
options(scipen = 999)
fp$UPCcode[fp$UPCcode == 0] <- ""
#make gauge in ##GA format
#format the integer with 2 digts left padding it with zeroes
fp$Gauge <- sprintf("%02d", fp$Gauge)
#add GA to end of number
fp$Gauge <- paste(fp$Gauge, "GA",sep="")
#make grade in GR## format
fp$Grade <- gsub("GRXX","",fp$Grade)
#create new display name for panels
fp$Gauge <- gsub("00GA","",fp$Gauge)

#make new column for AMS Eclipse Decimal Inch(Just for Panel Eaches Ex. 10" Panel = 120 inches) (only for Home Depot and Lowes)

fp$'AMS Eclipse Decimal Inch' <- ifelse(str_detect(fp$ExternalId, "HD") == TRUE, fp$Length,"")
fp$`AMS Eclipse Decimal Inch` <- ifelse(str_detect(fp$ExternalId, "LW") == TRUE, fp$Length,fp$`AMS Eclipse Decimal Inch`)

#make a column for Eclipse Stock Panel Flag(Yes for any panel stocked, only for Home Depot and Lowes)
fp$'Eclipse Stock Panel Flag' <- ifelse(str_detect(fp$ExternalId, "HD") == TRUE, TRUE,"")
fp$`Eclipse Stock Panel Flag` <- ifelse(str_detect(fp$ExternalId, "LW") == TRUE, TRUE,fp$`Eclipse Stock Panel Flag`)


#add " at the end of length when length is given and ' at the end of the inches
#edit this to be feet and inches
fp$Length <- as.numeric(fp$Length)
fp$Length2 <- floor(fp$Length / 12)
fp$LengthInches <- fp$Length %% 12
fp$Length2 <- as.character(fp$Length2)
fp$LengthInches <- as.character(fp$LengthInches)
fp$Length3 <- paste(fp$Length2,"'","-",fp$LengthInches,'"',sep="")
fp$Length3 <- gsub("NA'-","",fp$Length3)
fp$Length3 <- gsub('NA"', "",fp$Length3)
fp$Length <- fp$Length3
fp$Length <- gsub('-0"',"",fp$Length)
#make supplytype column and change M's to B's
fp <- fp %>%
  mutate(SupplyType = gsub("M","B",MakeBuyCode))

fp$SupplyType <- gsub("B","Build",fp$SupplyType)
fp$SupplyType <- gsub("P","Purchase",fp$SupplyType)

#UnitsType needs to be either "Each" or "Linear Foot" so change 4 unit columns from EA to Each and LF to Linear Foot
fp$DefaultUnit <- gsub("EA","Each",fp$DefaultUnit)
fp$DefaultUnit <- gsub("LF","Linear Foot",fp$DefaultUnit)
fp$StockUnit <- gsub("EA","Eaches",fp$StockUnit)
fp$StockUnit <- gsub("LF","Linear Feet",fp$StockUnit)
fp$PurchaseUnit <- gsub("EA","Eaches",fp$PurchaseUnit)
fp$PurchaseUnit <- gsub("LF","Linear Feet",fp$PurchaseUnit)
fp$SalesUnit <- gsub("EA","Eaches",fp$SalesUnit)
fp$SalesUnit <- gsub("LF","Linear Feet",fp$SalesUnit)

fp$DefaultUnit <- ifelse(fp$DefaultUnit == "LB",fp$PurchaseUnit,fp$DefaultUnit)
fp$StockUnit <- ifelse(fp$StockUnit == "LB",fp$PurchaseUnit,fp$StockUnit)
fp$SalesUnit <- ifelse(fp$SalesUnit == "LB",fp$PurchaseUnit,fp$SalesUnit)

fp$DefaultUnit <- gsub("Linear Feet","Linear Foot",fp$DefaultUnit)
fp$DefaultUnit <- gsub("Eaches","Each",fp$DefaultUnit)


#change Subsidiary cells to Interlock:Metal Sales Manufacturing Corporation

fp$Subsidiary <- "Interlock:Metal Sales Manufacturing Corporation"

#IncludeChildren, TrackLandedCost, MatchBillToReceipt, UseBins, RoundQuantities all TRUE
fp$IncludeChildren <- "TRUE"
fp$TrackLandedCost <- "TRUE"
fp$MatchBillToReceipt <- "TRUE"
fp$UseBins <- "TRUE"
fp$RoundQuantities <- "TRUE"

#costing method should be "Average"
fp$CostingMethod <- "Average"

#ATPMethod = Cumulative ATP with Look Ahead
fp$ATPMethod <- "Cumulative ATP with Look Ahead"

#come back to SupplyReplenishmentMethod later

#AutoPreferredStockLevel, AutoReorderPoint, AutoLeadTime, SeasonalDemand = FALSE
fp$AutoPreferredStockLevel <- "FALSE"
fp$AutoReorderPoint <- "FALSE"
fp$AutoLeadTime <- "FALSE"
fp$SeasonalDemand <- "FALSE"

#Various Purchasefields should be 0's
#PurchasePrice(probably not),PurchaseOrderBillQty?, PurchaseOrderBillAmount, PurchaseOrderBillDiff, PurchaseOrderReceiptQty,
#PurchaseOrderReceiptAmount, PurchaseOrderRreceiptDiff
fp$PurchaseOrderBillQty <- 0
fp$PurchaseOrderBillAmount <- 0
fp$PurchaseOrderBillDiff <- 0
fp$PurchaseOrderReceiptQty <- 0
fp$PurchaseOrderReceiptAmount <- 0
fp$PurchaseOrderReceiptDiff <- 0

#preffered location is blank in item master

#set Cost Estimate Type to Average Cost
fp$CostEstimateType <- "Average Cost"

#minimumquantity is equal to zero
fp$MinimumQuantity <- 0

#EnforceMinQtyInternally = False
fp$EnforceMinQtyInternally <- "FALSE"

#make new column named MaxPiecesPerBundle that is rounded down of ItemWeight / 40 ***** only for trim
#fp <- fp %>%
#  mutate(MaxPiecesPerBundle = floor(40 / ItemWeight))
#make new column for AMS Eclipse Decimal Inch(Just for Panel Eaches Ex. 10" Panel = 120 inches) (only for Home Depot and Lowes)

#make a column for Eclipse Stock Panel Flag(Yes for any panel stocked, only for Home Depot and Lowes)

#make the itemweight 3 decimal places
fp$ItemWeight <- as.numeric(fp$ItemWeight)
fp$ItemWeight <- format(round(fp$ItemWeight, 3), nsmall = 3)

#UseMarginalRates = True
fp$UseMarginalRates <- "TRUE"

#COGSAccount = 51100 Cost of Goods Sold
fp$COGSAccount <- "51100 Cost of Goods Sold"

#IncomeAccount = 41100 Sales
fp$IncomeAccount <- "41100 Sales"

#AssetAccount = 11698 INVENTORY-RAW/FINISHED
fp$AssetAccount <- "11698 INVENTORY-RAW/FINISHED"

#TaxSchedule should be "Vertex Tax Schedule"
fp$TaxSchedule <- "Vertex Tax Schedule"

#Offer Support = True
fp$OfferSupport <- "TRUE"

#IsInactive = FALSE
fp$UnitsType2 <- fp$PurchaseUnit
fp$UnitsType2 <- gsub("Linear Feet", "Linear Foot",fp$UnitsType2)
fp$'TRANSFER RATE' <- ""
#MAY HAVE TO CHANGE THE TRANSFER RATE FOR THE T-PANELS#################################################################################################################
fp$`TRANSFER RATE` <- Transfer_Rates$Multiply.Material.Value.by.this.Figure.for.Overhead.Charges[match(fp$Raw.Item,Transfer_Rates$X5.Digit.Leading.Characters.with..XX..Wild.Card)]
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "5V CRIMP") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "V-LINE") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "V LINE") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "IC72") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "CORRUGATED") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "PBR") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "R-PANEL") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "SPAN") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "PRO-PANEL") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "CLASSIC") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "U-PANEL") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "PBU") == TRUE, .09,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "DELTA") == TRUE, .09,fp$`TRANSFER RATE`)

fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "MAGNA") == TRUE, .3,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "SEAM-LOC") == TRUE, .3,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "SNAP") == TRUE, .3,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "VERTICAL SEAM") == TRUE, .3,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "VERT SEAM") == TRUE, .3,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "CLIP-LOC") == TRUE, .3,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "PRO-LOC") == TRUE, .3,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "MINI") == TRUE, .3,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "MAXI") == TRUE, .3,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "IMAGE") == TRUE, .3,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "SOFFIT") == TRUE, .3,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "T ARMOR") == TRUE, .3,fp$`TRANSFER RATE`)

fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "FLAT SHEET") == TRUE, .15,fp$`TRANSFER RATE`)
fp$`TRANSFER RATE` <- ifelse(str_detect(fp$Profile, "FLATSHEET") == TRUE, .15,fp$`TRANSFER RATE`)

fp$'TRANSFER RATE U/M' <- "Material Cost"
fp$'OZLINK CLASS' <- "Panel"
fp$'OZLINK CLASS' <- ifelse(str_detect(fp$Profile, "FLAT SHEET") == TRUE, "FLAT SHEET",fp$`OZLINK CLASS`)
fp$'OZLINK CLASS' <- ifelse(str_detect(fp$Profile, "FLATSHEET") == TRUE, "FLATSHEET",fp$`OZLINK CLASS`)
fp$ReorderMultiple <- gsub(0,"",fp$ReorderMultiple)
fp$Manufacturer[is.na(fp$Manufacturer)] <- ""
fp$CountryOfManufacture[is.na(fp$CountryOfManufacture)] <- ""
fp$QtyPricingSchedule[is.na(fp$QtyPricingSchedule)] <- ""
fp$PricingGroup[is.na(fp$PricingGroup)] <- ""
fp$UnbuildVarianceAccount[is.na(fp$UnbuildVarianceAccount)] <- ""
fp$Thickness <- ifelse(fp$Thickness == 0,gsub(0,"",fp$Thickness),fp$Thickness)


fp$Substrate <- as.character(fp$Substrate)

fp$Substrate <- ifelse(fp$ColorDescription == "COPPER PLUS", "COPPER", fp$Substrate)
fp$Substrate <- ifelse(fp$ColorDescription == "COPPER","COPPER",fp$Substrate)
fp$Substrate <- ifelse(fp$ColorDescription == "16OZ COPPER","COPPER",fp$Substrate)
fp$Substrate <- ifelse(fp$Substrate == "ALUMINUM","ALUM",fp$Substrate)

#disable scientific notation
fp$'Configuration Group' <- "PANEL LF CONFIGURATION GROUP 1"
fp$`Configuration Group` <- ifelse(str_detect(fp$DisplayNameLegacy, "CURVED") == TRUE, "PANEL LF CONFIGURATION GROUP 5", fp$`Configuration Group`)
fp$`Configuration Group` <- ifelse(str_detect(fp$DisplayNameLegacy, "NOTCHED") == TRUE, "PANEL LF CONFIGURATION GROUP 2", fp$`Configuration Group`)
fp$`Configuration Group` <- ifelse(str_detect(fp$DisplayNameLegacy, "PUNCHED") == TRUE, "PANEL LF CONFIGURATION GROUP 3", fp$`Configuration Group`)
fp$'Guided Selling Categories' <- ""

#add pricing update item category based on Color Description

fp$'Pricing Update Item Category' <- ""
fp <- fp %>%
  mutate(`Pricing Update Item Category` = ifelse(ColorDescription == "GALVALUME","Panels_Flats- Glume",`Pricing Update Item Category`))

fp <- fp %>%
  mutate(`Pricing Update Item Category` = ifelse(ColorDescription == "GALVALNIZED","Panels_Flats- Galv",`Pricing Update Item Category`))

fp <- fp %>%
  mutate(`Pricing Update Item Category` = ifelse(ColorDescription == "GALVANIZED","Panels_Flats- Galv",`Pricing Update Item Category`))

fp <- fp %>%
  mutate(`Pricing Update Item Category` = ifelse(ColorDescription == "WEATHERING STEEL","Panels_Flats- Weathering Steel",
                                                 `Pricing Update Item Category`))

fp <- fp %>%
  mutate(`Pricing Update Item Category` = ifelse(ColorDescription == "WEATHERING STEL","Panels_Flats- Weathering Steel",
                                                 `Pricing Update Item Category`))

fp <- fp %>%
  mutate(`Pricing Update Item Category` = ifelse(Substrate == "ALUMINUM","Panels_Flats- Exotic Metals (Copper, etc)",
                                                 `Pricing Update Item Category`))

fp <- fp %>%
  mutate(`Pricing Update Item Category` = ifelse(Substrate == "ALUM","Panels_Flats- Exotic Metals (Copper, etc)",
                                                 `Pricing Update Item Category`))

fp <- fp %>%
  mutate(`Pricing Update Item Category` = ifelse(str_detect(DisplayNameLegacy, "ALUM"),"Panels_Flats- Exotic Metals (Copper, etc)",
                                                 `Pricing Update Item Category`))

fp <- fp %>%
  mutate(`Pricing Update Item Category` = ifelse(Substrate == "COPPER","Panels_Flats- Exotic Metals (Copper, etc)",
                                                 `Pricing Update Item Category`))

fp <- fp %>%
  mutate(`Pricing Update Item Category` = ifelse(`Pricing Update Item Category` == "","Panels_Flats- Painted",
                                                 `Pricing Update Item Category`))

#update common misspellings in colorDescription
fp$ColorDescription <- gsub("WEATHERING STEL","WEATHERING STEEL",fp$ColorDescription)

fp$ColorDescription <- gsub("GALVALNIZED", "GALVANIZED",fp$ColorDescription)

fp$PaintType <- ifelse(fp$ColorDescription == "BONDERIZED", "BONDERIZED",fp$PaintType)

fp$ColorDescription <- gsub("BONDERIZED","",fp$ColorDescription)

#remove all rows that have an externalID with XX
fp <- fp[!grepl("XX", fp$ExternalId),]
#update various misspellings
fp$ColorDescription <- ifelse(str_detect(fp$ColorDescription, "CUSTOM") == TRUE, "CUSTOM COLOR",fp$ColorDescription)

fp$ColorDescription <- gsub("CHAMPAGNE","CHAM",fp$ColorDescription)

fp$ColorDescription <- ifelse(str_detect(fp$ColorDescription, "LOW GLOSS") == TRUE, "LG",fp$ColorDescription)

fp$ColorDescription <- gsub("STAINLESS STEEL","STAINLESS", fp$ColorDescription)

fp$ColorDescription <- gsub("BRKRED", "BRICK RED", fp$ColorDescription)

fp$ColorDescription <- gsub("CUSCOL", "CUSTOM COLOR", fp$ColorDescription)

fp$PaintType <- gsub("WEATHERSTL", "WEATHERING STEEL", fp$PaintType)


fp <- fp %>%
  mutate(DisplayName2 = paste(Profile, Width, Configuration, Gauge, Substrate, PaintType, Length, ColorDescription))
#create new display name for trim
#fp <- fp %>%
#mutate(DisplayName2 = paste(Profile, Configuration, Gauge, Substrate, PaintType, Length2, ColorDescription))

fp$SalesDescription2 <- fp$DisplayName2
fp$PurchaseDescription2 <- fp$DisplayName2
fp$Class <- fp$NS_ItemClassDesc

#fp$Class <- class_data$NS.Item.Class.Desc[match(fp$Raw.Item,class_data$Raw.Item)]

#create Guided Selling Categories based on if they have certain characters in their display name
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "SOFFIT") == TRUE, "Soffit Panel: Panels- Standard",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "5V") == TRUE, "5V-Crimp: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "PBR") == TRUE, "PBR/R-Panel: PBR-Panel",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "R-PANEL") == TRUE, "PBR/R-Panel: R-Panel",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "PBU") == TRUE, "PBU/U-Panel: PBU-Panel",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "U-PANEL") == TRUE, "PBU/U-Panel: U-Panel",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "PRO-PANEL") == TRUE, "Pro-Panel II: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "PRO PANEL") == TRUE, "Pro-Panel II: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "CLASSIC RIB") == TRUE, "Classic Rib: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$Profile, "1.25") == TRUE, '1.25" Corrugated: Panels',fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$Profile, "2.5") == TRUE, '2.5" Corrugated: PanelS',fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "STRONGPANEL") == TRUE, "Strongpanel: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "STRONG PANEL") == TRUE, "Strongpanel: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "SPAN-LINE") == TRUE, "Span-Line 36A: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "SPAN LINE") == TRUE, "Span-Line 36A: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "BI-RIB") == TRUE, "Bi-Rib: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "DELTA-RIB") == TRUE, "Delta Rib: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "DELTA RIB") == TRUE, "Delta Rib: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "DUAL RIB") == TRUE, "Dual Rib: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "DL-3") == TRUE, "DL-3 Panel: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "Curve Line") == TRUE, "Curve Line: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "7/8") == TRUE, '7/8" Corrugated: Panels',fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "V-LINE") == TRUE, "V-LINE 32: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "IC72") == TRUE, "IC72-Panel: Roof Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "T ARMOR") == TRUE, "T-Armor Series: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "[T]{1}[0-9]{1}") == TRUE, "T-Panel Series: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "[TL]{2}[0-9]{1}") == TRUE, "T-Panel Series: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "TDR6") == TRUE, "T-Panel Series: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "CONTEMPRA") == TRUE, "Contempra Series- Concealed Fastened Wall Panel System: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "CN88") == TRUE, "Contempra Series- Concealed Fastened Wall Panel System: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "APEX") == TRUE, "Apex Contempra- Concealed  Fastened Wall Panel System: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "AP1") == TRUE, "Apex Contempra- Concealed  Fastened Wall Panel System: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "TLC") == TRUE, "Flush Face Series- TLC: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "STONECREEK") == TRUE, "Stone Creek Wall Systems: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "VERTICAL SEAM") == TRUE, "Vertical Seam: Panels- Standard|Vertical Seam: Residential System: Panels- Standard",
                                         fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "IMAGE") == TRUE, "Image II- Exposed Fastened: Panels- Standard|Image II- Concealed Fastened: Panels- Standard|Image II- Residential System: Panels- Standard",
                                         fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "MAGNA-LOC") == TRUE, "Magna-Loc: Magna-Loc Panels- Standard",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "MGL") == TRUE, "Magna-Loc: Magna-Loc Panels- Standard",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "MAGNA-LOC 180") == TRUE, "Magna-Loc: Magna-Loc 180 Panels- Standard",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "SEAM-LOC") == TRUE, "Seam-Loc 24: Panels- Standard",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "SNAP-LOC") == TRUE, "Snap-Loc 24: Panels- Standard",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "CLIP-LOC") == TRUE, "Clip-Loc: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, 'PAN-MINI 1"') == TRUE, 'Mini-Batten and Pan: 1" Panels',fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, 'PAN-MINI 1.5"') == TRUE, 'Mini-Batten and Pan: 1.5" Panels',fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "MINI-BATTEN") == TRUE, "Mini-Batten and Pan: Mini-Batten",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "MINI BATTEN") == TRUE, "Mini-Batten and Pan: Mini-Batten",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, 'MINI PAN 1"') == TRUE, 'Mini-Batten and Pan: 1" Panels',fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, 'MINI PAN 1.5"') == TRUE, 'Mini-Batten and Pan: 1.5" Panels',fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "PAN-MAXI") == TRUE, "Maxi-Batten and Pan: Maxi-Batten",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, 'PAN-MAXI 1"') == TRUE, 'Maxi-Batten and Pan: 1" Panels',fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "MAXI BATTEN") == TRUE, "Maxi-Batten and Pan: Maxi-Batten",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "CEE") == TRUE, "Cees",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "ZEE") == TRUE, "Zees: Standard",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "CHANNEL") == TRUE, "Channels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "BASE ANGLE") == TRUE, "Base Angles",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "FLAT SHEET") == TRUE, "Flat Sheets",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "PAN-MINI") & str_detect(fp$Configuration, "CURVED"), "Curved Magna-Loc: Panels- In House Curved- Standard",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "EM1") == TRUE, "Empire Series- Concealed Fastened Wall Panel System: Panels",fp$`Guided Selling Categories`)
fp$'Guided Selling Categories' <- ifelse(str_detect(fp$DisplayName2, "EMPIRE") == TRUE, "Empire Series- Concealed Fastened Wall Panel System: Panels",fp$`Guided Selling Categories`)

#create new standardized profile column
fp$Profile_New <- fp$Profile

fp$Profile_New <- ifelse(fp$Profile == 'CORRUGATED 2.5', '2.5" CORRUGATED',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'CORRUGATED 2.5"', '2.5" CORRUGATED',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'CORRUGATED 1.25', '1.25" CORRUGATED',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'CORRUGATED 1.25"', '2.5" CORRUGATED',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'CORRUGATED 7/8', '7/8" CORRUGATED',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'CORRUGATED 7/8"', '7/8" CORRUGATED',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == '5V CRIMP', '5V-CRIMP',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'BI RIB', 'BI-RIB',fp$Profile_New)
fp$Configuration <- ifelse((str_detect(fp$Profile,"PERF") & str_detect(fp$Configuration,"PERF") == FALSE), paste("PERF",fp$Configuration),fp$Configuration)
fp$Profile_New <- ifelse(fp$Profile == 'CLASSIC RIB- PERF', 'CLASSIC RIB',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'CLIP-LOC 16', 'CLIP-LOC',fp$Profile_New)
fp$Configuration <- ifelse((str_detect(fp$Profile,"ROOF") & str_detect(fp$Configuration,"ROOF") == FALSE), paste(fp$Configuration," ROOF"),fp$Configuration)
fp$Configuration <- ifelse((str_detect(fp$Profile,"WALL") & str_detect(fp$Configuration,"WALL") == FALSE), paste(fp$Configuration," WALL"),fp$Configuration)
fp$Profile_New <- gsub("- ROOF","",fp$Profile_New)
fp$Profile_New <- gsub("- WALL","",fp$Profile_New)
fp$Profile_New <- gsub("ROOF","",fp$Profile_New)
fp$Profile_New <- gsub("WALL","",fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile_New == 'IC72', 'IC72 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'IMG12', 'IMAGE II',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'MGL18', 'MAGNA-LOC 180',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'MAXI BATTEN CAP', 'MAXI-BATTEN',fp$Profile_New)
fp$Configuration <- ifelse(fp$Configuration == 'MAXI', '',fp$Configuration)
fp$Profile_New <- ifelse(fp$Profile == 'MINI BATTEN CAP', 'MINI-BATTEN',fp$Profile_New)
fp$Configuration <- ifelse(fp$Configuration == 'MINI', '',fp$Configuration)
fp$Profile_New <- ifelse(fp$Profile == 'PRO-PANEL II - CF40', 'PRO-PANEL II',fp$Profile_New)
fp$Configuration <- ifelse(fp$Configuration == 'REVERSE ROLLED', 'REVERSE ROLL',fp$Configuration)
fp$Profile_New <- ifelse(fp$Profile == 'PRO PANEL II', 'PRO-PANEL II',fp$Profile_New)
fp$Configuration <- ifelse((str_detect(fp$Profile,"LINER") & str_detect(fp$Configuration,"LINER") == FALSE), paste(fp$Configuration," LINER"),fp$Configuration)
fp$Profile_New <- gsub("-LINER","",fp$Profile_New)
fp$Configuration <- ifelse((str_detect(fp$Profile,"REVERSE ROLL") & str_detect(fp$Configuration,"REVERSE ROLL") == FALSE), paste(fp$Configuration," REVERSE ROLL"),fp$Configuration)
fp$Configuration <- ifelse((str_detect(fp$Profile,"REV ROLL") & str_detect(fp$Configuration,"REVERSE ROLL") == FALSE), paste(fp$Configuration," REVERSE ROLL"),fp$Configuration)
fp$Profile_New <- gsub("- REVERSE ROLL","",fp$Profile_New)
fp$Profile_New <- gsub("REVERSE ROLL","",fp$Profile_New)
fp$Profile_New <- gsub("- REV ROLL","",fp$Profile_New)
fp$Profile_New <- gsub("REV ROLL","",fp$Profile_New)
fp$Profile_New <- gsub(" - CF40","",fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'PBR', 'PBR-PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'SEAM-LOC 24', 'SEAM-LOC',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'SNPLC', 'SNAP-LOC',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'SNAP-LOC 24', 'SNAP-LOC',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'SOFFIT PANEL (1" )', 'SOFFIT PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'SOFFIT PANEL (1")', 'SOFFIT PANEL',fp$Profile_New)
fp$Configuration <- ifelse((str_detect(fp$Profile,"T ARMOR 2.375") & str_detect(fp$Configuration,"2.375") == FALSE), paste('2.375" ',fp$Configuration),fp$Configuration)
fp$Configuration <- ifelse((str_detect(fp$Profile,'T ARMOR 3') & str_detect(fp$Configuration,"3") == FALSE), paste('3" ',fp$Configuration),fp$Configuration)
fp$Profile_New <- ifelse(fp$Profile == 'T ARMOR 2.375', 'T-ARMOR',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T ARMOR 2.375"', 'T-ARMOR',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T ARMOR 3', 'T-ARMOR',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T ARMOR 3"', 'T-ARMOR',fp$Profile_New)
fp$Profile_New <- gsub("FLUSH","",fp$Profile_New)
fp$Configuration <- ifelse((str_detect(fp$Profile,'EMBOSSED') & str_detect(fp$Configuration,"EMBOSSED") == FALSE), paste('EMBOSSED',fp$Configuration),fp$Configuration)
fp$Profile_New <- gsub("EMBOSSED","",fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TLC1', 'TLC-1',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TLC2', 'TLC-2',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TLC3', 'TLC-3',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TLC4', 'TLC-4',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TLC5', 'TLC-5',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TLC6', 'TLC-6',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TLC7', 'TLC-7',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TLC8', 'TLC-8',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TLC9', 'TLC-9',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TLC10', 'TLC-10',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'PBUPN', 'PBU',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'PBU', 'PBU-PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'VERSM', 'VERTICAL SEAM',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'V-LINE 32', 'V-LINE',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T1 EXPOSED FASTENED', 'T1 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T2 EXPOSED FASTENED', 'T2 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T3 EXPOSED FASTENED', 'T3 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T3 EXPOSED FASTENED', 'T3 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T4 EXPOSED FASTENED', 'T4 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T5 EXPOSED FASTENED', 'T5 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T5 EXPOSED FASTENED ', 'T5 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T5', 'T5 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T6 EXPOSED FASTENED', 'T6 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T6A EXPOSED FASTENED', 'T6-A PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T7 EXPOSED FASTENED', 'T7 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T7-A EXP FASTENED', 'T7-A PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T7-B EXP FASTENED', 'T7-B PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T7 A&B EXP FASTENED', 'T7 A&B PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T8 EXPOSED FASTENED', 'T8 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T9 EXPOSED FASTENED', 'T9 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T10A VERTI-LINE', 'T10-A PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T10B VERTI-LINE', 'T10-B PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T10C VERTI-LINE', 'T10-C PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T10D VERTI-LINE', 'T10-D PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T10A', 'T10-A PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T10E VERTI-LINE', 'T10-E PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T10F VERTI-LINE', 'T10-F PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T11A EXPOSED FASTENED', 'T11A PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T11A EXPOSED FASTEND', 'T11A PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T12 EXPOSED FASTENED', 'T12 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T13A INDUSTRIAL RIB', 'T13-A PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T13 INDUSTRIAL RIB', 'T13 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T13B INDUSTRIAL RIB', 'T13-B PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T14 INDUSTRIAL RIB', 'T14 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T15 INDUSTRIAL RIB', 'T15 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T16 INDUSTRIAL RIB', 'T16 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T23 INDUSTRIAL RIB', 'T23 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T24 INDUSTRIAL RIB', 'T24 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T25 INDUSTRIAL RIB', 'T25 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T2630 DEEP RIB', 'T2630 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'T2832 DEEP RIB', 'T2832 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL17', 'TL-17 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL17A', 'TL-17A',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL17C', 'TL-17C',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL17D', 'TL-17D',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL19A LINER', 'TL-19A PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL18', 'TL-18 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL19B LINER', 'TL-19B',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL20', 'TL-20 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL21', 'TL-21 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL1222', 'TL-1222 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL1222 FLUSH FACE', 'TL-1222 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TL1251 FLUSH FACE', 'TL-1241 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'APEX, 12" COVERAGE', 'AP1-1212',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'APEX, 16" COVERAGE', 'AP1-1653',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'CONTEMPRA,12" COVERAGE', 'CN88-1212',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'CONTEMPRA,16" COVERAGE', 'CN88-1653',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'EMPIRE SERIES, 16" COVERAGE', 'EM15-168',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Profile == 'TDR6 INDUSTRIAL RIB', 'TDR-6 PANEL',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '27745XX', 'EM1-1212',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '29745XX', 'EM1-1212',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '30745XX', 'EM1-1212',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '27750XX', 'EM1-1653',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '29750XX', 'EM1-1653',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '30750XX', 'EM1-1653',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '27740XX', 'EM15-126',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '29740XX', 'EM15-126',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '30740XX', 'EM15-126',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '27742XX', 'EM15-168',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '29742XX', 'EM15-168',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '30742XX', 'EM15-168',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '27770XX', 'EM15-1266',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '27775XX', 'EM15-1275',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '29775XX', 'EM15-1275',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '30775XX', 'EM15-1275',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '27780XX', 'EM15-1284',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '29780XX', 'EM15-1284',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '30780XX', 'EM15-1284',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '27785XX', 'EM15-1293',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '29785XX', 'EM15-1293',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '29785XX', 'EM15-1293',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '27781XX', 'EM15-1222',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '29781XX', 'EM15-1222',fp$Profile_New)
fp$Profile_New <- ifelse(fp$Raw.Item == '30781XX', 'EM15-1222',fp$Profile_New)
fp$Configuration <- ifelse((str_detect(fp$Profile,'STRIATED') & str_detect(fp$Configuration,"STRIATED") == FALSE), paste('STRIATED',fp$Configuration),fp$Configuration)
fp$Profile_New <- gsub("STRIATED","",fp$Profile_New)
fp$Profile_New <- gsub("-CF40","",fp$Profile_New)
fp$Profile_New <- gsub("-NO WARRANTY","",fp$Profile_New)
fp$Profile_New <- gsub("-NO WARANTY","",fp$Profile_New)
fp$Profile_New <- gsub("-NO WNTY","",fp$Profile_New)
fp$Profile_New <- gsub("-VERSA30","",fp$Profile_New)

fp$Configuration <- ifelse((str_detect(fp$Profile,'WITH DRIP SHIELD') & str_detect(fp$Configuration,"DRIP SHIELD") == FALSE), paste('DRIP SHIELD',fp$Configuration),fp$Configuration)
fp$Profile_New <- gsub("WITH DRIP SHIELD","",fp$Profile_New)

fp$Configuration <- ifelse((str_detect(fp$Profile,'DRIP SHIELD') & str_detect(fp$Configuration,"DRIP SHIELD") == FALSE), paste('DRIP SHIELD',fp$Configuration),fp$Configuration)
fp$Profile_New <- gsub("DRIP SHIELD","",fp$Profile_New)

#get rid of all double spaces and na's
fp <- fp %>%
  mutate_all(as.character)

fp[is.na(fp)] <- " "

fp <- fp %>%
  mutate_all(str_squish)
#reorganize the columns
fp2 <- fp %>%
  select(ExternalId, Name, Raw.Item, UPCcode, Profile_New, Profile, Configuration, Substrate, PaintType, Length, Thickness,
         Width, Gauge, ItemWeight, ColorCode, ColorDescription, Class, DisplayName2, SalesDescription2, UnitsType2, SupplyType,
         DefaultUnit, StockUnit, PurchaseUnit, SalesUnit, Grade, PurchaseDescription2, SupplyReplenishmentMethod,
         IsInactive, `AMS Eclipse Decimal Inch`, `Eclipse Stock Panel Flag`, `TRANSFER RATE`,`TRANSFER RATE U/M`,`OZLINK CLASS`,
         `Configuration Group`, `Guided Selling Categories`,`Pricing Update Item Category`,everything())

write.csv(fp2,"N:\\Master Data\\Branch Files\\Sellersburg\\R extract\\Sellersburg_Panels_Extract.csv", row.names=FALSE)