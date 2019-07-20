#######################################
##    Econometrics of Marketing      ##
##          RFM Analysis             ##
#######################################
## Author: Maria Bugge               ##
## Created: September 2018           ##
## Last modified: November 18        ##
#######################################

# 1- Set up ---------------------------
library(readxl)
db <- read_excel("df.xlsx")
View(db)
#First things first: remove the last two rows (they are just column totals)
db <-db[! is.na(db$ID_INDIVIDU),]
#35957 observations; 35955 customers.
#Active: 16512 (46%); inactive: 19443 (54%)

db$actif[! is.na(db$MONTANT_CUMULE)]<-1
db$actif[is.na(db$MONTANT_CUMULE)]<-0

# Info --------------------------------------------
#New customers = customers < 1 year 

#step1: RFM analysis for all the active customers
#step 2: special group for new customers (active under 1 year)
#step3: inactive group
#The new customers and old customers are a separate group BUT you do RFM on ALL active customers,
#then later you separate them (or else doing RFM has no point)

# 2 - RFM -----------------------------------------
#RFM, the 3 variables used: db$recene (R); db$montant_cumule (M) and db$nb_visites (F)
#We do RFM analysis on all active customers

# 2a) Monetary -------
#I plot histograms to get an idea on where to cut
library(ggplot2)
ggplot(subset(db,! is.na(db$MONTANT_CUMULE))) + 
  geom_histogram(aes(MONTANT_CUMULE))
 #weak: <165; medium: 165<= . <= 342; high: >342

db$Mcut[db$actif=="oui" & db$MONTANT_CUMULE<165]<-1
db$Mcut[db$actif=="oui" & db$MONTANT_CUMULE>=165 & db$MONTANT_CUMULE<=342]<-2
db$Mcut[db$actif=="oui" & db$MONTANT_CUMULE>342]<-3

#2b) Frequency -------

db$Fcut[db$actif=="oui" & db$NB_VISITE==1]<-1
db$Fcut[db$actif=="oui" & db$NB_VISITE>=2 & db$NB_VISITE<=3]<-2
db$Fcut[db$actif=="oui" & db$NB_VISITE>3]<-3

# FM cut ------------
db$FMcut[db$actif=="oui" & (db$Mcut==1 & db$Fcut == 1) | db$actif=="oui" & (db$Mcut == 1 & db$Fcut == 2) | db$actif=="oui" & (db$Mcut == 2 & db$Fcut == 1)]<-"w"
db$FMcut[db$actif=="oui" & (db$Mcut==2 & db$Fcut == 2) | db$actif=="oui" & (db$Mcut == 1 & db$Fcut == 3) | db$actif=="oui" & (db$Mcut == 3 & db$Fcut == 1)]<-"m"
db$FMcut[db$actif=="oui" & (db$Mcut==3 & db$Fcut == 2) | db$actif=="oui" & (db$Mcut == 3 & db$Fcut == 3) | db$actif=="oui" &  (db$Mcut == 2 & db$Fcut == 3)]<-"h"
#db$FMcut[db$Mcut==0 & db$Fcut==0]<-"unactive"

#2c Recency --------
#Now for the recency. Recency = number of days since last purchase --> the higher the worse the client
ggplot(subset(db,! is.na(db$MONTANT_CUMULE))) + 
  geom_histogram(aes(RECENCE))

#the higher the number of days the weaker the customer
db$Rcut[db$actif=="oui" & db$RECENCE<87]<-3
db$Rcut[db$actif=="oui" & db$RECENCE>=87 & db$RECENCE<277]<-2
db$Rcut[db$actif=="oui" & db$RECENCE>277]<-1
#db$Rcut[db$actif != "oui"]<-0
#---
RFM<-table(active$Rcut,active$FMcut)

db$cust[db$ANCIENNETE<=1]<-"newcustomers"
db$cust[db$actif=="oui" & ((db$Rcut==3 & db$FMcut=="h") | (db$FMcut=="h" & db$Rcut==2))]<-"topcustomers"
db$cust[db$actif=="oui" & ((db$FMcut=="m" & db$Rcut==3) | (db$Rcut==2 & db$FMcut=="m"))]<-"goodcustomers"
db$cust[db$actif=="oui" & ((db$FMcut=="h" & db$Rcut==1) | (db$FMcut=="m" & db$Rcut==1))]<-"decelaration"
db$cust[db$actif=="oui" & ((db$FMcut=="w" & db$Rcut==3) | (db$FMcut=="w" & db$Rcut==2))]<-"smallcustomers"
db$cust[db$actif=="oui" & (db$FMcut=="w" & db$Rcut==1)]<-"weakcustomers"
db$cust[db$actif=="non"]<-"unactive"

sum(db$ANCIENNETE<=1 & is.na(db$MONTANT_CUMULE)) #we have 186 UNACTIVE NEW CUSTOMERS

#Loyalty program
db$loyalty[db$NB_CADEAUX==0]<-"insensible"
db$loyalty[db$NB_CADEAUX>=1 & db$NB_CADEAUX<=3]<-"reactive"
db$loyalty[db$NB_CADEAUX>3]<-"addict"
db$loyalty[is.na(db$NB_CADEAUX)]<-"empty"

#Eclectisism: number of different lines (1 line, 2 lines, 3 lines, empty)
db$eclectism[(db$NB_LIGNE_DIFF==1)]<-1
db$eclectism[db$NB_LIGNE_DIFF==2]<-2
db$eclectism[db$NB_LIGNE_DIFF==3]<-3
db$eclectism[is.na(db$NB_LIGNE_DIFF)]<-0


#Diversifications: number of different families
db$diversif[db$NB_FAM_DIFF<=2 & db$NB_FAM_DIFF>=1]<-"basic"
db$diversif[db$NB_FAM_DIFF>=3 & db$NB_FAM_DIFF<=5]<-"diversified"
db$diversif[db$NB_FAM_DIFF>=6 & db$NB_FAM_DIFF<=15]<-"addict"
db$diversif[is.na(db$NB_FAM_DIFF)]<-"empty"

#Store attachment: part of purchases (visits) in the administrator store:
db$store_attach[db$PART_VIST_MAG_GEST<0.7]<-"weak"
db$store_attach[db$PART_VIST_MAG_GEST >=0.7 & db$PART_VIST_MAG_GEST<1]<-"medium"
db$store_attach[db$PART_VIST_MAG_GEST ==1]<-"high"


db$NEWCUSTOMER[db$ANCIENNETE<=1]<-"YES"
#-----------------------------------------
