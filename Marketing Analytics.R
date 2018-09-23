rm(list=ls())
library(data.table)
library(tidyverse)
library(dplyr)

setwd('C:\\Drive E\\Business Analytics\\Other\\Marketing Analytics Competition\\Data\\Customers')
rm(list=ls())
df1<-fread('utdallas_contest_data-1.csv')
df1 <- filter(df1, wc_bought_any==1) ##selecting those who baught

#df<-select(df,wc_num_items,wc_big_ticket_bought_any,wc_big_ticket_num_items,wc_big_ticket_amt_spent,
#wc_low_ticket_amt_spent,wc_low_ticket_bought_any,wc_low_ticket_num_items,client_times_shopped_30_days,
#client_shopped_3_mos)

#which(names(df)=="client_shopped_3_mos")

df<-df1[,618:626] ##historic columns
df[df==""]<-'z'
#df <- df %>%drop_na()
#df[complete.cases(df),]

#df[df=='NA']<-0

##changipurchase amounts to numeric
df$wc_num_items2<- as.numeric(as.character(factor(df$wc_num_item,
               levels = c("A", "B", "C",'z'),labels = c(1, 2,4,0)))) 

df$wc_big_ticket_num_items2<- as.numeric(as.character(factor(df$wc_big_ticket_num_items,
              levels = c("A", "B", "C",'z'),labels = c(1, 2,4,0)))) 

df$wc_low_ticket_num_items2<- as.numeric(as.character(factor(df$wc_low_ticket_num_items,
               levels = c("A", "B", "C",'z'),labels = c(1, 2,4,0)))) 

df$wc_big_ticket_amt_spent2 <- as.numeric(as.character(factor(df$wc_big_ticket_amt_spent,
                  levels = c("A", "B", "C", "D","E",'z'),labels = c(50, 150,250,400,600,0))))

df$wc_low_ticket_amt_spent2 <- as.numeric(as.character(factor(df$wc_low_ticket_amt_spent,
         levels = c("A", "B", "C","D","E","F","G",'z'),labels = c(50, 75,150,250,400,750,1200,0))))

df$client_times_shopped_30_days2<- as.numeric(as.character(factor(df$client_times_shopped_30_days,
     levels = c("A", "B", "C","D","E","F","G",'H','I','z'),labels = c(0,1,2,3,4,5,7.5,12,15,0))))

df$freq<-(df$wc_big_ticket_num_items2)+(df$wc_low_ticket_num_items2) ##frequency 

df$monet<-(df$wc_big_ticket_amt_spent2)+(df$wc_low_ticket_amt_spent2)/df$freq ##monetary

df$rec<-(df$client_times_shopped_30_days2) ##recency


##Changing the income to numeric

df1<-fread('df_cust_numvar.csv')
df1$wc_low_ticket
df1$Est_HH_Income_3_U324064_num<- as.numeric(as.character(factor(df1$Est_HH_Income_3_U324064,
                                                                 levels = c("A", "B", "C","D","E","F","G",'H','I','J',"K","L",'M','N','O'),
                                                                 labels = c(10000,17000,24500,35000,45000,55000,67000,112000,137000,175000,225000,325000,450000,600000,80))))

##making logs of monetary, frequency and recency
cust<-df
cust<-df[,10:12]
cust$freq.log <- log(cust$freq)
cust$monet.log <- cust$monet + 15
cust$monet.log<-log(cust$monet.log)
cust$rec.log<-cust$rec+0.001
cust$rec.log<-cust$rec.log

##K-Means Clustering

seed        <-	2	# NOT a good random seed!
maxClusters	<-	10 #try with 50, then with 15

## Use within-group variation to choose k
wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) { # i represents the k value
  set.seed(seed)
  model <- kmeans(cust,centers=i,nstart=10)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

##K_means plotting
k=3

output <- kmeans(cust, centers = k, nstart = 20)
var.name <- paste("cluster", k, sep="_")
cust[,(var.name)] <- output$cluster
cust[,(var.name)] <- factor(cust[,(var.name)], levels = c(1:k))
output$centers



##df_c Making (our Customers with all columns)
rm(df)
df<-fread('utdallas_contest_data-1.csv')
df <- filter(df, wc_bought_any==1)
df_c<-df
df_c$freq<-cust$freq
df_c$monet<-cust$monet
df_c$rec<-cust$rec
df_c$clusters<-cust$cluster_3
df_c$freq.log<-cust$freq.log
df_c$monet.log<-cust$monet.log
df_c$rec.log<-cust$rec.log

write.csv(df_c,'df_c.csv')

##Making Our Customer with specific demographic columns
df_c<-fread('df_c.csv')
our_cust<-select(df_c,match,match_level_1,State_99039,Zip_Code_99040,Homeowner_Renter_90303,
                 Gender_Code_90304,
                 Age_Range_90305,
                 Est_HH_Income_90309
                 ,Home_Market_Value_90310
                 ,Designated_Mkt_Area_U324014
                 ,Cen10_Med_HH_Inc_U324028
                 ,Cen10_Pct_BCol_Emp_U324034
                 ,Cen10_Pct_WhtColEmp_U324035
                 ,Cen10_Pct_Married_U324036
                 ,Cen10_Pct_AbvPovrty_U324042
                 ,Cen10_Pct_BelPovLvl_U324043
                 ,Family_PositionCode_U324059
                 ,Est_HH_Income_3_U324064
                 ,Dual_Income_Indx_3_U324067
                 ,Number_Children_U324074
                 ,Byr_Cat_Total_Ordrs_U324419
                 ,Byr_Cat_Total_Dollr_U324420
                 ,Byr_Cat_Online_Ords_U324421
                 ,Byr_Cat_Online_Doll_U324422
                 ,Byr_Cat_Offline_Ord_U324423
                 ,Byr_Cat_Off_Dollars_U324424
                 ,Byr_Cat_Avg_Ord_Dol_U324425
                 ,Byr_Cat_Avg_Onl_Dol_U324426
                 ,Byr_Cat_Avg_Off_Dol_U324427
                 ,Byr_Ret_Orders_U324481
                 ,Byr_Ret_Dollars_U324482
                 ,Chan_Pref_FirstPref_U324542
                 ,Chan_Pref_Idx_Email_U324545
                 ,Chan_Pref_Idx_Mail_U324546
                 ,Uninsured_Index_U324550
                 ,Digital_Dest_Groups_U324611
                 ,Digital_Devices_U324612
                 ,Digi_Neighborhoods_U324614
                 ,Numeric_Cty_Code_U324013
                 ,Bank_Card_New_IssDt_U324098
                 ,Credit_Active_U324101
                 ,Credit_Actv_Spc_App_U324108
                 ,Srvy_Occ_Homemaker_U324292
                 ,Srvy_Occ_Managemnt_U324293
                 ,Srvy_Occ_Sale_Mktng_U324294
                 ,Srvy_Occ_TrdesmnLbr_U324295
                 ,Srvy_Occ_Prof_Tech_U324296
                 ,Srvy_Occ_Retired_U324297
                 ,Srvy_Occ_Self_Emp_U324298
                 ,Srvy_Occ_Op_HomeBiz_U324299
                 ,Srvy_Occ_Student_U324300
                 ,Srvy_Occ_MilityGovt_U324301
                 ,Srvy_Occ_Doctor_U324302
                 ,Srvy_Occ_Full_Time_U324303
                 ,Srvy_Occ_Nurse_U324304
                 ,Srvy_Occ_Part_Time_U324305
                 ,Srvy_Occ_Teacher_U324306
                 ,Srvy_Likes_To_Read_U324307
                 ,Srvy_Purch_HomeDcor_U324399
                 ,Srvy_PrchBeautyProd_U324400
                 ,Srvy_PurchClubStore_U324401
                 ,Srvy_Purch_FastFood_U324402
                 ,Srvy_Prch_SpcltyBty_U324403
                 ,Srvy_Group_DIY_U324407
                 ,Srvy_Group_Fine_Art_U324408
                 ,Srvy_Group_HomeLife_U324409
                 ,Srvy_Group_Lux_Life_U324410
                 ,Srvy_Grp_Outdoor_U324411
                 ,Srvy_Grp_Phys_Actv_U324412
                 ,Srvy_Grp_Sprts_Enth_U324413
                 ,Srvy_Group_Techies_U324414
                 ,Srvy_Group_Up_Town_U324415
                 ,Byr_Cat_App_Teen_U324433
                 ,Byr_Ret_Apprl_Gen_U324483
                 ,Byr_Ret_App_Teen_U324486
                 ,Byr_Ret_App_Wmn_Pet_U324488
                 ,Byr_Ret_AppWmnPlsSz_U324489
                 ,Est_HH_Income_90309
                 ,Mail_Order_Byr_90315
                 ,Mail_Order_Resp_90316
                 ,State_U324007
                 ,Mail_Order_Respnder_U324068
                 ,Mail_Order_Byr_U324069
                 ,Online_Access_U324085
                 ,Bank_Card_U324099
                 ,Ret_Card_U324100
                 ,Presence_Comp_Owner_U324133
                 ,Email_Access_U324134
                 ,Presence_of_Donor_U324140
                 ,Est_HH_Debt_Level_U324161
                 ,Srvy_Mail_Ord_Clthg_U324257
                 ,srvy_rding_Fashion_U324316
                 ,srvy_rding_Magazine_U324337
                 ,Srvy_Purch_Coupons_U324404
                 ,Byr_Cat_Last_OrdDt_U324416
                 ,Byr_Cat_Lst_OnOrdDt_U324417
                 ,ByrCat_Lst_OffOrdDt_U324418
                 ,Byr_Cat_Appar_Women_U324437
                 ,Byr_Ret_Appar_Women_U324490
                 ,Byr_Cat_Spend_Patrn_U324526
                 ,Country_of_Origin_U324574)

our_cust$freq<-df_c$freq
our_cust$monet<-df_c$monet
our_cust$rec<-df_c$rec
our_cust$clusters<-df_c$clusters
our_cust$freq.log<-df_c$freq.log
our_cust$monet.log<-df_c$monet.log
our_cust$rec.log<-df_c$rec.log
our_cust$Total_Amount_Dollar<-(our_cust$monet)*(our_cust$freq)


##Group by monet-states
df1 %>% 
  group_by(states) %>%
  summarize(monet = sum(monet)) %>%
  arrange(desc(monet))

##Renaming ccustomers(if needed)
our_cust$clusters <- factor(our_cust$clusters, levels = c(1,2,3),
         labels = c("1) Loyal Customers", "2) Needing Attention", "3) Hibernating Customers"))

##making high,med, and low customers
high_cust<-filter(our_cust, Customers==1)
med_cust<-filter(our_cust,Customers==2)
low_cust<-filter(our_cust,Customers==3)

##Saving them as CSV files
write.csv(our_cust,'our_cust.csv')
write.csv(low_cust, 'low_cust.csv')
write.csv(med_cust,'med_cust.csv')
write.csv(high_cust,'high_cust.csv')

#####K-Means Clustering the market(to make Switching Customers)
dfex<-fread('utdallas_contest_data-1-HHI.csv')
exc1<-select(exc, HHI_value)

#finding K
seed        <-	2	# NOT a good random seed!
maxClusters	<-	10 #try with 50, then with 15

## Use within-group variation to choose k
wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) { # i represents the k value
  set.seed(seed)
  model <- kmeans(exc1,centers=i,nstart=10)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

##K_means Plotting
k=2

output <- kmeans(exc1, centers = k, nstart = 20)
var.name <- paste("cluster", k, sep="_")
exc1[,(var.name)] <- output$cluster
exc1[,(var.name)] <- factor(exc1[,(var.name)], levels = c(1:k))
output$centers
exc1$cluster_2<-output$cluster
hist(exc1$HHI_value)

#subsetting and saving exchange customers
ex_cust<-filter(exc, df_ex$ex_customer==1)

write.csv(ex_cust,'ex_cust.csv')


##Mapplot
library(usmap)
library(ggmap)
library(ggplot2)
library(rworldmap)
library(RgoogleMaps)

####US Map Plotting
our_cust<-fread('our_cust.csv')
#Method 1. by states(You shoould have a column named 'state')
our_cust$state<-our_cust$State_99039

##All STates by monetary Amount
usmap::plot_usmap(data = our_cust, values = "monet", lines = "red") + 
  scale_fill_continuous( name = "Amout of Purchase(($)", label = scales::comma) + 
  theme(legend.position = "right") ##you can remove the low and high color 

##Some States by monetary amount
plot_usmap( data = our_cust, values = "tot_amt", include = c("CA", "ID", "NV", "OR"), lines = "red"
) + scale_fill_continuous(
  low = "white", high = "red", name = "Population (2015)", label = scales::comma
) + labs(title = "Western US States", subtitle = "These are the states in the Pacific Timezone.") +
  theme(legend.position = "right")

###Method 2. BY black cricles-according to monetary amount
usmap::plot_usmap(data = our_cust, values = "monet", lines = "red") + 
  scale_fill_continuous( name = "AMout of Purchase", label = scales::comma) + 
  theme(legend.position = "right")



##Method 3. Mapping by zip codes
rm(list=ls())
fm<- read.csv("our_cust.csv", header = TRUE, sep = ",")
data(zipcode)
fm$zip<- clean.zipcodes(fm$Zip_Code_99040)
fm<- merge(fm, zipcode, by.x='zip', by.y='zip')

density<- ddply(fm, .(State_99039), "nrow")
names(density)[2] <- "count"
FM<- merge(fm, density)
##removing duplicate customers if there are any(but do not run it if yo know there is not any duplicate)
#duplicated(FM$city.x)
#FM[duplicated(FM$city.x),]
#unique(FM[duplicated(FM$city.x),])
#FM<-FM[!duplicated(FM$city.x),]

##if you didn't get a result, reduce the zoom to 4 and then try again
ny <- get_map(location='united state',zoom=6, maptype="terrain")
ggmap(ny)
#Plot by Law category
ggmap(ny)+
  geom_point(
    aes(x=longitude, y=latitude, color='red', alpha=1/10),
    data=FM
  )

## By red points and a bit different shape
map<-get_map(location='united states', zoom=4, maptype='roadmap')
ggmap(map)+geom_point(aes(x=longitude, y=latitude, size=(count)),color='g', data=FM, alpha=.5)

##Bar plot and group bar plot
counts <- table(our_cust$state)
barplot(counts, main="State Distribution", 
        xlab="States")

##Grouped
counts <- table(our_cust$state, med_c$state,high_c$state)
barplot(counts, main="State Distribution",
        xlab="States", col=c("darkblue","red", 'green'),
        legend = rownames(counts), beside=TRUE)

###Grouped boxplot

#df$Byr_Cat_Online_Ords_U324421<-log(df$Byr_Cat_Online_Ords_U324421+.01)
boxplot(Byr_Cat_Online_Ords_U324421 ~ clusters, data = df, col = "lightgreen",
        main='Byr_Cat_Online_Ords_U324421')

#df$Byr_Cat_Total_Ordrs_U324419<-log(df$Byr_Cat_Total_Ordrs_U324419+.01)
boxplot(Byr_Cat_Total_Ordrs_U324419 ~ clusters, data = df, col = "lightgreen",
        main='Byr_Cat_Total_Ordrs_U324419')


