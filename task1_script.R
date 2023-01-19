library(readxl)
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(stringr)
library(dplyr)
library(scales)

# Task 1 

# Clean the two datasets (fix outliers, anamolies, check for nulls, etc)

qvi_purchase <- read.csv("QVI_purchase_behaviour.csv")
qvi_purchase

qvi_trans <- read_excel("QVI_transaction_data.xlsx")
qvi_trans

# column names

col_qvi <- colnames(qvi_purchase)
trans_col <- colnames(qvi_trans)

# Clean data 

#==========================#
# For transaction data set 
#==========================#

#1)

# check for data types

trans_col # names of columns 

str(qvi_trans)

dt_table<-data.frame(Names=names(qvi_trans), Type=sapply(qvi_trans,class))
labels(dt_table)

# change data DATE column into date type. 

qvi_trans$DATE <- as.Date(qvi_trans$DATE, origin = "1899-12-30")

str(qvi_trans)

#2)

#Check for missing data across the data set

sum(is.null(qvi_trans))
sum(is.na(qvi_trans))

# no null or na data. 

#3)

#Check product names are correctly entered. 


unique(qvi_trans$PROD_NAME)
summary(qvi_trans$PROD_NAME)

#Make sure we are looking at chip products. Remove any incorrect 
#products and clean the PROD NAME e.g. characters etc.\

product_unique <- unique(qvi_trans[ , "PROD_NAME"])
split_words_by <- strsplit(as.character(product_unique), " ")

productWords <- data.table(unlist(split_words_by))


setnames(productWords, 'words')

# Remove special characters from words

x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="


productWords_clean <- str_replace_all(productWords, "[^[:alnum:]]", " ")

productWords_num2 <- trimws(gsub("\\w*[0-9]+\\w*\\s*", "", productWords_clean))


#productWords_final <- trimws(gsub("[^\\s]*[0-9][^\\s]*", "", productWords_clean, perl=T))

#productWords_final <- strsplit(as.character(productWords_final), " ")

# use subset to remove empty values ""

productWords_final <- subset(productWords_num2[1], productWords_num2[1]!=" ")

productWords_final

# Frequency table
productword_freq <- strsplit(as.character(productWords_final), " ")
productword_freqtab <- table(productword_freq)

# Remove Salsa

qvi_superclean <- qvi_trans[!grepl("salsa", qvi_trans$PROD_NAME, ignore.case = TRUE), ]


#4)

#Check for outliers

summary(qvi_superclean)

# interested in product quantity and total sales for outliers. 

qty_and_sales <- summary(qvi_superclean[c("PROD_QTY","TOT_SALES")])


# Look at boxplots for detecting outliers. 
# ======================================#

boxplot(qvi_superclean$PROD_QTY, horizontal = TRUE, main = "Product Quantity per Transaction")
boxplot(qvi_superclean$TOT_SALES, horizontal = TRUE,
        main = "Total Sales per Transaction")

boxplot.stats(qvi_superclean$PROD_QTY)$out

hist(qvi_superclean$PROD_QTY)
hist(qvi_superclean$TOT_SALES, breaks = 150)

#Thanks to the which() function it is possible to extract the row number corresponding to these outliers:
# e.g.:  
#  out <- boxplot.stats(dat$hwy)$out
#out_ind <- which(dat$hwy %in% c(out))
#out_ind

#outliers index
# boxplot()$out shows the datapoints which are >1.5x IQR
#
outliers_pq <- boxplot.stats(qvi_superclean$PROD_QTY)$out
outliers_idx_pq <- which(qvi_superclean$PROD_QTY %in% c(outliers_pq))


length(qvi_superclean$PROD_QTY)

#no_outliers_trans <- qvi_trans[-outliers_idx_pq, ]

#outliers sales

#qvi_pq <- qvi_trans$TOT_SALES

#outliers_sales <- boxplot.stats(qvi_pq)$out
#outliers_idx_sal <- which(qvi_pq %in% c(outliers_idx_pq))
#
#no_outliers_qvi <- no_outliers_trans[-outliers_idx_sal, ]

#clean_qvi <- no_outliers_qvi

#IQR takes too many datapoints away

# CHECK the 200 OUTLIER

qvi_superclean[qvi_superclean$PROD_QTY == 200, ]

unique(qvi_superclean[qvi_superclean$PROD_QTY == 200, ]$LYLTY_CARD_NBR)

# 226000 Loyalty Card Number
# other transactions he made

qvi_superclean[qvi_superclean$LYLTY_CARD_NBR == 226000, ]




# Remove from further analysis 

qvi_superclean2 <- qvi_superclean[qvi_superclean$LYLTY_CARD_NBR != 226000, ]

# Number of transactions by date

length(unique(qvi_superclean2$DATE))

table(qvi_superclean2$DATE)

#create a sequence of dates and join with the counts of transactions per date

seq_1 <- seq(from = as.Date("2018-7-1"), to = as.Date("2019-06-30"),
    by = "days")

date_trans <- as.data.frame(seq_1)

date_wit_trans <- as.data.frame(t(table(qvi_superclean2$DATE)))[ , c(2, 3)]

date_wit_trans$Var2 <- as.Date(date_wit_trans$Var2)

freq_table <- merge(date_trans, date_wit_trans, by.x = "seq_1", by.y = "Var2", all.x = TRUE)

freq_table

which(is.na(freq_table$Freq))# 178

freq_table2 <- freq_table[-178, ]
freq_table2[178, ] <- freq_table[178, ]


freq_table2$Freq <- as.numeric(freq_table2$Freq)
freq_table2$seq_1 <- as.Date(freq_table2$seq_1)


#Graph counts
#### Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#### Plot transactions over time
ggplot(freq_table2, aes(x = seq_1, y = Freq)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

##Zoom into december

ggplot(freq_table2, aes(x = seq_1, y = Freq)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(limits = c(as.Date("2018-12-01"), as.Date("2018-12-31")))

#Now that we are satisfied that the data no longer has outliers, we can move on to
#creating other features such as brand of chips or pack size from PROD_NAME. We will
#start with pack size.


#### Pack size
#### We can work this out by taking the digits that are in PROD_NAME

qvi_superclean2$PACK_SIZE <- parse_number(qvi_superclean2$PROD_NAME)

#See the sizes in order

qvi_superclean2[order(qvi_superclean2$PACK_SIZE), "PACK_SIZE"]

summary(qvi_superclean2[order(qvi_superclean2$PACK_SIZE), "PACK_SIZE"])

#### Let's plot a histogram of PACK_SIZE since we know that it is a categorical
#variable and not a continuous variable even though it is numeric.


hist(qvi_superclean2$PACK_SIZE)

# FIRST WORD OF EACH Product. 
first_word <- strsplit(qvi_superclean2$PROD_NAME, " ")

brand <- list()

#for( i in 1:length(first_word)){
# brand <- c(brand, first_word[[i]][1])
# }

#OR

first_words <- sapply(first_word, function(x) strsplit(x, " ")[[1]][1])

qvi_superclean2$brands <- first_words

# make sure the same products are referred to by the same string

qvi_superclean3 <- qvi_superclean2


qvi_superclean3$brands <- gsub("RRD", "Red", qvi_superclean3$brands )
qvi_superclean3$brands <-gsub("GrnWves", "Grain", qvi_superclean3$brands )
qvi_superclean3$brands <-gsub("Snbts", "Sunbites", qvi_superclean3$brands )
qvi_superclean3$brands <-gsub("Infzns", "Infuzions", qvi_superclean3$brands )
qvi_superclean3$brands <-gsub("Smiths", "Smith", qvi_superclean3$brands )
qvi_superclean3$brands <-gsub("WW", "Woolworths", qvi_superclean3$brands )

qvi_superclean3$brands

#===================#

# Z score approach

# ==================#
# use z scores to get rid of outliers for PROD_QTY

#z_score = (qvi_superclean$PROD_QTY - mean(qvi_superclean$PROD_QTY)) / sd(qvi_superclean$PROD_QTY)
#outliers_idx_pq <- which(!(-3 < z_score & z_score < 3))

# use z scores to get rid of outliers for TOT_SALES

#z_score2 <- (qvi_superclean$TOT_SALES - mean(qvi_superclean$TOT_SALES))/ sd(qvi_superclean$TOT_SALES)
#outliers_idx_sales <- which(!(-3 < z_score2 & z_score2 < 3))


#clean_qvi_trans <- qvi_superclean[-outliers_idx_sales, ]
#clean_qvi_trans <- clean_qvi_trans[-outliers_idx_pq, ]
#clean_qvi_trans <- clean_qvi_trans

#hist(clean_qvi_trans$TOT_SALES)
#hist(clean_qvi_trans$PROD_QTY)


#==========================#
# For purchase data set 
#==========================#

qvi_purchase

#1)

# check for data types

trans_col # names of columns 

str(qvi_purchase)

dp_table<-data.frame(Names=names(qvi_purchase), Type=sapply(qvi_purchase,class))
labels(dp_table)

#2)
# check for null or na values

sum(is.na(qvi_purchase))
sum(is.null(qvi_purchase))

#3)

#Check product names are correctly entered. 


unique(qvi_purchase$LIFESTAGE)
unique(qvi_purchase$PREMIUM_CUSTOMER)

barplot(table(qvi_purchase$LIFESTAGE), main = "Life Stage")
barplot(table(qvi_purchase$PREMIUM_CUSTOMER), main = "Customer Status")

# merge dataframes 

qvi_data <- merge(qvi_superclean3, qvi_purchase, by = 'LYLTY_CARD_NBR')
dim(qvi_data)

# I think these two (above) and below are the same left joins. 

qvi_data2 <- merge(qvi_superclean3, qvi_purchase, all.x = TRUE)

#Let's also check if some customers were not matched on by checking for nulls.

is.null(qvi_data2$LYLTY_CARD_NBR) # No Nulls. 

fwrite(qvi_data2, "qvi_data2.csv", sep = ",", row.names = FALSE)

# ToTAL SaLES BY BRAND 

agg_brand_sales <- aggregate(qvi_data2$TOT_SALES, by = list(qvi_data2$brands), sum)

ggplot(agg_brand_sales, aes(x = reorder(Group.1, x), y = x, fill = Group.1)) +
  geom_bar(position = 'dodge', stat = 'identity', show.legend = FALSE) +
  labs(x = "Total Sales", y= "Chips Brand", title = "Total Sales by Brand")+
  scale_y_continuous(labels = comma)+
  coord_flip()



# DATA ANALYSIS

## Data analysis on customer segments
#Now that the data is ready for analysis, we can define some metrics of interest to
#the client:
#  - Who spends the most on chips (total sales), describing customers by lifestage and
#how premium their general purchasing behaviour is

sales_by_life <- aggregate(qvi_data2$TOT_SALES, 
                           by = list(qvi_data2$LIFESTAGE), sum)

sales_by_customer <-aggregate(qvi_data$TOT_SALES, 
                              by = list(qvi_data2$PREMIUM_CUSTOMER), sum)



barplot(sales_by_life$x, beside = TRUE, names.arg=sales_by_life$Group.1,
        main = "Total Sales by Life Stage")

barplot(sales_by_customer$x, beside = TRUE, names.arg=sales_by_customer$Group.1,
        main = "Total Sales by General Purchasing Behaviour")



#- How many customers are in each segment
customer_behaviour <- aggregate(qvi_data2$LYLTY_CARD_NBR~qvi_data2$PREMIUM_CUSTOMER,
                                qvi_data2, FUN = function(x) length(unique(x)))


barplot(customer_behaviour[ ,2], beside = TRUE, names.arg=sales_by_customer$Group.1,
        main = "Total customers by general behaviour")


customer_lifestage <- aggregate(qvi_data2$LYLTY_CARD_NBR~qvi_data2$LIFESTAGE,
                                qvi_data2, FUN = function(x) length(unique(x)))

barplot(customer_lifestage[ ,2], beside = TRUE, names.arg=sales_by_life$Group.1,
        main = "Total customers by Life Stage")

rotate_x <- function(data, column_to_plot, labels_vec, rot_angle, title) {
  plt <- barplot(data[[column_to_plot]], col='dodgerblue3', xaxt="n", density = 25,
                 angle = 60, main = title)
  text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), xpd = TRUE, cex=0.6) 
}

rotate_x(sales_by_life, 'x', sales_by_life$Group.1, 45, "Total Sales by Life Stage")


#- Total chip sales per customer by segment 

# REMEMBER UNIQUE CUSTOMERS SO AGGREGATE BY LYLTY CARD NUMBER

chips_per_customer <- aggregate(qvi_data2$LYLTY_CARD_NBR~qvi_data2$PREMIUM_CUSTOMER,
                                qvi_data2, FUN = function(x) length(unique(x)))

chips_customer_sales <- data.frame(group = c(sales_by_customer$Group.1),
                                   total_sales = c(sales_by_customer$x),
                                   total_customers = c(chips_per_customer[,2]))

chips_pcps <- chips_customer_sales[,2]/chips_customer_sales[,3]

barplot(chips_pcps, beside = TRUE, names.arg = chips_customer_sales$group,
        main = "Chip Sales per customer by customer general behaviour")


# REMEMBER UNIQUE CUSTOMERS SO AGGREGATE BY LYLTY CARD NUMBER

chips_per_life <- aggregate(qvi_data$LYLTY_CARD_NBR~qvi_data2$LIFESTAGE,
                            qvi_data2, FUN = function(x) length(unique(x)))

chips_customer_life<- data.frame(group = c(sales_by_life$Group.1),
                                   total_sales = c(sales_by_life$x),
                                   total_customers = c(chips_per_life[,2]))

total_chip_sales_per_lifestage <- chips_customer_life[,2]/chips_customer_life[,3]

barplot(total_chip_sales_per_lifestage, beside = TRUE, names.arg = chips_customer_life[,1],
        main = "Chip sales per customer by lifestyle segment")

#- How many chips are bought per customer by segment

qty_by_customertype <- aggregate(qvi_data2$PROD_QTY, by = list(qvi_data2$PREMIUM_CUSTOMER),
                                 sum)

qty_by_customertype2 <- qty_by_customertype

qty_by_customertype2[, 3] <- customer_behaviour[,2]

qty_by_customertype2[,2]/qty_by_customertype2[,3] #chips bought by customer by segment (general behaviour)


qty_by_lifestage <- aggregate(qvi_data2$PROD_QTY, by=list(qvi_data2$LIFESTAGE),
                              sum)

qty_by_lifestage2 <- qty_by_lifestage

qty_by_lifestage2[,3] <- chips_per_life[,2]

qty_by_lifestage2[,2] / qty_by_lifestage2[,3] # chips bought per customer by life stage


#- What's the average chip price by customer segment

# Find total sales by LIFESTAGE and CUSTOMER BEHAVIOUR, then divide each by PROD_QTY for each category

total_product_qty_life <- aggregate(qvi_data2$PROD_QTY, by = list(qvi_data2$LIFESTAGE), sum) 

avgprice_qty_lifestage <- sales_by_life[,2]/total_product_qty_life[,2]

avgprice_qty_lifestage

barplot(avgprice_qty_lifestage, beside = TRUE,
        names.arg = total_product_qty_life$Group.1, main = "Average Chip Price by Life Stage")

total_product_qty_customer_behaviour <- aggregate(qvi_data2$PROD_QTY, by = list(qvi_data2$PREMIUM_CUSTOMER), sum) 

avgprice_qty_customer_behaviour <- sales_by_customer[,2]/total_product_qty_customer_behaviour[,2]

avgprice_qty_customer_behaviour

barplot(avgprice_qty_customer_behaviour, beside = TRUE, 
        names.arg = total_product_qty_customer_behaviour$Group.1, main = "Average Chip Price by Customer Behaviour")

#### Total sales by LIFESTAGE and PREMIUM_CUSTOMER


tot_sales_premium_life <- aggregate(TOT_SALES~LIFESTAGE+PREMIUM_CUSTOMER, qvi_data2, sum)

ggplot(tot_sales_premium_life, aes(x = PREMIUM_CUSTOMER, y = TOT_SALES,fill = LIFESTAGE)) +
  geom_bar(position = 'dodge', stat = 'identity')

#Number of Customers by LIFESTAGE and PREMIUM_CUSTOMER


filtered_lylty_card <- qvi_data2[unique(qvi_data2$LYLTY_CARD_NBR), ]

number_customers_premium_life <- aggregate(LYLTY_CARD_NBR~LIFESTAGE+PREMIUM_CUSTOMER,
                                       filtered_lylty_card, length)

ggplot(number_customers_premium_life, aes(x =PREMIUM_CUSTOMER, y = LYLTY_CARD_NBR,fill = LIFESTAGE)) +
  geom_bar(position = 'dodge', stat = 'identity')

# Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER

units_per_category <- aggregate(PROD_QTY~LIFESTAGE+PREMIUM_CUSTOMER,
                                qvi_data2, sum)

units_per_customer_category <- units_per_category

units_per_customer_category[, 4] <- units_per_category[,3]/number_customers_premium_life[,3]

ggplot(units_per_customer_category, aes(x =PREMIUM_CUSTOMER, y = V4,fill = LIFESTAGE)) +
  geom_bar(position = 'dodge', stat = 'identity')

#Average price per unit in LIFESTAGE and PREMIUM_CUSTOMER

average_price_per_unit <- units_per_category 

average_price_per_unit[, 4] <- tot_sales_premium_life[ , 3]

average_price_per_unit$total_sales <- average_price_per_unit[ ,4] 

average_price_per_unit <- average_price_per_unit[, -4]

average_price_per_unit_LIFE_PREMIUM <- average_price_per_unit

average_price_per_unit_LIFE_PREMIUM[ , 5] <- average_price_per_unit[,4]/
  average_price_per_unit[3]

names(average_price_per_unit_LIFE_PREMIUM)[5] <- c("Average_Price")

ggplot(average_price_per_unit, aes(x =PREMIUM_CUSTOMER, y =total_sales/PROD_QTY,fill = LIFESTAGE)) +
  geom_bar(position = 'dodge', stat = 'identity')

#Perform an independent t-test between mainstream vs premium and budget mid age and
# young singles and couples

#T test is between two groups mainstream midage and young singles/couples vs
# premimum and mainstream midage and young singles/couples


# null hypothesis, there is not a statistically significant difference between 
# the two groups.

df_main <- qvi_data2 %>% filter(PREMIUM_CUSTOMER == "Mainstream", 
                                LIFESTAGE %in% c("MIDAGE SINGLES/COUPLES",
                                                 "YOUNG SINGLES/COUPLES"))

df_other <- qvi_data2 %>% filter(PREMIUM_CUSTOMER %in% c("Budget", "Premium"),
                                 LIFESTAGE %in% c("MIDAGE SINGLES/COUPLES",
                                                  "YOUNG SINGLES/COUPLES"))
main_avgsales <- df_main$TOT_SALES/df_main$PROD_QTY

other_avgsales <- df_other$TOT_SALES/df_other$PROD_QTY

tTest <- t.test(x = main_avgsales, y = other_avgsales, alternative = "two.sided", var.equal = TRUE)


# Statistically significant result

#The t-test results in a p-value of <2.2e-16, i.e. the unit price for mainstream,
#young and mid-age singles and couples ARE significantly higher than
#that of budget or premium, young and mid age singles and couples.

#### Deep dive into Mainstream, young singles/couples
# Over to you! Work out of there are brands that these two customer segments prefer
#more than others. You could use a technique called affinity analysis or a-priori
#analysis (or any other method if you prefer)



