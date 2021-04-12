
setwd("D:\\User\\copy\\Price Elasticity")

# Load data and output summary stats
sales.data<-read.csv('supermarket.csv')
#Look at column names and their classes
sapply(sales.data,class)

# Change Ad Type to factor
sales.data$Ad.Type<-as.factor(sales.data$Ad.Type)
summary(sales.data)


# Create models
# Load required library
library(memisc) 

m1<-lm(formula=Sales~Price.Eggs,data=sales.data)
m2<-update(m1,.~.+Ad.Type)
m3<-update(m2,.~.+Price.Cookies)
mtable(m1,m2,m3)

#########################
#   Price Elasticity    #
#########################

# Calculate Price Elasticity
PE<-as.numeric(m3$coefficients["Price.Eggs"] * mean(sales.data$Price.Eggs)/mean(sales.data$Sales))
CPEcookies<-as.numeric(m3$coefficients["Price.Cookies"] * mean(sales.data$Price.Cookies)/mean(sales.data$Sales))

# Print Results 
PE
CPEcookies

####################################
#   BONUS - What about the ads?    #
####################################

# Subset the data
sales.adEggs <- subset(sales.data,Ad.Type==0)
sales.adCookies <- subset(sales.data,Ad.Type==1)

# Diagnostic on subsets' means and if they are different ... they are. 
wilcox.test(x=sales.adCookies$Sales,y=sales.adEggs$Sales,exact=F,paired=T)

# On average, does the advert for eggs generate higher sales for eggs?
mean(sales.adEggs$Sales) >= mean(sales.adCookies$Sales)


##############################################################################
#Find optimum price

data <- read.csv("Vendor_Data.csv")
dim(data)
#250*7
summary(data)


#This function tries to get the next best increment vector
incremental_new <- function(initial_increments){
  initial_rev <- find_rev(initial_increments)
  intermediate_rev <- 0
  for(i in 1:250){
    
    increments <- initial_increments
    if(increments[i] > -0.099) {increments[i] <- increments[i] - 0.01}
    
    rev <- find_rev(increments)
    if (rev > initial_rev) {
      final_increments <- increments;
      intermediate_rev <- rev
    }
    
    if(increments[i] < 0.19) {increments[i] <- initial_increments[i] + 0.01}
    
    rev <- find_rev(increments)
    if (rev > max(initial_rev,intermediate_rev)) {final_increments <- increments}
  }
  return(final_increments)
}

# This function will get us the overall revenue for the given increment vector
find_rev <- function(increment){
  price <- data$Avg_Price_per_unit*(1+increment)
  volumes <- data$Average_units_sold*(1-(data$Increase_sale_volume*increment*10))
  multiplier <- (1-(data$Incremental_acquisition*increment*10))
  total_multiplier <- prod(multiplier)
  profit_wo_multiplier <- 0.05*(sum(price*volumes) - sum(volumes*data$Cost_per_unit))
  profit_w_multiplier <- profit_wo_multiplier*total_multiplier
  net_profit <- sum(profit_w_multiplier)
  return(net_profit)}


# This is the initial value of the increment vector - all zeros
increment <- array(0,250)
flag = 0
increment_i <- increment

#flag = 1 is a condition when the increment vector remains the same
while (flag == 0) {
  #print(find_rev(increment_i))
  increment_iplus1 <- incremental_new(increment_i)
  if (min(increment_iplus1 == increment_i) == 1) {flag = 1}
  increment_i <- increment_iplus1
}

increment_i
find_rev(increment_i)
#find_rev(increment)
price <- increment_i
write.csv(price,"price.csv")


setwd("D:/Price Elasticity")
data_clean <- read.csv("optimum_pricePOS.csv")

#including all other predictors
data = data_clean[,c('Isweekday','Isweekend','ProductSalePrice',
                     'SupPurchasePrice','OrderItemQuantity',"Profit")]

library(rpart)
library(rpart.plot)
fit <- rpart(Profit ~., data = data)
summary(fit)


rpart.plot(fit,extra = 101)
