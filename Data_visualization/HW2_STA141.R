#name: Hao Luo     ID: 912423597
Hdata <- read.csv(file = "/Users/haoluo/Desktop/STA/STA141A/hw2/housing.csv")

library(stringr)
library(tidyr)
library(lubridate)
library(dplyr)
library(maps)
library(ggplot2)

# qn1 (time)
names(Hdata)
head(sort(Hdata$year, decreasing = TRUE) , n = 30)
tail(sort(Hdata$year, decreasing = TRUE) , n = 30)
Hdata$year <- str_replace(Hdata$year, "20005", "2005") # typo fixed.
Hdata$year <- str_replace(Hdata$year, "3885", "1885") # typo fixed.
typo_ind <- which(as.numeric(Hdata$year) < 100)
Hdata$year[typo_ind] <- "NA" #removed unknowns
year <- Hdata$year
Hdata$county <- str_replace( Hdata$county, pattern = "county", replacement = "County") %>% factor()
Hdata$county <- str_replace( Hdata$county, pattern = "Franciscoe", replacement = "Francisco") %>% factor()
Hdata$br <- cut(Hdata$br,
                c(0,1,2,3,Inf),
                c("1 bed", "2 bed", "3 bed", "4+ bed")
                  )   # got helped by Jody Zhou
Hdata$price[Hdata$price == 0] = NA



date <- as.Date(Hdata$date)

# qn2
timeSpanSalesCover <- max(date) - min(date)
timeSpanSalesCover

#by looking into the data, only the highest number year 2005 and lowerst number 1885
#make logical sense.

timeSpanConstruction <- max(as.numeric(year), na.rm = TRUE) - min(as.numeric(year), na.rm = TRUE)  
timeSpanConstruction#timespan for construction is 120 years

# qn 3. Examine the monthly housing sales for this dataset. You will need to look at combinations of both year
#and month using the date variable. Make two plots:
#                              A plot that shows the number of sales over time.
#                              A plot that shows the average house price over time.

date_sold <- ymd(Hdata$date)


price_sold <- Hdata$price

date_df <- data.frame(date = date_sold,
                      The_year_sold <- as.numeric(format(date_sold, format = "%Y")),
                      the_month_sold <- as.numeric(format(date_sold, format = "%m")),
                      The_day_sold <- as.numeric(format(date_sold, format = "%d" ))) 

#source: https://stackoverflow.com/questions/4078502/split-date-data-m-d-y-into-3-separate-columns

sold_in_2003_ind <- which(date_df$The_year_sold....as.numeric.format.date_sold..format.....Y... == 2003)
sold_in_2003 <- date[sold_in_2003_ind]
price_sold_2003 <- price_sold[sold_in_2003_ind]#FIXME EACH MON

sold_in_2004_ind <- which(date_df$The_year_sold....as.numeric.format.date_sold..format.....Y... == 2004)
sold_in_2004 <- date[sold_in_2004_ind]
price_sold_2004 <- price_sold[sold_in_2004_ind]#FIXME EACH MON

sold_in_2005_ind <- which(date_df$The_year_sold....as.numeric.format.date_sold..format.....Y... == 2005)
sold_in_2005 <- date[sold_in_2005_ind]
price_sold_2005 <- price_sold[sold_in_2005_ind]#FIXME EACH MON

sold_in_2006_ind <- which(date_df$The_year_sold....as.numeric.format.date_sold..format.....Y... == 2006)
sold_in_2006 <- date[sold_in_2006_ind]
price_sold_2006 <- price_sold[sold_in_2006_ind]#FIXME EACH MON
#
#########Number of sales over time
par(mfrow = c(2,2))
hist(sold_in_2003 , breaks = "month")
hist(sold_in_2004 , breaks = "month")
hist(sold_in_2005 , breaks = "month")
hist(sold_in_2006 , breaks = "month")

df1 = data.frame(sold_in_2003, price_sold_2003)
df2 = data.frame(sold_in_2004, price_sold_2004)
df3 = data.frame(sold_in_2005, price_sold_2005)
df4 = data.frame(sold_in_2006, price_sold_2006)
########### Average over time

ggplot(df1 , aes(df1$sold_in_2003, df1$price_sold_2003)) + geom_bar(stat = "identity")
ggplot(df2 , aes(df2$sold_in_2004, df2$price_sold_2004)) + geom_bar(stat = "identity")
ggplot(df3 , aes(df3$sold_in_2005, df3$price_sold_2005)) + geom_bar(stat = "identity")
ggplot(df4 , aes(df4$sold_in_2006, df4$price_sold_2006)) + geom_bar(stat = "identity")


# qn 4. Make a line plot that shows the relationship between county, bedrooms,
#and sale year. For the bedrooms variable, define the levels to be 1,2,3,4+ bedrooms. 
#For the sale year variable, use the levels 2003,2004,2005. Use all levels of the county variable. 
#Each line should have three points corresponding to year. 
#You will need separate lines for each combination of county and br.

year_level <- date[c(sold_in_2003_ind , sold_in_2004_ind , sold_in_2005_ind)]

qn4_df <- data.frame( theYear = Hdata$date,
                      bedroom = Hdata$br,
                      county = Hdata$county,
                      price = Hdata$price)

below_06 = subset(data.frame(Hdata[, c("county", "price", "br")], year = year(Hdata$date)), year(Hdata$date) < 2006)
below_06_county = split(below_06, below_06$county)

below_06_county_br = lapply(below_06_county, function(x) split(x, x$br))

below_06_county_br_year = lapply(below_06_county_br, function(x) lapply(x, function(y) tapply(y$price, y$year, mean)))
list_name = names(unlist(below_06_county_br_year))
below_06_df = data.frame(avg_price = as.numeric(unlist(below_06_county_br_year)), do.call("rbind", strsplit(list_name, "\\.")))
names(below_06_df) = c("avg_price", "county", "bedrooms", "year")

below_06_df$bedrooms = factor(below_06_df$bedrooms)

ggplot(below_06_df, aes(x = year, y = avg_price, col = bedrooms)) + geom_line(aes(group = bedrooms)) + facet_grid(. ~ county)

# qn 5

city_county = sapply(split(Hdata$county, Hdata$city), function(x) length(unique(as.character(x))))

which(city_county > 1)

# For city: "San Francisco"
as.character(unique(subset(Hdata$county, Hdata$city == "San Francisco")))

# For city: "Vallejo"
as.character(unique(subset(Hdata$county, Hdata$city == "Vallejo")))

# qn 6

m1 = lm(price~bsqft, data = Hdata)
par(mfrow=c(1,2))
plot(m1, which = c(1,2)) # suggest transformation

library(MASS)
par(mfrow=c(1,1))
boxcox(m1) # log transformation is suggested

lm_df = data.frame(x = Hdata$bsqft, y = log(Hdata$price))
lm_df = lm_df[complete.cases(lm_df),]

m2 = lm(y~x, data = lm_df)
par(mfrow=c(1,2))
plot(m2, which = c(1,2))

# remove outliers
lm_df_clean = lm_df[-1*as.numeric(which(rstandard(m2) < -5)),]

m3 = lm(y~x, data = lm_df_clean)
par(mfrow=c(1,2))
plot(m3, which = c(1,2)) # final model 

# qn 7

m4 = lm(price ~ lsqft + bsqft, data = Hdata)
summary(m4)

beta_b = summary(m4)$coef[3,1]
beta_l = summary(m4)$coef[2,1]
se_b = summary(m4)$coef[3,2]
se_l = summary(m4)$coef[2,2]

ts = (beta_b-beta_l)/sqrt(se_b^2+se_l^2)
n = length(m4$residuals) # 15602
pt(ts, n-3) # p-value close to 1
# fail to reject H0 at 5% significance level because p-value > 0.05

# qn 8

q8_df = split(Hdata[,c("price", "bsqft")], Hdata$county)
lm_coef = do.call("rbind", lapply(q8_df, function(x) summary(lm(x$price~x$bsqft))$coef[,1]))

par(mfrow=c(1,1))
plot(Hdata$bsqft, Hdata$price, xlab = "building size of the house", ylab = "sale price", main = "Sale price vs building size \n with fitted regression lines by county")
sapply(1:nrow(lm_coef), function(x) abline(a = lm_coef[x,1], b = lm_coef[x, 2], col=x))
legend("bottomright", rownames(lm_coef), col=1:nrow(lm_coef), lty=1, cex = 0.8)

# qn 9

library(treemap)

Hdata$city = as.character(Hdata$city)
county_split = split(Hdata[,c("county", "city", "price")], Hdata$county)
high_freq_city_list = lapply(county_split, function(x) names(sort(table(x$city), decreasing=TRUE)[1:3]))
high_freq_city = as.character(unlist(high_freq_city_list))

Hdata_f = Hdata[Hdata$city %in% high_freq_city,]
county_split_f = split(Hdata_f[,c("county", "city", "price")], Hdata_f$county)
avg = lapply(county_split_f, function(x) tapply(x$price, x$city, function(y) mean(y, na.rm=TRUE)))

q9_df = data.frame(price = as.numeric(unlist(avg)), do.call("rbind", strsplit(names(unlist(avg)), "\\.")))
names(q9_df) = c("price", "county", "city")

treemap(q9_df,
        index=c("county", "city"),
        vSize="price",
        vColor="price",
        type="value",
        format.legend = list(scientific = FALSE, big.mark = " "))


# qn 10

#  source code: piazza. @252

library(maps)

SFdata<-subset(Hdata,Hdata$city=="San Francisco")
SFdata$long2<-round(as.numeric(as.character(SFdata$long)),2)
SFdata$lat2<-round(as.numeric(as.character(SFdata$lat)),2)

long_range<-range(SFdata$long2,na.rm=TRUE)
long_seq<-seq(long_range[1],long_range[2],.01)
SFdata$longF<-factor(SFdata$long2,levels=long_seq)

lat_range<-range(SFdata$lat2,na.rm = T)
lat_seq<-seq(lat_range[1],lat_range[2],.01)
SFdata$latF<-factor(SFdata$lat2,levels=lat_seq)

SF_agg<-aggregate(price~latF+longF,SFdata,function(x) c(mean(x),length(x)),drop=FALSE)
house_prices<-matrix(SF_agg$price[,1],nlevels(SFdata$longF),nlevels(SFdata$latF),byrow=TRUE)
house_counts<-matrix(SF_agg$price[,2],nlevels(SFdata$longF),nlevels(SFdata$latF),byrow=TRUE)

# heatmap for average housing price
sf_border=map('county','california,san francisco',plot = F)
image(x = as.numeric(levels(SFdata$longF)) + .03, y = as.numeric(levels(SFdata$latF)), z = house_prices, #create heatmap
      xlab = "Longitude", ylab = "Latitude", main = "San Francisco Heatmap of Average Home Prices")
lines(sf_border$x,sf_border$y)

# heatmap for # of sale records

sf_border=map('county','california,san francisco',plot = F)
image(x = as.numeric(levels(SFdata$longF)) + .03, y = as.numeric(levels(SFdata$latF)), z = house_counts, #create heatmap
      xlab = "Longitude", ylab = "Latitude", main = "San Francisco Heatmap of Number of Sale Records")
lines(sf_border$x,sf_border$y)

