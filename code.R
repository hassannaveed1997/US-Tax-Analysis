

## R Scriot
#I prefer Markdown to the script, but here is the same file in .R if anyone wants to run it.
#Remember to download the "tl_2015_us_county" file as it is essential to the running of this code

#countytax contains data about the tax obtained from :
#  https://www.irs.gov/statistics/soi-tax-stats-county-data-2015

#```{r}
library(readr)
countytax <- read_csv("https://www.irs.gov/pub/irs-soi/15incyallnoagi.csv")
#```

#now we read in the spatial data. This was obtained from https://www.census.gov/geo/maps-data/data/tiger-line.html

#although more recent data is available, to conform to out tax information data we use the one from 2015.

#```{r}
library(rgdal)
library(sf) #to add to data column

#also going to try read in as multipolygon using sf
shpdata <- st_read(dsn = "tl_2015_us_county", layer = "tl_2015_us_county", stringsAsFactors = FALSE)

#```
#```{r}
library(dplyr)
statenames <- read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")
#merge(countytax, statenames, by.x="STATE", by.y = "Abbreviation")
countytax <- merge(countytax, statenames, by.x="STATE", by.y = "Abbreviation")
countytax3 <- select(countytax, State, STATE, STATEFIPS, COUNTYFIPS, COUNTYNAME, N1 , N19300, A19300, N07220,A07220 , N18425, A18425 , N18500, A18500, N59660, A59660 , N01000, A01000, N00300, A00300, N00650, A00650 , N04470, A04470, N10600, A10600)

#```
#A19300 = Mortgage Interest paid
#A18425 = state and local income tax
#A07220 = child tax credit
#A18500 = Real Estate tax
#A59660 = Earned Income credit 

#A01000 = Net capital gain (less loss)
#A00300 = Taxable interest amount
#A00650 = Qualified Dividends amount
#A04470 = Total itemized deductions
#A10600 = Total Tax payments amount

#Now we move on with plotting. Its possible to plot the maps with ggplot. An example that would plot states by their color is:
  
#  ggplot(shpdata) + geom_sf(aes(fill = STATEFP),size=0.1, color = "grey40")

#To start merging data, we I'll create a unique id by combining the stateid and countyid. The only data we need from shp is the one about the geometry.

#```{r}
#first give the unique id to the tax data.
countytax3$uniqueID <- paste(countytax3$STATEFIPS,countytax3$COUNTYFIPS)

#then give the unique id to the location data.
shpdata$uniqueID <- paste(shpdata$STATEFP, shpdata$COUNTYFP)

#now we create a temporary version of shpdata with only the geometry and unique id
tmp <- select(shpdata, uniqueID, geometry)

#now we complete the merge
countytax4 <- merge(tmp, countytax3)

#delete all the old data files
remove(countytax)
remove(countytax3)
remove(shpdata)
remove(statenames)
remove(tmp)

#also need to rename the columns so they make more sense.
#A19300 = Mortgage Interest paid
#A18425 = state and local income tax
#A07220 = child tax credit
#A18500 = Real Estate tax
#A59660 = Earned Income credit 
names(countytax4) = c("UniqueID", "State", "StateAbb", "StateFIPS", "CountyFIPS", "CountyName","NumReturns","NumMortgageI","MortgageI","NumChildTC","ChildTC", "NumStateIT","StateIT","NumRealET","RealET","NumEITC","EITC", "NumNetCapitalGain" , "NetCapitalGain","NumTaxableInterest" ,"TaxableInterest" ,"NumQDividends" ,"QDividends" ,"NumItemizedDeductions" ,"ItemizedDeductions" ,"NumTaxPayments" ,"TaxPayments" , "geometry")

#also create new columns for averages. This is done by amount divided by number of filings. Also, all the values were in thousands so multiply it by 1000 to make it easier to understand.
countytax4$AvgMortgageI <- (countytax4$MortgageI)/(countytax4$NumMortgageI)*1000
countytax4$AvgChildTC <- (countytax4$ChildTC)/(countytax4$NumChildTC)*1000
countytax4$AvgStateIT <- (countytax4$StateIT)/(countytax4$NumStateIT)*1000
countytax4$AvgRealET <- (countytax4$RealET)/(countytax4$NumRealET)*1000
countytax4$AvgEITC <- (countytax4$EITC)/(countytax4$NumEITC)*1000

countytax4$AvgNetCapitalGain <- (countytax4$NetCapitalGain)/(countytax4$NumNetCapitalGain)*1000
countytax4$AvgTaxableInterest <- (countytax4$TaxableInterest)/(countytax4$NumTaxableInterest)*1000
countytax4$AvgQDividends <- (countytax4$QDividends)/(countytax4$NumQDividends)*1000
countytax4$AvgItemizedDeductions <- (countytax4$ItemizedDeductions)/(countytax4$NumItemizedDeductions)*1000
countytax4$AvgTaxPayments <- (countytax4$TaxPayments)/(countytax4$NumTaxPayments)*1000

#also removing hawaii and alaska so theres only mainland US.
countytax <- countytax4[countytax4$State!="Hawaii", ]
countytax <- countytax[countytax$State!="Alaska", ]

remove(countytax4)
#```

#Now we have a data set called countytax4 which has all the information we currently need. Time to work on the plots.

#```{r}
#import necessary libraries
library(ggplot2) #to make the plot
library(RColorBrewer) #color selection

#creating a copy so the original data set is not changed
tmpdata <- countytax
#```


#plotting using the same method as Sirui did earlier (NOTE: This takes some time as:

#```{r}
#for mortgage data

tmpdata$AvgMortgageI[tmpdata$AvgMortgageI > 10000] <- 10001

ggplot(tmpdata) + geom_sf(aes(fill = tmpdata$AvgMortgageI),size=0.05, color = "grey40")+  borders(database = "state", colour ="grey80", size = 0.2) +  scale_fill_gradientn(name="Avg Morgage Interest", colors =brewer.pal(n = 9, name = "Blues"), breaks = c(4000,6000,8000,10000), labels = c("4000","6000","8000","10000+"))+theme_void()
#```



#```{r}
#for child care tax credit
tmpdata$AvgChildTC[tmpdata$AvgChildTC > 1600] <- 1601

ggplot(tmpdata) + geom_sf(aes(fill = tmpdata$AvgChildTC),size=0.05, color = "grey40")+  borders(database = "state", colour ="grey80", size = 0.2) +  scale_fill_gradientn(name="Avg Child Tax Credit", colors =brewer.pal(n = 9, name = "YlOrRd"), breaks = c(700,1000,1300,1600), labels = c("700","1000","1300","1600+"))+theme_void()

#```

#```{r}
#For state and local income tax

tmpdata$AvgStateIT[tmpdata$AvgStateIT > 15000] <- 15001

ggplot(tmpdata) + geom_sf(aes(fill = tmpdata$AvgStateIT),size=0.05, color = "grey40")+  borders(database = "state", colour ="grey80", size = 0.2) +  scale_fill_gradientn(name="Avg State Income Tax", colors =brewer.pal(n = 11, name = "YlGn"), breaks = c(3000, 7000, 10000, 15000), labels = c("3000","7000","10000","15000+"))+theme_void()

#```

#```{r}
#For Real Estate Tax
tmpdata$AvgRealET[tmpdata$AvgRealET > 6000] <- 6001

ggplot(tmpdata) + geom_sf(aes(fill = tmpdata$AvgRealET),size=0.05, color = "grey40")+  borders(database = "state", colour ="grey80", size = 0.2) +  scale_fill_gradientn(name="Avg Real Estate Tax", colors =brewer.pal(n = 9, name = "Greens"), breaks = c(0, 2000, 4000, 6000), labels = c("0","2000","4000","6000+"))+theme_void()

#```

#```{r}
#For Earnings Tax Credit
ggplot(tmpdata) + geom_sf(aes(fill = tmpdata$AvgEITC),size=0.05, color = "grey40")+  borders(database = "state", colour ="grey80", size = 0.2) +  scale_fill_gradientn(name="Avg EITC", colors =brewer.pal(n = 9, name = "BuPu"), breaks = c(1000, 2000, 3000), labels = c("1000","2000","3000"))+theme_void()



#```


#```{r}
#For Avg Capital Gains
tmpdata$AvgNetCapitalGain[tmpdata$AvgNetCapitalGain > 16000] <- 16001

ggplot(tmpdata) + geom_sf(aes(fill = tmpdata$AvgNetCapitalGain),size=0.05, color = "grey40")+  borders(database = "state", colour ="grey80", size = 0.2) +  scale_fill_gradientn(name="Avg Net Capital Gain (less loss)", colors =brewer.pal(n = 9, name = "BuPu"), breaks = c(0, 5000, 10000, 15000), labels = c("0","5000","10000","15000+"))+theme_void()

#```



#```{r}
#For Avg Taxable interest amount
tmpdata$AvgTaxableInterest[tmpdata$AvgTaxableInterest > 3000] <- 3001

ggplot(tmpdata) + geom_sf(aes(fill = tmpdata$AvgTaxableInterest),size=0.05, color = "grey40")+  borders(database = "state", colour ="grey80", size = 0.2) +  scale_fill_gradientn(name="Avg Taxable Interest Amount", colors =brewer.pal(n = 9, name = "PuBu"), breaks = c(0, 1000, 2000, 3000), labels = c("0","1000","2000","3000+"))+theme_void()

#```

#```{r}
#For Avg Qualified Dividends Amount
tmpdata$AvgQDividends[tmpdata$AvgQDividends > 15000] <- 15000

ggplot(tmpdata) + geom_sf(aes(fill = tmpdata$AvgQDividends),size=0.05, color = "grey40")+  borders(database = "state", colour ="grey80", size = 0.2) +  scale_fill_gradientn(name="Avg Qualified Dividends", colors =brewer.pal(n = 9, name = "PuBu"), breaks = c(0, 5000, 10000, 15000), labels = c("0","5000","10000","15000+"))+theme_void()

#```


#```{r}
#For Avg Itemzied Deductions
tmpdata$AvgItemizedDeductions[tmpdata$AvgItemizedDeductions > 40000] <- 40001

ggplot(tmpdata) + geom_sf(aes(fill = tmpdata$AvgItemizedDeductions),size=0.05, color = "grey40")+  borders(database = "state", colour ="grey80", size = 0.2) +  scale_fill_gradientn(name="Avg Itemized Deductions", colors =brewer.pal(n = 9, name = "YlOrBr"), breaks = c(0, 10000, 20000, 30000, 40000), labels = c("0","10000","20000","30000", "40000+"))+theme_void()

#```


#For Average Total Tax Payments
tmpdata$AvgTaxPayments[tmpdata$AvgTaxPayments > 25000] <- 25001

ggplot(tmpdata) + geom_sf(aes(fill = tmpdata$AvgTaxPayments),size=0.05, color = "grey40")+  borders(database = "state", colour ="grey80", size = 0.2) +  scale_fill_gradientn(name="Avg Total Tax Payments", colors =brewer.pal(n = 9, name = "Greens"), breaks = c(0, 5000, 10000, 15000, 20000, 25000), labels = c("0","5000","10000","15000", "20000","25000+"))+theme_void()


