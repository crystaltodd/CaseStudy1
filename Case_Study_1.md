# Case Study 1
Crystal Todd  
October 31, 2016  

## Introduction

In regards to the GDP of several countries, we would like to analyze the GDP of countries against their income groups. This analysis will help define the economic health of countries compared to other demographic data for the countries.

## Data Downloading, Tidying, and Merging
The process used to clean the data began with downloading the data provided, unsure of the format or condition of the data. First, we must load the packages required for this analysis and download the two data sets from the given URLs.

```r
# Load Needed Packages
library(repmis)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

# Set the Working Directory
setwd("C:/Users/cltod/Documents/MSDS 6306 Doing Data Science/Unit 8/")

# Data Set 1
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(URL, destfile = "./getdataGDP.csv")
rm(URL)
GDP <- read.csv("getdataGDP.csv", header = FALSE, stringsAsFactors = FALSE) # Load Data Set into R from Working Directory. Use header = FALSE because we do not know the format and condition of the data set.

# Data Set 2
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(URL, destfile = "./getdataEDSTATS.csv")
rm(URL)
EDSTATS <- read.csv("getdataEDSTATS.csv", header = FALSE, stringsAsFactors = FALSE) # Load Data Set into R from Working Directory. Use header = FALSE because we do not know the format and condition of the data set.
```

First, we cleaned the GDP data by looking at several characteristics of the data.


```r
dim(GDP)
```

```
## [1] 331  10
```

```r
str(GDP)
```

```
## 'data.frame':	331 obs. of  10 variables:
##  $ V1 : chr  "" "" "" "" ...
##  $ V2 : chr  "Gross domestic product 2012" "" "" "Ranking" ...
##  $ V3 : logi  NA NA NA NA NA NA ...
##  $ V4 : chr  "" "" "" "Economy" ...
##  $ V5 : chr  "" "" "(millions of" "US dollars)" ...
##  $ V6 : chr  "" "" "" "" ...
##  $ V7 : logi  NA NA NA NA NA NA ...
##  $ V8 : logi  NA NA NA NA NA NA ...
##  $ V9 : logi  NA NA NA NA NA NA ...
##  $ V10: logi  NA NA NA NA NA NA ...
```

```r
head(GDP)
```

```
##    V1                          V2 V3            V4           V5 V6 V7 V8
## 1     Gross domestic product 2012 NA                               NA NA
## 2                                 NA                               NA NA
## 3                                 NA               (millions of    NA NA
## 4                         Ranking NA       Economy  US dollars)    NA NA
## 5                                 NA                               NA NA
## 6 USA                           1 NA United States  16,244,600     NA NA
##   V9 V10
## 1 NA  NA
## 2 NA  NA
## 3 NA  NA
## 4 NA  NA
## 5 NA  NA
## 6 NA  NA
```
The header of the GDP data appears to have some column headers in the fifth row. We renamed the columns we could quickly identify and remove the top five rows.

```r
GDP <- rename(GDP, COUNTRY_CODE = V1, RANKING = V2, ECONOMY = V4, USD_MIL = V5)
str(GDP)
```

```
## 'data.frame':	331 obs. of  10 variables:
##  $ COUNTRY_CODE: chr  "" "" "" "" ...
##  $ RANKING     : chr  "Gross domestic product 2012" "" "" "Ranking" ...
##  $ V3          : logi  NA NA NA NA NA NA ...
##  $ ECONOMY     : chr  "" "" "" "Economy" ...
##  $ USD_MIL     : chr  "" "" "(millions of" "US dollars)" ...
##  $ V6          : chr  "" "" "" "" ...
##  $ V7          : logi  NA NA NA NA NA NA ...
##  $ V8          : logi  NA NA NA NA NA NA ...
##  $ V9          : logi  NA NA NA NA NA NA ...
##  $ V10         : logi  NA NA NA NA NA NA ...
```

```r
tail(GDP)
```

```
##     COUNTRY_CODE RANKING V3 ECONOMY USD_MIL V6 V7 V8 V9 V10
## 326                      NA                    NA NA NA  NA
## 327                      NA                    NA NA NA  NA
## 328                      NA                    NA NA NA  NA
## 329                      NA                    NA NA NA  NA
## 330                      NA                    NA NA NA  NA
## 331                      NA                    NA NA NA  NA
```

```r
GDP <- GDP[6:nrow(GDP),]
head(GDP)
```

```
##    COUNTRY_CODE RANKING V3        ECONOMY      USD_MIL V6 V7 V8 V9 V10
## 6           USA       1 NA  United States  16,244,600     NA NA NA  NA
## 7           CHN       2 NA          China   8,227,103     NA NA NA  NA
## 8           JPN       3 NA          Japan   5,959,718     NA NA NA  NA
## 9           DEU       4 NA        Germany   3,428,131     NA NA NA  NA
## 10          FRA       5 NA         France   2,612,878     NA NA NA  NA
## 11          GBR       6 NA United Kingdom   2,471,784     NA NA NA  NA
```
Next, there are six columns we cannot quickly identify the contents of the columns. We looked at these columns to find if they are completely empty or not.

```r
table(GDP$V3)
```

```
## < table of extent 0 >
```

```r
table(GDP$V6)
```

```
## 
##       a   b   c   d   e   f 
## 320   1   1   1   1   1   1
```

```r
table(GDP$V7)
```

```
## < table of extent 0 >
```

```r
table(GDP$V8)
```

```
## < table of extent 0 >
```

```r
table(GDP$V9)
```

```
## < table of extent 0 >
```

```r
table(GDP$V10)
```

```
## < table of extent 0 >
```

Only the column V6 contained any data with some kind of comment in the form of a through f; the other columns contained no data and were deleted from the data set with V6 being renamed to the column name COMMENTS.

```r
GDP <- select(GDP, COUNTRY_CODE, RANKING, ECONOMY, USD_MIL, COMMENTS = V6)
```

Previously, we looked at the tail of the data set, and it appeared that there were at least six rows at the end of the data that had no data in them.


```r
table(GDP$COUNTRY_CODE) #There are 98 blank values aside from the first five rows deleted from the beginning of analysis.
```

```
## 
##     ABW ADO AFG AGO ALB ARE ARG ARM ASM ATG AUS AUT AZE BDI BEL BEN BFA 
##  98   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## BGD BGR BHR BHS BIH BLR BLZ BMU BOL BRA BRB BRN BTN BWA CAF CAN CHE CHI 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## CHL CHN CIV CMR COG COL COM CPV CRI CUB CUW CYM CYP CZE DEU DJI DMA DNK 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## DOM DZA EAP ECA ECU EGY EMU ERI ESP EST ETH FIN FJI FRA FRO FSM GAB GBR 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## GEO GHA GIN GMB GNB GNQ GRC GRD GRL GTM GUM GUY HIC HKG HND HRV HTI HUN 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## IDN IMY IND IRL IRN IRQ ISL ISR ITA JAM JOR JPN KAZ KEN KGZ KHM KIR KNA 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## KOR KSV KWT LAC LAO LBN LBR LBY LCA LIC LIE LKA LMC LMY LSO LTU LUX LVA 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## MAC MAF MAR MCO MDA MDG MDV MEX MHL MIC MKD MLI MLT MMR MNA MNE MNG MNP 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## MOZ MRT MUS MWI MYS NAM NCL NER NGA NIC NLD NOR NPL NZL OMN PAK PAN PER 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## PHL PLW PNG POL PRI PRK PRT PRY PYF QAT ROM RUS RWA SAS SAU SDN SEN SGP 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## SLB SLE SLV SMR SOM SRB SSA SSD STP SUR SVK SVN SWE SWZ SXM SYC SYR TCA 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## TCD TGO THA TJK TKM TMP TON TTO TUN TUR TUV TZA UGA UKR UMC URY USA UZB 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## VCT VEN VIR VNM VUT WBG WLD WSM YEM ZAF ZAR ZMB ZWE 
##   1   1   1   1   1   1   1   1   1   1   1   1   1
```

```r
nulls <- GDP[GDP$COUNTRY_CODE == "",] # Splitting null from non-null Country Code values to examine.
nonull <- GDP[GDP$COUNTRY_CODE != "",]

table(nulls$RANKING)
```

```
## 
##                                                                                                                              
##                                                                                                                           94 
##                                                                                                          .. Not available.   
##                                                                                                                            1 
##     a. Includes Former Spanish Sahara.  b. Excludes South Sudan  c. Covers mainland Tanzania only. d. Data are for the area  
##                                                                                                                            1 
## controlled by the government of the Republic of Cyprus.   e. Excludes Abkhazia and South Ossetia.  f. Excludes Transnistria. 
##                                                                                                                            1 
##           Note: Rankings include only those economies with confirmed GDP estimates. Figures in italics are for 2011 or 2010. 
##                                                                                                                            1
```

```r
## DATA HIDING IN RANKING COLUMN
# .. Not available.
# a. Includes Former Spanish Sahara.
# b. Excludes South Sudan
# c. Covers mainland Tanzania only.
# d. Data are for the area controlled by the government of the Republic of Cyprus.
# e. Excludes Abkhazia and South Ossetia.
# f. Excludes Transnistria.
# Note: Rankings include only those economies with confirmed GDP estimates. Figures in italics are for 2011 or 2010.

table(nulls$ECONOMY)
```

```
## 
##    
## 98
```

```r
table(nulls$USD_MIL)
```

```
## 
##    
## 98
```

```r
table(nulls$V6)
```

```
## < table of extent 0 >
```

Since those 98 blank rows have been examined and the important information regarding a through f has been identified, we can delete those rows in the data set.


```r
# For all of the 98 blank values for COUNTRY_CODE, the other columns are blank. Deleting those 98 rows from the data set GDP.
GDP <- GDP[GDP$COUNTRY_CODE != "",]
rm(nulls, nonull)
```

From the information in the RANKING column, we know that values that have ".." in the USD_MIL column are considered not applicable. This assumption was applied to the data.


```r
for(j in 1:nrow(GDP))
{
  if(GDP$USD_MIL[j] == "..")
    GDP$USD_MIL[j] = NA
}
```

In the data, there were commas separating the thousands place in USD_MIL, and the RANKING column is considered a character field. These must be changed to numerics and any commas removed.


```r
GDP$RANKING <- as.numeric(gsub("[^[:digit:]]","", GDP$RANKING))
GDP$USD_MIL <- as.numeric(gsub("[^[:digit:]]","", GDP$USD_MIL))
```

There are values in the RANKING and USD_MIL columns that did not have data values or were NA. These values must be removed to allow for correct analysis.


```r
nrow(GDP[is.na(GDP$RANKING),])
```

```
## [1] 38
```

```r
# There are 122 NA records in the RANKING column.
nrow(GDP[is.na(GDP$USD_MIL) | GDP$USD_MIL == "",])
```

```
## [1] 24
```

```r
# There are 135 records in USD_MIL column that are either NA or do not have a value.
# Remove where there is no RANKING
GDP <- GDP[!is.na(GDP$RANKING),]
```

The GDP data is clean for the analysis portion of the project. Now, we must clean the EDSTATS data set. First, we look at the characteristics of the data set.


```r
dim(EDSTATS)
```

```
## [1] 235  31
```

```r
head(EDSTATS)
```

```
##            V1                           V2                   V3
## 1 CountryCode                    Long Name         Income Group
## 2         ABW                        Aruba High income: nonOECD
## 3         ADO      Principality of Andorra High income: nonOECD
## 4         AFG Islamic State of Afghanistan           Low income
## 5         AGO  People's Republic of Angola  Lower middle income
## 6         ALB          Republic of Albania  Upper middle income
##                          V4               V5           V6             V7
## 1                    Region Lending category Other groups  Currency Unit
## 2 Latin America & Caribbean                                Aruban florin
## 3     Europe & Central Asia                                         Euro
## 4                South Asia              IDA         HIPC Afghan afghani
## 5        Sub-Saharan Africa              IDA              Angolan kwanza
## 6     Europe & Central Asia             IBRD                Albanian lek
##                         V8                       V9
## 1 Latest population census  Latest household survey
## 2                     2000                         
## 3           Register based                         
## 4                     1979               MICS, 2003
## 5                     1970 MICS, 2001, MIS, 2006/07
## 6                     2001               MICS, 2005
##                                                                           V10
## 1                                                               Special Notes
## 2                                                                            
## 3                                                                            
## 4 Fiscal year end: March 20; reporting period for national accounts data: FY.
## 5                                                                            
## 6                                                                            
##                           V11                              V12
## 1 National accounts base year National accounts reference year
## 2                        1995                                 
## 3                                                             
## 4                   2002/2003                                 
## 5                        1997                                 
## 6                                                         1996
##                           V13                 V14
## 1 System of National Accounts SNA price valuation
## 2                                                
## 3                                                
## 4                                             VAB
## 5                                             VAP
## 6                        1993                 VAB
##                             V15             V16
## 1 Alternative conversion factor PPP survey year
## 2                                              
## 3                                              
## 4                                              
## 5                       1991-96            2005
## 6                                          2005
##                                 V17                            V18
## 1 Balance of Payments Manual in use External debt Reporting status
## 2                                                                 
## 3                                                                 
## 4                                                           Actual
## 5                              BPM5                         Actual
## 6                              BPM5                         Actual
##               V19                           V20
## 1 System of trade Government Accounting concept
## 2         Special                              
## 3         General                              
## 4         General                  Consolidated
## 5         Special                              
## 6         General                  Consolidated
##                               V21
## 1 IMF data dissemination standard
## 2                                
## 3                                
## 4                            GDDS
## 5                            GDDS
## 6                            GDDS
##                                                 V22
## 1 Source of most recent Income and expenditure data
## 2                                                  
## 3                                                  
## 4                                                  
## 5                                         IHS, 2000
## 6                                        LSMS, 2005
##                           V23                        V24
## 1 Vital registration complete Latest agricultural census
## 2                                                       
## 3                         Yes                           
## 4                                                       
## 5                                                1964-65
## 6                         Yes                       1998
##                      V25               V26                          V27
## 1 Latest industrial data Latest trade data Latest water withdrawal data
## 2                                     2008                             
## 3                                     2006                             
## 4                                     2008                         2000
## 5                                     1991                         2000
## 6                   2005              2008                         2000
##            V28       V29         V30         V31
## 1 2-alpha code WB-2 code  Table Name  Short Name
## 2           AW        AW       Aruba       Aruba
## 3           AD        AD     Andorra     Andorra
## 4           AF        AF Afghanistan Afghanistan
## 5           AO        AO      Angola      Angola
## 6           AL        AL     Albania     Albania
```

```r
EDSTATS <- read.csv("getdataEDSTATS.csv", header = TRUE, stringsAsFactors = FALSE) #Notice that the first row of the data set has apparent headers. We can reload the data with header = TRUE.
head(EDSTATS)
```

```
##   CountryCode                    Long.Name         Income.Group
## 1         ABW                        Aruba High income: nonOECD
## 2         ADO      Principality of Andorra High income: nonOECD
## 3         AFG Islamic State of Afghanistan           Low income
## 4         AGO  People's Republic of Angola  Lower middle income
## 5         ALB          Republic of Albania  Upper middle income
## 6         ARE         United Arab Emirates High income: nonOECD
##                       Region Lending.category Other.groups  Currency.Unit
## 1  Latin America & Caribbean                                Aruban florin
## 2      Europe & Central Asia                                         Euro
## 3                 South Asia              IDA         HIPC Afghan afghani
## 4         Sub-Saharan Africa              IDA              Angolan kwanza
## 5      Europe & Central Asia             IBRD                Albanian lek
## 6 Middle East & North Africa                                U.A.E. dirham
##   Latest.population.census  Latest.household.survey
## 1                     2000                         
## 2           Register based                         
## 3                     1979               MICS, 2003
## 4                     1970 MICS, 2001, MIS, 2006/07
## 5                     2001               MICS, 2005
## 6                     2005                         
##                                                                 Special.Notes
## 1                                                                            
## 2                                                                            
## 3 Fiscal year end: March 20; reporting period for national accounts data: FY.
## 4                                                                            
## 5                                                                            
## 6                                                                            
##   National.accounts.base.year National.accounts.reference.year
## 1                        1995                               NA
## 2                                                           NA
## 3                   2002/2003                               NA
## 4                        1997                               NA
## 5                                                         1996
## 6                        1995                               NA
##   System.of.National.Accounts SNA.price.valuation
## 1                          NA                    
## 2                          NA                    
## 3                          NA                 VAB
## 4                          NA                 VAP
## 5                        1993                 VAB
## 6                          NA                 VAB
##   Alternative.conversion.factor PPP.survey.year
## 1                                            NA
## 2                                            NA
## 3                                            NA
## 4                       1991-96            2005
## 5                                          2005
## 6                                            NA
##   Balance.of.Payments.Manual.in.use External.debt.Reporting.status
## 1                                                                 
## 2                                                                 
## 3                                                           Actual
## 4                              BPM5                         Actual
## 5                              BPM5                         Actual
## 6                              BPM4                               
##   System.of.trade Government.Accounting.concept
## 1         Special                              
## 2         General                              
## 3         General                  Consolidated
## 4         Special                              
## 5         General                  Consolidated
## 6         General                  Consolidated
##   IMF.data.dissemination.standard
## 1                                
## 2                                
## 3                            GDDS
## 4                            GDDS
## 5                            GDDS
## 6                            GDDS
##   Source.of.most.recent.Income.and.expenditure.data
## 1                                                  
## 2                                                  
## 3                                                  
## 4                                         IHS, 2000
## 5                                        LSMS, 2005
## 6                                                  
##   Vital.registration.complete Latest.agricultural.census
## 1                                                       
## 2                         Yes                           
## 3                                                       
## 4                                                1964-65
## 5                         Yes                       1998
## 6                                                   1998
##   Latest.industrial.data Latest.trade.data Latest.water.withdrawal.data
## 1                     NA              2008                           NA
## 2                     NA              2006                           NA
## 3                     NA              2008                         2000
## 4                     NA              1991                         2000
## 5                   2005              2008                         2000
## 6                     NA              2008                         2005
##   X2.alpha.code WB.2.code           Table.Name           Short.Name
## 1            AW        AW                Aruba                Aruba
## 2            AD        AD              Andorra              Andorra
## 3            AF        AF          Afghanistan          Afghanistan
## 4            AO        AO               Angola               Angola
## 5            AL        AL              Albania              Albania
## 6            AE        AE United Arab Emirates United Arab Emirates
```

```r
str(EDSTATS)
```

```
## 'data.frame':	234 obs. of  31 variables:
##  $ CountryCode                                      : chr  "ABW" "ADO" "AFG" "AGO" ...
##  $ Long.Name                                        : chr  "Aruba" "Principality of Andorra" "Islamic State of Afghanistan" "People's Republic of Angola" ...
##  $ Income.Group                                     : chr  "High income: nonOECD" "High income: nonOECD" "Low income" "Lower middle income" ...
##  $ Region                                           : chr  "Latin America & Caribbean" "Europe & Central Asia" "South Asia" "Sub-Saharan Africa" ...
##  $ Lending.category                                 : chr  "" "" "IDA" "IDA" ...
##  $ Other.groups                                     : chr  "" "" "HIPC" "" ...
##  $ Currency.Unit                                    : chr  "Aruban florin" "Euro" "Afghan afghani" "Angolan kwanza" ...
##  $ Latest.population.census                         : chr  "2000" "Register based" "1979" "1970" ...
##  $ Latest.household.survey                          : chr  "" "" "MICS, 2003" "MICS, 2001, MIS, 2006/07" ...
##  $ Special.Notes                                    : chr  "" "" "Fiscal year end: March 20; reporting period for national accounts data: FY." "" ...
##  $ National.accounts.base.year                      : chr  "1995" "" "2002/2003" "1997" ...
##  $ National.accounts.reference.year                 : int  NA NA NA NA 1996 NA NA 1996 NA NA ...
##  $ System.of.National.Accounts                      : int  NA NA NA NA 1993 NA 1993 1993 NA NA ...
##  $ SNA.price.valuation                              : chr  "" "" "VAB" "VAP" ...
##  $ Alternative.conversion.factor                    : chr  "" "" "" "1991-96" ...
##  $ PPP.survey.year                                  : int  NA NA NA 2005 2005 NA 2005 2005 NA NA ...
##  $ Balance.of.Payments.Manual.in.use                : chr  "" "" "" "BPM5" ...
##  $ External.debt.Reporting.status                   : chr  "" "" "Actual" "Actual" ...
##  $ System.of.trade                                  : chr  "Special" "General" "General" "Special" ...
##  $ Government.Accounting.concept                    : chr  "" "" "Consolidated" "" ...
##  $ IMF.data.dissemination.standard                  : chr  "" "" "GDDS" "GDDS" ...
##  $ Source.of.most.recent.Income.and.expenditure.data: chr  "" "" "" "IHS, 2000" ...
##  $ Vital.registration.complete                      : chr  "" "Yes" "" "" ...
##  $ Latest.agricultural.census                       : chr  "" "" "" "1964-65" ...
##  $ Latest.industrial.data                           : int  NA NA NA NA 2005 NA 2001 NA NA NA ...
##  $ Latest.trade.data                                : int  2008 2006 2008 1991 2008 2008 2008 2008 NA 2007 ...
##  $ Latest.water.withdrawal.data                     : int  NA NA 2000 2000 2000 2005 2000 2000 NA 1990 ...
##  $ X2.alpha.code                                    : chr  "AW" "AD" "AF" "AO" ...
##  $ WB.2.code                                        : chr  "AW" "AD" "AF" "AO" ...
##  $ Table.Name                                       : chr  "Aruba" "Andorra" "Afghanistan" "Angola" ...
##  $ Short.Name                                       : chr  "Aruba" "Andorra" "Afghanistan" "Angola" ...
```

```r
table(EDSTATS$CountryCode)
```

```
## 
## ABW ADO AFG AGO ALB ARE ARG ARM ASM ATG AUS AUT AZE BDI BEL BEN BFA BGD 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## BGR BHR BHS BIH BLR BLZ BMU BOL BRA BRB BRN BTN BWA CAF CAN CHE CHI CHL 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## CHN CIV CMR COG COL COM CPV CRI CUB CYM CYP CZE DEU DJI DMA DNK DOM DZA 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## EAP EAS ECA ECS ECU EGY EMU ERI ESP EST ETH FIN FJI FRA FRO FSM GAB GBR 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## GEO GHA GIN GMB GNB GNQ GRC GRD GRL GTM GUM GUY HIC HKG HND HPC HRV HTI 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## HUN IDN IMY IND IRL IRN IRQ ISL ISR ITA JAM JOR JPN KAZ KEN KGZ KHM KIR 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## KNA KOR KSV KWT LAC LAO LBN LBR LBY LCA LCN LDC LIC LIE LKA LMC LMY LSO 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## LTU LUX LVA MAC MAR MCO MDA MDG MDV MEA MEX MHL MIC MKD MLI MLT MMR MNA 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## MNE MNG MNP MOZ MRT MUS MWI MYS NAC NAM NCL NER NGA NIC NLD NOC NOR NPL 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## NZL OEC OMN PAK PAN PER PHL PLW PNG POL PRI PRK PRT PRY PYF QAT ROM RUS 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## RWA SAS SAU SDN SEN SGP SLB SLE SLV SMR SOM SRB SSA SSF STP SUR SVK SVN 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## SWE SWZ SYC SYR TCA TCD TGO THA TJK TKM TMP TON TTO TUN TUR TUV TZA UGA 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
## UKR UMC URY USA UZB VCT VEN VIR VNM VUT WBG WLD WSM YEM ZAF ZAR ZMB ZWE 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
```

There are no rows that have blank CountryCode so there is no need to look into deleting rows. However, there are many columns to this data set, and we would only like to analyze the CountryCode, Long.Name, and Income.Group fields.


```r
EDSTATS <- select(EDSTATS, CountryCode, Long.Name, Income.Group)
table(EDSTATS$Long.Name)
```

```
## 
##                                                            American Samoa 
##                                                                         1 
##                                                       Antigua and Barbuda 
##                                                                         1 
##                                                    Arab Republic of Egypt 
##                                                                         1 
##                                                        Argentine Republic 
##                                                                         1 
##                                                                     Aruba 
##                                                                         1 
##                                                                  Barbados 
##                                                                         1 
##                                                                    Belize 
##                                                                         1 
##                                                    Bosnia and Herzegovina 
##                                                                         1 
##                                                         Brunei Darussalam 
##                                                                         1 
##                                                              Burkina Faso 
##                                                                         1 
##                                                                    Canada 
##                                                                         1 
##                                                            Cayman Islands 
##                                                                         1 
##                                                  Central African Republic 
##                                                                         1 
##                                                           Channel Islands 
##                                                                         1 
##                                                 Commonwealth of Australia 
##                                                                         1 
##                                                  Commonwealth of Dominica 
##                                                                         1 
##                                               Commonwealth of The Bahamas 
##                                                                         1 
##                              Commonwealth of the Northern Mariana Islands 
##                                                                         1 
##                                                            Czech Republic 
##                                                                         1 
##                                     Democratic People's Republic of Korea 
##                                                                         1 
##                              Democratic Republic of São Tomé and Principe 
##                                                                         1 
##                                          Democratic Republic of the Congo 
##                                                                         1 
##                                        Democratic Republic of Timor-Leste 
##                                                                         1 
##                                Democratic Socialist Republic of Sri Lanka 
##                                                                         1 
##                                                        Dominican Republic 
##                                                                         1 
##                                   East Asia & Pacific (all income levels) 
##                                                                         1 
##                                     East Asia & Pacific (developing only) 
##                                                                         1 
##                                                                 Euro area 
##                                                                         1 
##                                 Europe & Central Asia (all income levels) 
##                                                                         1 
##                                   Europe & Central Asia (developing only) 
##                                                                         1 
##                                                            Faeroe Islands 
##                                                                         1 
##                                   Federal Democratic Republic of Ethiopia 
##                                                                         1 
##                                               Federal Republic of Germany 
##                                                                         1 
##                                               Federal Republic of Nigeria 
##                                                                         1 
##                                            Federated States of Micronesia 
##                                                                         1 
##                                             Federative Republic of Brazil 
##                                                                         1 
##                                     Former Yugoslav Republic of Macedonia 
##                                                                         1 
##                                                          French Polynesia 
##                                                                         1 
##                                                           French Republic 
##                                                                         1 
##                                                         Gabonese Republic 
##                                                                         1 
##                                                                   Georgia 
##                                                                         1 
##                                                 Grand Duchy of Luxembourg 
##                                                                         1 
##                                                                 Greenland 
##                                                                         1 
##                                                                   Grenada 
##                                                                         1 
##                                                                      Guam 
##                                                                         1 
##                                               Hashemite Kingdom of Jordan 
##                                                                         1 
##                                    Heavily indebted poor countries (HIPC) 
##                                                                         1 
##                                                         Hellenic Republic 
##                                                                         1 
##                                                               High income 
##                                                                         1 
##                                                      High income: nonOECD 
##                                                                         1 
##                                                         High income: OECD 
##                                                                         1 
## Hong Kong Special Administrative Region of the People's Republic of China 
##                                                                         1 
##                                                                   Ireland 
##                                                                         1 
##                                                  Islamic Republic of Iran 
##                                                                         1 
##                                            Islamic Republic of Mauritania 
##                                                                         1 
##                                              Islamic Republic of Pakistan 
##                                                                         1 
##                                              Islamic State of Afghanistan 
##                                                                         1 
##                                                               Isle of Man 
##                                                                         1 
##                                                          Italian Republic 
##                                                                         1 
##                                                                   Jamaica 
##                                                                         1 
##                                                                     Japan 
##                                                                         1 
##                                                        Kingdom of Bahrain 
##                                                                         1 
##                                                        Kingdom of Belgium 
##                                                                         1 
##                                                         Kingdom of Bhutan 
##                                                                         1 
##                                                       Kingdom of Cambodia 
##                                                                         1 
##                                                        Kingdom of Denmark 
##                                                                         1 
##                                                        Kingdom of Lesotho 
##                                                                         1 
##                                                        Kingdom of Morocco 
##                                                                         1 
##                                                         Kingdom of Norway 
##                                                                         1 
##                                                   Kingdom of Saudi Arabia 
##                                                                         1 
##                                                          Kingdom of Spain 
##                                                                         1 
##                                                      Kingdom of Swaziland 
##                                                                         1 
##                                                         Kingdom of Sweden 
##                                                                         1 
##                                                       Kingdom of Thailand 
##                                                                         1 
##                                                Kingdom of the Netherlands 
##                                                                         1 
##                                                          Kingdom of Tonga 
##                                                                         1 
##                                                           Kyrgyz Republic 
##                                                                         1 
##                                          Lao People's Democratic Republic 
##                                                                         1 
##                             Latin America & Caribbean (all income levels) 
##                                                                         1 
##                               Latin America & Caribbean (developing only) 
##                                                                         1 
##                              Least developed countries: UN classification 
##                                                                         1 
##                                                         Lebanese Republic 
##                                                                         1 
##                                                       Low & middle income 
##                                                                         1 
##                                                                Low income 
##                                                                         1 
##                                                       Lower middle income 
##                                                                         1 
##     Macao Special Administrative Region of the People's Republic of China 
##                                                                         1 
##                                                                  Malaysia 
##                                                                         1 
##                            Middle East & North Africa (all income levels) 
##                                                                         1 
##                              Middle East & North Africa (developing only) 
##                                                                         1 
##                                                             Middle income 
##                                                                         1 
##                                                                  Mongolia 
##                                                                         1 
##                                                                Montenegro 
##                                                                         1 
##                                                                     Nepal 
##                                                                         1 
##                                                             New Caledonia 
##                                                                         1 
##                                                               New Zealand 
##                                                                         1 
##                                                             North America 
##                                                                         1 
##                                              Oriental Republic of Uruguay 
##                                                                         1 
##                                   People's Democratic Republic of Algeria 
##                                                                         1 
##                                               People's Republic of Angola 
##                                                                         1 
##                                           People's Republic of Bangladesh 
##                                                                         1 
##                                                People's Republic of China 
##                                                                         1 
##                                            Plurinational State of Bolivia 
##                                                                         1 
##                                                       Portuguese Republic 
##                                                                         1 
##                                                   Principality of Andorra 
##                                                                         1 
##                                             Principality of Liechtenstein 
##                                                                         1 
##                                                    Principality of Monaco 
##                                                                         1 
##                                                               Puerto Rico 
##                                                                         1 
##                                                       Republic of Albania 
##                                                                         1 
##                                                       Republic of Armenia 
##                                                                         1 
##                                                       Republic of Austria 
##                                                                         1 
##                                                    Republic of Azerbaijan 
##                                                                         1 
##                                                       Republic of Belarus 
##                                                                         1 
##                                                         Republic of Benin 
##                                                                         1 
##                                                      Republic of Botswana 
##                                                                         1 
##                                                      Republic of Bulgaria 
##                                                                         1 
##                                                       Republic of Burundi 
##                                                                         1 
##                                                      Republic of Cameroon 
##                                                                         1 
##                                                    Republic of Cape Verde 
##                                                                         1 
##                                                          Republic of Chad 
##                                                                         1 
##                                                         Republic of Chile 
##                                                                         1 
##                                                      Republic of Colombia 
##                                                                         1 
##                                                         Republic of Congo 
##                                                                         1 
##                                                    Republic of Costa Rica 
##                                                                         1 
##                                                 Republic of Côte d'Ivoire 
##                                                                         1 
##                                                       Republic of Croatia 
##                                                                         1 
##                                                          Republic of Cuba 
##                                                                         1 
##                                                        Republic of Cyprus 
##                                                                         1 
##                                                      Republic of Djibouti 
##                                                                         1 
##                                                       Republic of Ecuador 
##                                                                         1 
##                                                   Republic of El Salvador 
##                                                                         1 
##                                             Republic of Equatorial Guinea 
##                                                                         1 
##                                                       Republic of Estonia 
##                                                                         1 
##                                                          Republic of Fiji 
##                                                                         1 
##                                                       Republic of Finland 
##                                                                         1 
##                                                         Republic of Ghana 
##                                                                         1 
##                                                     Republic of Guatemala 
##                                                                         1 
##                                                        Republic of Guinea 
##                                                                         1 
##                                                 Republic of Guinea-Bissau 
##                                                                         1 
##                                                        Republic of Guyana 
##                                                                         1 
##                                                         Republic of Haiti 
##                                                                         1 
##                                                      Republic of Honduras 
##                                                                         1 
##                                                       Republic of Hungary 
##                                                                         1 
##                                                       Republic of Iceland 
##                                                                         1 
##                                                         Republic of India 
##                                                                         1 
##                                                     Republic of Indonesia 
##                                                                         1 
##                                                          Republic of Iraq 
##                                                                         1 
##                                                    Republic of Kazakhstan 
##                                                                         1 
##                                                         Republic of Kenya 
##                                                                         1 
##                                                      Republic of Kiribati 
##                                                                         1 
##                                                         Republic of Korea 
##                                                                         1 
##                                                        Republic of Kosovo 
##                                                                         1 
##                                                        Republic of Latvia 
##                                                                         1 
##                                                       Republic of Liberia 
##                                                                         1 
##                                                     Republic of Lithuania 
##                                                                         1 
##                                                    Republic of Madagascar 
##                                                                         1 
##                                                        Republic of Malawi 
##                                                                         1 
##                                                      Republic of Maldives 
##                                                                         1 
##                                                          Republic of Mali 
##                                                                         1 
##                                                         Republic of Malta 
##                                                                         1 
##                                                     Republic of Mauritius 
##                                                                         1 
##                                                       Republic of Moldova 
##                                                                         1 
##                                                    Republic of Mozambique 
##                                                                         1 
##                                                       Republic of Namibia 
##                                                                         1 
##                                                     Republic of Nicaragua 
##                                                                         1 
##                                                         Republic of Niger 
##                                                                         1 
##                                                         Republic of Palau 
##                                                                         1 
##                                                        Republic of Panama 
##                                                                         1 
##                                                      Republic of Paraguay 
##                                                                         1 
##                                                          Republic of Peru 
##                                                                         1 
##                                                        Republic of Poland 
##                                                                         1 
##                                                        Republic of Rwanda 
##                                                                         1 
##                                                    Republic of San Marino 
##                                                                         1 
##                                                       Republic of Senegal 
##                                                                         1 
##                                                        Republic of Serbia 
##                                                                         1 
##                                                    Republic of Seychelles 
##                                                                         1 
##                                                  Republic of Sierra Leone 
##                                                                         1 
##                                                     Republic of Singapore 
##                                                                         1 
##                                                      Republic of Slovenia 
##                                                                         1 
##                                                  Republic of South Africa 
##                                                                         1 
##                                                      Republic of Suriname 
##                                                                         1 
##                                                    Republic of Tajikistan 
##                                                                         1 
##                                                    Republic of The Gambia 
##                                                                         1 
##                                          Republic of the Marshall Islands 
##                                                                         1 
##                                               Republic of the Philippines 
##                                                                         1 
##                                                     Republic of the Sudan 
##                                                                         1 
##                                                          Republic of Togo 
##                                                                         1 
##                                           Republic of Trinidad and Tobago 
##                                                                         1 
##                                                       Republic of Tunisia 
##                                                                         1 
##                                                        Republic of Turkey 
##                                                                         1 
##                                                        Republic of Uganda 
##                                                                         1 
##                                                    Republic of Uzbekistan 
##                                                                         1 
##                                                       Republic of Vanuatu 
##                                                                         1 
##                                                         Republic of Yemen 
##                                                                         1 
##                                                        Republic of Zambia 
##                                                                         1 
##                                                      Republic of Zimbabwe 
##                                                                         1 
##                                        República Bolivariana de Venezuela 
##                                                                         1 
##                                                                   Romania 
##                                                                         1 
##                                                        Russian Federation 
##                                                                         1 
##                                                                     Samoa 
##                                                                         1 
##                                                           Slovak Republic 
##                                                                         1 
##                                 Socialist People's Libyan Arab Jamahiriya 
##                                                                         1 
##                                             Socialist Republic of Vietnam 
##                                                                         1 
##                                                           Solomon Islands 
##                                                                         1 
##                                                Somali Democratic Republic 
##                                                                         1 
##                                                                South Asia 
##                                                                         1 
##                                                       St. Kitts and Nevis 
##                                                                         1 
##                                                                 St. Lucia 
##                                                                         1 
##                                            St. Vincent and the Grenadines 
##                                                                         1 
##                                                          State of Eritrea 
##                                                                         1 
##                                                           State of Israel 
##                                                                         1 
##                                                           State of Kuwait 
##                                                                         1 
##                                                            State of Qatar 
##                                                                         1 
##                                    Sub-Saharan Africa (all income levels) 
##                                                                         1 
##                                      Sub-Saharan Africa (developing only) 
##                                                                         1 
##                                                         Sultanate of Oman 
##                                                                         1 
##                                                               Switzerland 
##                                                                         1 
##                                                      Syrian Arab Republic 
##                                                                         1 
##                                                              The Bermudas 
##                                                                         1 
##                                 The Independent State of Papua New Guinea 
##                                                                         1 
##                                                              Turkmenistan 
##                                                                         1 
##                                                  Turks and Caicos Islands 
##                                                                         1 
##                                                                    Tuvalu 
##                                                                         1 
##                                                                   Ukraine 
##                                                                         1 
##                                                          Union of Myanmar 
##                                                                         1 
##                                                      Union of the Comoros 
##                                                                         1 
##                                                      United Arab Emirates 
##                                                                         1 
##                      United Kingdom of Great Britain and Northern Ireland 
##                                                                         1 
##                                                     United Mexican States 
##                                                                         1 
##                                               United Republic of Tanzania 
##                                                                         1 
##                                                  United States of America 
##                                                                         1 
##                                                       Upper middle income 
##                                                                         1 
##                                       Virgin Islands of the United States 
##                                                                         1 
##                                                        West Bank and Gaza 
##                                                                         1 
##                                                                     World 
##                                                                         1
```

```r
table(EDSTATS$Income.Group)
```

```
## 
##                      High income: nonOECD    High income: OECD 
##                   24                   37                   30 
##           Low income  Lower middle income  Upper middle income 
##                   40                   56                   47
```

```r
# Notice, there are 24 blanks in Income.Group column.
```

The data are ready to analyze.

## Analysis

## Part 1: Merging Data
### Merge the data based on the country shortcode. How many of the IDs match?


```r
merged <- merge(GDP, EDSTATS, by.x = "COUNTRY_CODE", by.y = "CountryCode", all = FALSE)
nrow(merged)
```

```
## [1] 189
```

There are 189 Country shortcode IDs that matched between the GDP and the Education data.

## Part 2: Sorting Data
### Sort the data frame in ascending order by GDP (so United States is last). What is the 13th country in the resulting data frame?


```r
mergedsort <- merged[order(merged$USD_MIL, decreasing = FALSE),]
mergedsort[13,]
```

```
##    COUNTRY_CODE RANKING             ECONOMY USD_MIL COMMENTS
## 93          KNA     178 St. Kitts and Nevis     767         
##              Long.Name        Income.Group
## 93 St. Kitts and Nevis Upper middle income
```

The 13th country with the lowest GDP in millions of US dollars is St. Kitts and Nevis (Country Code KNA).


## Part 3: Average GDP Rankings
### What are the average GDP rankings for the "High Income: OECD" and "High Income: OECD" groups?


```r
mean(mergedsort$RANKING[mergedsort$Income.Group == "High income: OECD"])
```

```
## [1] 32.96667
```

```r
mean(mergedsort$RANKING[mergedsort$Income.Group == "High income: nonOECD"])
```

```
## [1] 91.91304
```

The average GDP ranking for the "High income: OECD" group is 32.96667, and the average GDP ranking for the "High income: nonOECD" group is 91.91304.

## Part 4: Plotting Data
### Plot the GDP for all of the countries. Use ggplot2 to color your plot by Income Group.


```r
ggplot(mergedsort)+ geom_point(aes(x=ECONOMY,y=USD_MIL,colour=Income.Group))+labs(x = "Country")
```

![](Case_Study_1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

## Part 5: Quantiling Data
### Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. How many countries are Lower middle income but among the 38 nations with highest GDP?


```r
mergedsort$QUANTILE <- cut(mergedsort$RANKING, breaks = 5)
table(mergedsort$QUANTILE, mergedsort$Income.Group)
```

```
##               
##                High income: nonOECD High income: OECD Low income
##   (0.811,38.8]                    4                18          0
##   (38.8,76.6]                     5                10          1
##   (76.6,114]                      8                 1          9
##   (114,152]                       4                 1         16
##   (152,190]                       2                 0         11
##               
##                Lower middle income Upper middle income
##   (0.811,38.8]                   5                  11
##   (38.8,76.6]                   13                   9
##   (76.6,114]                    12                   8
##   (114,152]                      8                   8
##   (152,190]                     16                   9
```

```r
mergedsort[ mergedsort$Income.Group == "Lower middle income" & mergedsort$RANKING <= 38,]
```

```
##     COUNTRY_CODE RANKING          ECONOMY USD_MIL COMMENTS
## 51           EGY      38 Egypt, Arab Rep.  262832         
## 165          THA      31         Thailand  365966         
## 77           IDN      16        Indonesia  878043         
## 78           IND      10            India 1841710         
## 34           CHN       2            China 8227103         
##                      Long.Name        Income.Group     QUANTILE
## 51      Arab Republic of Egypt Lower middle income (0.811,38.8]
## 165        Kingdom of Thailand Lower middle income (0.811,38.8]
## 77       Republic of Indonesia Lower middle income (0.811,38.8]
## 78           Republic of India Lower middle income (0.811,38.8]
## 34  People's Republic of China Lower middle income (0.811,38.8]
```

There are five countries that are Lower middle income but among the 38 nations with highest GDP. These countries are Arab Republic of Egypt, Thailand, Indonesia, India, and China.

## Conclusion

This analysis provided useful outcomes in regards to the countries with higher GDP but have a lower middle income group. This analysis also resulted in a plot of countries by their GDP and Income Group.
