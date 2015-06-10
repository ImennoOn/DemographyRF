source("demographyHelper.R")

populationsRF_DF <- readData(populationsRF_DF)
list[birthCoefs_DF, deathCoefs_DF] <- readCoefs(birthCoefs_DF, deathCoefs_DF)
list[birthCoefs_DF, deathCoefs_DF] <- koefsCorrect(birthCoefs_DF, deathCoefs_DF, 1991,1994)
list[birthCoefs_DF, deathCoefs_DF] <- koefsCorrect(birthCoefs_DF, deathCoefs_DF, 2014,2050)
populationsRF_DF <- simulateRF(populationsRF_DF, 1990,2001)
populationsRF_DF <- simulateRF(populationsRF_DF, 2003,2009)
populationsRF_DF <- simulateRF(populationsRF_DF, 2011,2013)
populationsRF_DF <- simulateRF(populationsRF_DF, 2015,2050)


#Linear Regression test
{
# {
# selectedYears <- c(2012, 2013, 2014)
# library(dplyr)
# dokDemoData <- dplyr::tbl_df(populationsRF_DF)
# 
# inputDemographyData <- data.frame(years = selectedYears,
#                                   ageGroup = seq(1,22,1),
#                                   birthCoef = birthCoefs_DF[[""]],
#                                   deathCoef = deathCoef_DF[],
#                                   population = sum(subset(populationsRF_DF, 
#                                                           years == selectedYears, 
#                                                           select = c(maleUrbanPopulation, femaleUrbanPopulation, maleRuralPopulation, femaleRuralPopulation)))
# }
}
#EndOfLinearRegression test

dataDemographyRF_transformed <- ddply(populationsRF_DF, c("years", "ageGroup"), summarise,
                                      population = sum(maleUrbanPopulation, femaleUrbanPopulation, maleRuralPopulation, femaleRuralPopulation)
)

dataDemographyRF_transformed$ageGroup <- as.factor(dataDemographyRF_transformed$ageGroup)
dataDemographyRF_transformed@years < as.Date(dataDemographyRF_transformed$years, "%Y%m%d")


with(dataDemographyRF_transformed, qplot(years, population/10^6, color = ageGroup, stat="identity", geom = "area"))

dataDemographyRF_transformed$population
groups <- aggregate(. ~ years, data = populationsRF_DF, FUN=sum)
dataDemographyRF <- data.frame(time = rep(1989:2050, each = 1),
                               var = rep(1:22, each = 1*(2050-1989+1)),
                               val = dataDemographyRF_transformed$population
)



library(googleVis)
df <- data.frame(country=c("US", "GB", "BR"), val1=c(10,13,14), val2=c(23,12,32))
Line <-  gvisLineChart(df, xvar="country", yvar=c("val1","val2"),
                       options=list(
                         title="Hello World",
                         titleTextStyle="{color:'red', 
                                           fontName:'Courier', 
                                           fontSize:16}",                         
                         backgroundColor="#D3D3D3",                          
                         vAxis="{gridlineColor:'#FFFFFF'}",
                         hAxis="{title:'Country', 
                                  titleTextStyle:{color:'blue'}}",
                         series="[{color:'green', targetAxisIndex: 0},
                                         {color: 'orange',targetAxisIndex:1}]",
                         vAxes="[{title:'val1'}, {title:'val2'}]",
                         legend="bottom",
                         curveType='function',
                         width=500,
                         height=300                         
                       ))
plot(Line)

library(XML)
participants=readHTMLTable(readLines("http://www.warwick.ac.uk/statsdept/useR-2011/participant-list.html"), 
                           which=1, stringsAsFactors=F)
names(participants)=c("Name", "Country", "Organisation")
## Correct typo and shortcut
participants$Country <- gsub("Kngdom","Kingdom",participants$Country)
participants$Country <- gsub("USA","United States",participants$Country)
participants$Country <- factor(participants$Country)
partCountry <- as.data.frame(xtabs( ~ Country, data=participants))
library(googleVis)
## Please note the option gvis.editor requires googleVis version >= 0.2.9
G <- gvisGeoChart(partCountry,"Country", "Freq", options=list(gvis.editor="Edit me") )
plot(G)