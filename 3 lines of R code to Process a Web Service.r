#a script to retrieve a list of World Bank Data takes all of three lines of code
library('XML')
doc = xmlTreeParse('http://open.worldbank.org/topics/', useInternal = TRUE)
sapply(getNodeSet(doc, "//wb:value") , function(el) xmlValue(el))

#if you wanted to alert your friends in New Zealand and Australia of the
#excellent opportunity they had to be first to market vs. their competition in
#Singapore and America who are starting businesses (based upon 2008 data):
countries=c('NZL','AUS','SGP','USA')
x=lapply(countries, function(country) {
url=paste('http://open.worldbank.org/countries/',
  country,
  '/indicators/IC.REG.DURS?date=2008:2008',sep=''
 )

doc = xmlTreeParse(url, useInternal = TRUE)
c(as.numeric(xmlValue(getNodeSet(doc,"//wb:value")[[1]])))
} )

pie(unlist(x),labels=countries)
title('Time Required To Start a Business (Days)')
