#Coursera # Reproductive Research #Duration 4 weeks / July-Aug2015
#Assignment2 - Storms
#https://class.coursera.org/repdata-031/human_grading/view/courses/975144/assessments/4/submissions

#Storm-events in the database start in the year 1950 and end in November 2011
#NOAA Storm Database: verylarge CSV bzip2 [47Mb] #https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2
#Instructions / Variable Name: #https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf
#FAQ #https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf
#----------------------#
#Document Layout
#Title: Your document should have a title that briefly summarizes your data analysis
#Synopsis: describes and summarizes your analysis in at most 10 complete sentences.
#
#section titled Data Processing which describes (in words and code) how the data were loaded into R and processed for analysis.
#In particular, your analysis must start from the raw CSV file containing the data. You cannot do any preprocessing outside the document.
#If preprocessing is time-consuming you may consider using the cache = TRUE option for certain code chunks.
#
#section titled Results in which your results are presented.
#
#At least one figure containing a plot/ no more than three figures.
#
#You must show all your code for the work in your analysis document.echo = TRUE
#----------------------#

setwd("D:/VIVEK/DataAnalysis/Coursera/5 RepRes/RepRes_Assignment2_Storms"); getwd()

require(stringr,  quietly = TRUE)
require(lubridate,  quietly = TRUE)
require(reshape2,  quietly = TRUE)
require(ggplot2,  quietly = TRUE)
require(scales, quietly = TRUE)
require(dplyr,  quietly = TRUE)

library(stringr,  quietly = TRUE)
library(lubridate,  quietly = TRUE)
library(reshape2,  quietly = TRUE)
library(ggplot2,  quietly = TRUE)
library(scales, quietly = TRUE)
library(dplyr,  quietly = TRUE)

sessionInfo()
Version 0.99.442 - © 2009-2015 RStudio, Inc.
#Final rendered HTML output at Rpubs:
#Reproducible study available at Github:

#----------------------#

##Loading the data
webURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipfilepath <- "repdata_data_StormData.csv.bz2"
rdsfilepath <- "StormData.RDS"

if (!file.exists(zipfilepath)) {
	message("wait for file to be downloaded from website-url")
	download.file(url = webURL, destfile = zipfilepath) # not used method = "curl"
}

#for RDS file, ie cache of already read file
#delete RDS file before running knitR
RDSloaded <- FALSE
message("main data set is being loaded")
if (!file.exists("rdsfilepath")) {
	message("large file...do patiently wait...will take many minutes")
	maindata <- read.csv(file = bzfile(zipfilepath), strip.white = TRUE)
	saveRDS(maindata, file = "rdsfilepath")
} else {
	message("extracting from compressed file and reading dataset into environment")
	maindata <- readRDS("rdsfilepath")
	RDSloaded <- TRUE
}

#----------------------#
a <- read.csv(file = "D:/VIVEK/DataAnalysis/Coursera/5 RepRes/RepRes_Assignment2_Storms/a.csv")
b <- read.csv(file = "D:/VIVEK/DataAnalysis/Coursera/5 RepRes/RepRes_Assignment2_Storms/b.csv")
subset <- read.csv(file = "D:/VIVEK/DataAnalysis/Coursera/5 RepRes/RepRes_Assignment2_Storms/subset.csv")
winddata <- read.csv(file = "D:/VIVEK/DataAnalysis/Coursera/5 RepRes/RepRes_Assignment2_Storms/winddata.csv")
#----------------------#

dim(maindata); ncol(maindata); nrow(maindata)
str(maindata); names(maindata)
head(maindata, n=3)

plot(table(maindata$STATE__)) #
#table(maindata$BGN_DATE)
#table(maindata$BGN_TIME)
plot(log(table(maindata$TIME_ZONE))) #
#table(maindata$COUNTY)
#table(maindata$COUNTYNAME)
plot(table(maindata$STATE)) #
plot(log(table(maindata$EVTYPE))) #
#table(maindata$BGN_RANGE)
#table(maindata$BGN_AZI)
#table(maindata$BGN_LOCATI)
#table(maindata$END_DATE)
#table(maindata$END_TIME)
#table(maindata$COUNTY_END)
#table(maindata$COUNTYENDN)
#table(maindata$END_RANGE)
#table(maindata$END_AZI)
#table(maindata$END_LOCATI)
#table(maindata$LENGTH)
#table(maindata$WIDTH)
#table(maindata$F)
#table(maindata$MAG)
plot(log(table(maindata$FATALITIES))) #
plot(log(table(maindata$INJURIES))) #
#table(maindata$PROPDMG)
table(maindata$PROPDMGEXP) #
#table(maindata$CROPDMG)
table(maindata$CROPDMGEXP) #
#table(maindata$WFO)
#table(maindata$STATEOFFIC)
#table(maindata$ZONENAMES)
#table(maindata$LATITUDE)
#table(maindata$LONGITUDE)
#table(maindata$LATITUDE_E)
#table(maindata$LONGITUDE_)
#table(maindata$REMARKS)
#table(maindata$REFNUM) 

#----------------------#

##Cleaning the data
message("these code-lines take a few minutes to calculate")
multifunction <- function(unit, multiplier) unit * switch(toupper(multiplier), H=100, K=1000, M=1000000, B=1000000000, 1)
maindata$propDamage <- with(maindata, mapply(multifunction, PROPDMG, PROPDMGEXP))
maindata$cropDamage <- with(maindata, mapply(multifunction, CROPDMG, CROPDMGEXP))
message("two columns added to maindata for PropertyDamage and CropDamage in numerical_USD_Amount")
#
#convert dates to POSIXct format
str(maindata$BGN_DATE)
maindata$dateUTC <- mdy(str_extract(maindata$BGN_DATE, "[^ ]+"))
message("column added for Date as POSIXct")
#
#cleaning the EVTYPE and making Category
str(maindata$EVTYPE);levels(maindata$EVTYPE)
levels(maindata$EVTYPE)<- tolower(levels(maindata$EVTYPE))
maindata$EVTYPEcat <- maindata$EVTYPE
#
names(maindata)
#----------------------#
a<-maindata

#----------------------#
str(a$EVTYPE);levels(a$EVTYPE)
levels(a$EVTYPE)<- tolower(levels(a$EVTYPE))
#
levels(a$EVTYPEcat)[grepl("lightning |lighting|lightning|ligntning| lightning| ligntning|lightning.|lighting",levels(a$EVTYPEcat),ignore.case = T)] <- "lightning"
levels(a$EVTYPEcat)[grepl("rain|hail|flood|wet|fld|stream|downburst|precip|precipatation|shower|microburst", levels(a$EVTYPEcat),ignore.case = T)] <- "rain"
levels(a$EVTYPEcat)[grep("snow|winter|wintry|blizzard|glaze|hail|spout|sleet|cold|ice|freez|avalanche|icy|frost",levels(a$EVTYPEcat),ignore.case = T)] <- "winter"
levels(a$EVTYPEcat)[grepl("thunder|tstm|typhoon|tornado|torndao|wnd|wind|swells|storm|hurricane|funnel|tropical +storm|gustnado|turbulence",levels(a$EVTYPEcat),ignore.case = T)] <- "wind"
levels(a$EVTYPEcat)[grepl("fire|smoke",levels(a$EVTYPEcat),ignore.case = T)] <- "fire & smoke"
levels(a$EVTYPEcat)[grepl("fog|visibility|dark|dust",levels(a$EVTYPEcat),ignore.case = T)] <- "low visibility"
levels(a$EVTYPEcat)[grepl("surf|surge|tide|tsunami|current|rough + seas|wave|depression| rapidly rising water| seas",levels(a$EVTYPEcat),ignore.case = T)] <- "ocean surge"
levels(a$EVTYPEcat)[grepl("heat|high +temp|temperature|record +temp|warm|dry|hot",levels(a$EVTYPEcat),ignore.case = T)] <- "heat"
levels(a$EVTYPEcat)[grepl("volcan",levels(a$EVTYPEcat),ignore.case = T)] <- "volcanic activity"
levels(a$EVTYPEcat)[grepl("avalance|flooding|slide|mud|dam|flash|landslump|erosion| erosin|rapidly rising water",levels(a$EVTYPEcat),ignore.case = T)] <- "flood, erosion & avalanche"
levels(a$EVTYPEcat)[grepl("summary|southeast|vog|none|northern|\\?|other|urban|small|criteria|apache|floyd",levels(a$EVTYPEcat),ignore.case = T)] <- "unknown condition" 
levels(a$EVTYPEcat)[grepl("wallcloud|county|record+low|excessive|high|seiche|heavy mix|excessive|no severe weather",levels(a$EVTYPEcat),ignore.case = T)] <- "unknown condition"
levels(a$EVTYPEcat)[grepl("marine",levels(a$EVTYPEcat),ignore.case = T)] <- "marine"
levels(a$EVTYPEcat)[grepl("hyperthermia/exposure|drowning",levels(a$EVTYPEcat),ignore.case = T)] <- "documented accidents"
levels(a$EVTYPEcat)[grepl("unseasonal low temp|driest|record low|unseasonably cool|cool spell|drought|large wall cloud|record cool|mild pattern|wall cloud",levels(a$EVTYPEcat),ignore.case = T)] <- "others"
#
str(a$EVTYPEcat);levels(a$EVTYPEcat) #985 levels have been cleaned to 14
table(a$EVTYPEcat)
#----------------------#

b<-a
write.csv(b, file = "b.csv")

#----------------------#
#Graphical: Exploratory Data Analysis
#make plots invisible
#
hist(b$dateUTC, breaks = 61)
plot(b$dateUTC,b$FATALITIES)
plot(b$dateUTC,b$INJURIES)
pairs(EVTYPE ~ FATALITIES + INJURIES + propDamage + cropDamage,data = b,
	subset = FATALITIES >0 & INJURIES >0 & propDamage >0 & propDamage>0, main = "pairs plot")
plot(b$EVTYPE,b$propDamage)
plot(b$EVTYPE,b$cropDamage)
#
t<-nrow(b)#Ans: total recorded incidents 	   [1] 902,297
#
dl<-sum(b$FATALITIES) #Ans: death_count 	   [1] 15,145
icd<-sum(b$FATALITIES >0, na.rm=TRUE)#Ans: fatality_incidents [1] 6,974
jl<-sum(b$INJURIES)   #Ans: injury_count	   [1] 140,528
ici<-sum(b$INJURIES >0, na.rm=TRUE)#Ans: injury_incidents  [1] 17,604
#
pl<-sum(b$propDamage) #Ans: Total Dollar Value [1] 427,318,652,972
icp<-sum(b$propDamage >0, na.rm=TRUE)#Ans: propertyloss_incidents  [1] 239,174
cl<-sum(b$cropDamage) #Ans: Total Dollar Value [1] 49,104,192,181
icc<-sum(b$cropDamage >0, na.rm=TRUE)#Ans: croploss_incidents  [1] 22,099
#
#Ratio of number of Human_Incident to total recorderd Incidents
t/icd #1 in every 130, recorded incidents has led to atleast 1 fatality
t/ici #1 in every 51, recorded incidents has led to atleast 1 injury
#
#Ratio of number of Damage_Incident to total recorderd Incidents
t/icp #1 in every 3, recorded incidents has caused property damage
t/icc #1 in every 40, recorded incidents has caused crop damage
#
#Average death count per fatality_incident
dl/icd #Ans: 2.17 deaths per incident
#Average death count per injury_incident
jl/ici #Ans: 7.98 injured per incident
#Average death count per propertyloss_incident
pl/icp #Ans: $1,786,643 loss per incident
#Average death count per croploss_incident
cl/icc #Ans: $2,222,010 loss per incident
#
incident_count<-c(icd/10^3,ici/10^3,icp/10^3,icc/10^3)
affected_count<-c(dl/10^3,jl/10^3)
damage_value<-c(pl/10^9,cl/10^9)
#
barplot(incident_count)
barplot(affected_count)
barplot(damage_value)
#
#----------------------#
rrange(summarise(b, FATALITIES=sum(FATALITIES, na.rm = T)),desc(FATALITIES))
arrange(summarise(b, INJURIES=sum(INJURIES, na.rm = T)),desc(INJURIES))
arrange(summarise(b, propDamage=sum(propDamage, na.rm = T)),desc(propDamage))
arrange(summarise(b, cropDamage=sum(cropDamage, na.rm = T)),desc(cropDamage))
#----------------------#
#caution note: group_by does not work if 'plyr'is loaded after 'dplyr
#search() # display the packages
#detach("package:dplyr", unload=TRUE) # unload the package
#
#----------------------#
#Combine Fatalities and Injuries into a single plot
names(b)
b <- group_by(b, EVTYPE)
#damage <- filter(b, propDamage > 0 & propDamage > 0)
harmful <- summarise(b, FATALITIES=sum(FATALITIES, na.rm = T), INJURIES=sum(INJURIES, na.rm = T))
harmful <- arrange(harmful, desc(FATALITIES+INJURIES))
names(harmful) <- c("EVTYPE","human_deaths","human_injuries")
mostharmful <- melt(harmful[1:5,], id.vars = "EVTYPE", measure.vars = c("human_deaths","human_injuries"))
leastharmful <- melt(harmful[9:14,], id.vars = "EVTYPE", measure.vars = c("human_deaths","human_injuries"))
#
par(mfrow = c(1, 2))
#grid.arrange(plot1, plot2, ncol=1, sub="Figure 1")

plot1 <- ggplot(data = mostharmful, aes(x=reorder(EVTYPE, desc(value)), y=value/10^3, fill=variable)) +
	geom_bar(stat = "identity", position = "dodge") +
	ylab("Number of Person (Thousand)") + 
	xlab("Major Event Category") +
	labs(fill = "Incidents") +
	theme(legend.position= c(0.8,0.8), legend.background = element_rect(fill="transparent"))+
	ggtitle("Top 5 Weather Events \n Dangerous for Human Life") +
	theme(plot.title = element_text(lineheight=.8, face="bold"))
	geom_text(aes(label = value), size =3, hjust = 0.5, vjust = -0.5, alpha = 0.5)
plot1
#Top Five Categories: wind rain heat winter lightning
#----------------------#
#skip this section
subset1<-subset(sdata,sdata$EVTYPE==c("wind"))
subset2<-subset(sdata,sdata$EVTYPE==c("rain"))
subset3<-subset(sdata,sdata$EVTYPE==c("heat"))
subset4<-subset(sdata,sdata$EVTYPE==c("winter"))
subset5<-subset(sdata,sdata$EVTYPE==c("lightning"))
#
subset<-rbind(subset1, subset2, subset3, subset4, subset5)
#
ggplot(subset, aes(x = dateUTC, y = FATALITIES, group = EVTYPE)) +
	geom_line(alpha= 0.5) + 
	scale_y_continuous(limits = c(0,100), oob=rescale_none) +
	facet_wrap( ~ EVTYPE) +
	coord_flip()
	
ggplot(subset, aes(x = dateUTC, y = INJURIES, group = EVTYPE)) +
	geom_line(alpha= 0.5) +
	scale_y_continuous(limits = c(0,500), oob=rescale_none) +
	facet_wrap( ~ EVTYPE) +
	coord_flip()
#----------------------#

plot9<-ggplot(subset, aes(x = dateUTC, y = INJURIES, group = EVTYPE)) +
	geom_line(aes(stat = "identity", colour="human_injuries", alpha= 0.5))+
	geom_line(data = subset,aes(y = FATALITIES, stat = "identity", colour="human_deaths", alpha= 0.1))+
	scale_y_continuous(limits = c(0,500), oob=rescale_none) +
	facet_wrap( ~ EVTYPE) +
	coord_flip()+
	xlab("time-line.years 1950-2011") +
	ylab("quantity of casuality") +
	ggtitle("Timeline: Annual Casuality Incidents. EVTYPE by Panel \nby Top 5 Weather Events -dangerous for humanlife") +
	theme(plot.title = element_text(lineheight=.6, face="bold")) +
	theme(legend.position= c(0.9,0.2), legend.background = element_rect(fill="transparent"))+
	theme(legend.title = element_text(colour="grey", size=12, face="bold"))+
	scale_color_discrete(name="Incidents")
plot9

#----------------------#

grid.arrange(plot1, plot9, ncol=2, sub="Figure 1")

#----------------------#
#skip this section
table(b$EVTYPE)
sdata <- subset(maindata, b$EVTYPE == "wind" && "rain", select = c(EVTYPE, FATALITIES, INJURIES, dateUTC))

selected <-c(wind, rain, heat, winter, lightning)
selected <-c("wind", "rain", "heat", "winter", "lightning")
selected[1]

sdata <- subset(b, select = c("EVTYPE", "FATALITIES", "INJURIES", "dateUTC"))
subset<-NULL
harmeventdata<-NULL
i=1
while(i<6){

i=i+1
harmeventdata <- rbind(harmeventdata,subset)
}
#----------------------#
#skip this section
write.csv(subset, file = "subset.csv")

#----------------------#
#skip this section
sdata <- subset(b, b$EVTYPE == , select = c(EVTYPE, FATALITIES, INJURIES, dateUTC))

ggplot(sdata, aes(x = dateUTC, y = FATALITIES, group = EVTYPE)) +
    geom_line() +
    facet_wrap( ~ EVTYPE) +
	coord_flip()

geom_line(mapping = aes(y = Upper), lty = "dashed") +
	geom_line(mapping = aes(y = Lower), lty = "dashed") +
	geom_line(mapping = aes(y = Signif), lwd = 1.3, colour = "red") +

harmful <- summarise(b, FATALITIES=sum(FATALITIES, na.rm = T), INJURIES=sum(INJURIES, na.rm = T))
harmful <- arrange(harmful, desc(FATALITIES+INJURIES))
names(harmful) <- c("EVTYPE","human_deaths","human_injuries")
mostharmful <- melt(harmful[1:5,], id.vars = "EVTYPE", measure.vars = c("human_deaths","human_injuries"))
leastharmful <- melt(harmful[9:14,], id.vars = "EVTYPE", measure.vars = c("human_deaths","human_injuries"))
#
#----------------------#

#levels(a$EVTYPE)[grepl("thunder|tstm|typhoon|tornado|torndao|wnd|wind|swells|storm|hurricane|funnel|tropical +storm|gustnado|turbulence",levels(a$EVTYPE),ignore.case = T)] <- "wind"
#seek deeper answer in wind
#"wind" >>>> thunder|tstm|typhoon|tornado|torndao|wnd|wind|swells|storm|hurricane|funnel|tropical +storm|gustnado|turbulence
#
#play with dataset maindataset
names(b)
winddata<-select(b, STATE__, STATE, EVTYPE, FATALITIES, INJURIES, propDamage, cropDamage, dateUTC, EVTYPEcat)
#winddata$EVTYPEcat<-as.factor(tolower(winddata$EVTYPEcat))
table(winddata$EVTYPEcat)
#
#----------------------#

write.csv(winddata, file = "winddata.csv")

#----------------------#

windset<- filter(winddata, EVTYPEcat == "wind", rm.na= T)
windset<- group_by(windset, EVTYPE)
windharm <- melt(windset[1:900,], id.vars = "EVTYPE", measure.vars = "STATE")

#----------------------#

gh <- filter(winddata, EVTYPEcat == "wind" | EVTYPEcat == "rain" | EVTYPEcat == "heat" | EVTYPEcat == "winter" | EVTYPEcat == "lightning")

ph <- filter(winddata, EVTYPEcat == "wind")
levels(ph$EVTYPE)

plot(ph$STATE, ph$EVTYPE)

#----------------------#

table(windset$EVTYPE)
#
plot3
plot3 <- ggplot(data = windharm, aes(x=reorder(EVTYPE, desc(value)), y=value, fill=variable)) +
	geom_bar(stat = "identity", position = "dodge")



plot3 <- ggplot(data = windharm, aes(x=reorder(EVTYPE, desc(value)), y=value, fill=variable)) +
	geom_bar(stat = "identity", position = "dodge") +
	ylab("Number of Person (Thousand)") + 
	xlab("Major Event Category") +
	labs(fill = "Incidents") +
	theme(legend.position= c(0.8,0.8), legend.background = element_rect(fill="transparent"))+
	ggtitle("Top 5 Weather Events \n Dangerous for Human Life") +
	theme(plot.title = element_text(lineheight=.8, face="bold"))
geom_text(aes(label = value), size =3, hjust = 0.5, vjust = -0.5, alpha = 0.5)

#----------------------#

plot(windset$EVTYPE, windset$FATALITIES, type = "p")
plot(windset$EVTYPE, windset$INJURIES, type = "h")
plot(windset$dateUTC, windset$FATALITIES, type = "l")
plot(windset$dateUTC, windset$INJURIES, type = "l")

plot(windset$EVTYPE, windset$propDamage, type = "h")
plot(windset$EVTYPE, windset$cropDamage, type = "h")
plot(windset$dateUTC, windset$propDamage, type = "l")
plot(windset$dateUTC, windset$cropDamage, type = "l")

#----------------------#

#
winddata$EVTYPEcat[grepl("thunder|tstm|typhoon|tornado|torndao|wnd|wind|swells|storm|hurricane|funnel|tropical +storm|gustnado|turbulence",winddata$EVTYPE,ignore.case = T)] <- "wind"
table(winddata$EVTYPEcat)
#
winddata <- winddata[winddata$EVTYPEcat %in% c("wind"),]
winddata$EVTYPEcat <- as.factor(winddata$EVTYPEcat)
levels(winddata$EVTYPE)
#
########here to look at########
plot(winddata$EVTYPE ~ winddata$FATALITIES)+INJURIES)
plot(winddata$EVTYPE,winddata$FATALITIES)

#----------------------#
#Q1.Which types of events (as indicated in the EVTYPE variable) are most harmful
#with respect to population health?[Across the United States]
#----------------------#
#Q2.Which types of events have the greatest economic consequences?[Across the United States]
#----------------------#
b <- group_by(b, EVTYPEcat)
damage <- summarise(b, propDamage=sum(propDamage, na.rm = T), cropDamage=sum(cropDamage, na.rm = T))
damage <- arrange(damage, desc(propDamage+cropDamage))
damage <- melt(damage[1:5,], id.vars = "EVTYPEcat", measure.vars = c("propDamage","cropDamage"))
#
par(mfrow = c(1, 2))
#grid.arrange(plot1, plot2, ncol=1, sub="Figure 1")
plot2
plot2 <- ggplot(data = damage, aes(x=reorder(EVTYPEcat, desc(value)), y=value/10^9, fill=variable)) +
	geom_bar(stat = "identity", position = "dodge")+
	ylab("Financial Measure: Loss in $ (Billion)") + 
	xlab("Major Event Category") +
	labs(fill = "Legend") +
	theme(legend.position= c(0.8,0.8), legend.background = element_rect(fill="transparent"))+
	ggtitle("Top 5 Weather Events \n causing Economic Damage ") +
	theme(plot.title = element_text(lineheight=.8, face="bold"))+
	geom_text(aes(label = round(value/10^9)), size =3, hjust = -0.4, vjust = -0.6, alpha = 0.3)
#----------------------#
#geo-plot

install.packages("maps")
library(maps)

map("state", interior = FALSE)
map("state", boundary = FALSE, col="gray", add = TRUE)
#----------------------#
%>% droplevels()
require(dplyr)
windclean <- filter(winddata, propDamage > 0)
windclean <- filter(winddata, dateUTC > by.)
windclean <- group_by(windclean, EVTYPE)

AnnualpropDam <- aggregate(windclean$propDamage, by=list(year = windclean$dateUTC), FUN=sum)


mapply(

plot(AnnualpropDam, xlab='year', ylab= 'abcd', main = 'xyz')
```



#----------------------#