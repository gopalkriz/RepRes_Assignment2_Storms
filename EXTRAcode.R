
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