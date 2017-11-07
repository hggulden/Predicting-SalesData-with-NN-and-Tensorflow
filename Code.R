#Normalizing num var
f_scale <- as.data.frame(scale(F_rng[,c(1,2,3,4,5,6)]))
F_factor<-F_rng[,7:24]
FA_norm = data.frame( cbind( f_scale,F_factor))
write.csv(FA_norm, file = "FA_norm.csv",row.names=TRUE)

means_myd<-tapply(F_rg$Chargeable_minutes,list(F_rg$HardwareCommodity.ProcessOwner,F_rg$Owner),mean)

#find highly correlated attributes
correlationMatrix <- round(cor(F_rg[,2:17],use = "complete"),2)
print(correlationMatrix)

#change blank with NA and drop columns
FC<- na.omit(FC)[,-c(3,5)]
#  
F<-sapply(F, function(f){is.na(f)<-which(f == '');f})

#read blank as NA
F<- read.csv("~/TDMS/FA_COST/FA_Final_Set.csv", na.strings="", stringsAsFactors=TRUE)

#Missing Values
sapply(F_rng, function(df) {
round((sum(is.na(df)==TRUE)/ length(df)*100),0)
})

#Convert to factors
FA_c[,13:28] <- lapply(FA_c[,13:28], as.factor)
# for single column
FA_c$LaborCost_Range <- sapply(FA_c$LaborCost_Range, as.factor)

#dplyr This adds new columns, often computed on old ones. But can refer to new columns created. 
#mutate(df, newvar1 = var1-var2, newvar2= newvar1/(existingvar/x))    select(WF, state, area) arrange(summaryDF, desc(total))
#arrange(summarize(group_by(select(filter(fires,type=="WF"),state,area),state),total=sum(area,na.rm=TRUE)),desc(total))

#Quantile and summary stat
table(FA$Family,exclude = NULL)
FA_q<-do.call("rbind", tapply(FA$, FA$HardwareCommodity.ProcessOwner, quantile, c(0.5,0.75,.85,.98)))
FA_s<-summarise(group_by(Fa,HardwareCommodity.ProcessOwner),Mean=round(mean(CHARGEABLE_MINS),0),SE=round(sd(CHARGEABLE_MINS)/sqrt(n()),0))
FA_d<-Fa %>% group_by(EngDisposition) %>% summarise(mean(CHARGEABLE_MINS), SE=round(sd(CHARGEABLE_MINS)/sqrt(n()),0))
mean_ap<-round(tapply(F_rng$Chargeable_minutes,list(F_rng$State,F_rng$Family),mean,na.rm=T),0)

#histogram#
ggplot(subset(FA,HardwareFailure.ProcessIssue %in% c("Cabling","Functional","Test Software","Test setup","Mechanical","Cosmetic")), 
       aes(x = Delta,group=HardwareFailure.ProcessIssue,fill=HardwareFailure.ProcessIssue)) + geom_histogram(fill="darkblue",colour="lightblue",binwidth=.50) + facet_wrap(~HardwareCommodity.ProcessOwner)+ scale_x_log10(breaks=10^(0:6),labels=trans_format("log10",math_format(10^.x)))+
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=16),axis.text.x  = element_text(angle=0, vjust=1, size=16))+
  ggtitle("Hardware/Process Issue Category Delta Histogram")

ggplot(data=FA, aes(x = Delta,group=State)) + geom_histogram(aes(fill=State),binwidth=200) +
  
facet_wrap(~Mfg.Floorlocation, scales = "free_x")+ theme(axis.title.x = element_text(face="bold", colour="#990000", size=16),
axis.text.x  = element_text(angle=0, vjust=1, size=12))+ggtitle("'State' by 'Family' Delta Distribution (Histogram)")

ggplot(subset(F_rng,HardwareFailure.ProcessIssue %in% c("Cabling","Functional","Test Software","Test setup","Mechanical","Cosmetic")),
aes(x = Delta,group=State)) + geom_histogram(aes(fill=State),binwidth=200) +facet_grid(HardwareFailure.ProcessIssue~ShiftCreated, margins = TRUE)+ 
theme(axis.title.x = element_text(face="bold", colour="#990000", size=16),axis.text.x  = element_text(angle=0, vjust=1, size=12))
+ggtitle("'State' by 'Shift', 'Hardware.ProcessIssue' Type Delta Distribution (Histogram)")

#subset#
FT_sub<-subset(F,HardwareFailure.ProcessIssue %in% c("Cabling","Not available',Functional","Test Software","Test setup","Mechanical","Cosmetic"))

#color#
colPal = colorRampPalette(brewer.pal(12, "Paired"))
colC = length(unique(M.l$Unit.Vendor))

#boxplot
getPalette = colorRampPalette(brewer.pal(12, "Paired"))
colourCount = length(unique(M.l$TopLevelSub.family))
ggplot(M.l, aes(x = HardwareCommodityProcessOwner, y = Chargeable_minutes)) + geom_boxplot(aes(fill =HardwareCommodityProcessOwner)
,alpha = .6,size = 1)+ scale_fill_manual(values = getPalette(colourCount),name="Sub Family") +stat_summary(fun.y = "mean", geom = "point",
shape= 22, size= 1, fill= "white") +facet_wrap(~TopLevelSub.family)+ggtitle("Boxplot for 'CHARGEABLE_MINS' Distribution by Owner and Sub Level Family")
+ theme(axis.title.x = element_text(face="bold", colour="black", size=10),
axis.text.x  = element_text(angle=90, vjust=0.5, size=10),axis.text.y=element_text(angle=0,hjust = 2,size=10))+scale_y_log10(breaks=10^(0:6),
labels=trans_format("log10",math_format(10^.x)))+ geom_jitter(position=position_jitter(width=.1), size=.7,col="red")


ggplot(data=Fa, aes(x=EngDisposition, y=CHARGEABLE_MINS, fill=EngDisposition)) + geom_boxplot() +
  + stat_summary(fun.data = "mean_cl_normal", aes(shape="mean"), colour = "red",geom="point") +
  scale_shape_manual("", values=c("mean"="x"))+
  scale_y_log10(breaks=10^(0:6),labels=trans_format("log10",math_format(10^.x)))+
  + theme(axis.title.x = element_text(face="bold", colour="black", size=10),axis.text.x  = element_text(angle=90, 
  vjust=0.5, size=8),axis.text.y=element_text(angle=0,hjust = 2,size=8))+geom_jitter(position=position_jitter(width=.3),
  size=.3,colour="black")

ggplot(M.l, aes(x = HardwareFailureProcessIssue, y =Chargeable_minutes)) + geom_boxplot(aes(fill = HardwareFailureProcessIssue),
alpha = .6,size = 1) + scale_fill_manual(values = colPal(colC),name="EngDisposition") +stat_summary(fun.y = "mean", 
geom = "point", shape= 23, size= 3, fill= "white") +ggtitle("Boxplot for Cost Distribution by Hardware/Process Issue Failure Types") 
+ theme(axis.title.y=element_blank()) + theme(axis.title.x = element_text(face="bold", colour="black", size=10),
axis.text.x  = element_text(angle=90, vjust=0.5, size=10),axis.text.y=element_text(angle=0,hjust = 2,size=10)) +
  scale_y_continuous(labels = function(x) str_wrap(x, width = 100))+geom_jitter(position=position_jitter(width=.2), size=.2)

## Scale all numeric columns in a data frame

performScaling <- TRUE  # Turn it on/off

if (performScaling) {
  
  # Loop over each column.
  for (colName in names(df)) {
    
    # Check if the column contains numeric data.
    if(class(df[,colName]) == 'integer' | class(df[,colName]) == 'numeric') {
      
      # Scale this column (scale() function applies z-scaling).
      df[,colName] <- scale(df[,colName])
    }
  }
}
#plots
ggplot(training, aes(x =HardwareCommodityProcessOwner, y =Chargeable_minutes)) + geom_boxplot
(aes(fill = HardwareCommodityProcessOwner),outlier.colour = "red", outlier.shape = 1,alpha = .6,size = 1) +
  scale_fill_manual(values = getPalette(colourCount),name="Hfailure_Pissue") +stat_summary(fun.y = "mean", geom = "point", shape= 22, size= .5, fill= "white") +
  facet_wrap(~training$TopLevelSub.family)+ggtitle("Boxplot for 'Unit PN Cost' Distribution by Hardware/Process Issue Failure Types") + 
  theme(axis.title.y=element_blank()) + theme(axis.title.x = element_text(face="bold", colour="black", size=10),axis.text.x  =
element_text(angle=90, vjust=0.5, size=10),axis.text.y=element_text(angle=0,hjust = 2,size=10))+scale_y_log10(breaks=10^(0:6),
labels=trans_format("log10",math_format(10^.x)))+ geom_jitter(position=position_jitter(width=.1), size=.3)

##histogram predicted
ggplot(testing, aes(x =exp.gamma.p)) + geom_histogram(fill="lightblue",colour="darkblue",binwidth=10) +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=10),axis.text.x  = element_text(angle=0, vjust=1, size=10))+ggtitle("Chargeable Minutes Distribution for Testing Dataset")

#plots
ggplot(data=F, aes(x=Fault, y=Chargeable_minutes, fill=Fault)) + geom_boxplot() +
  + stat_summary(fun.data = "mean_cl_normal", aes(shape="mean"), colour = "red",geom="point") +scale_shape_manual("", values=c("mean"="x"))+ scale_y_log10(breaks=10^(0:6),labels=trans_format("log10",math_format(10^.x)))+
  + theme(axis.title.x = element_text(face="bold", colour="black", size=10),axis.text.x  = element_text(angle=90, vjust=0.5, size=8),axis.text.y=element_text(angle=0,hjust = 2,size=8))+geom_jitter(position=position_jitter(width=.3), size=.3,colour="red")

#box plot
ggplot(Fa, aes(x =Fault,y=CHARGEABLE_MINS)) + geom_boxplot(aes(fill = EngDisposition),alpha = .6,size = 1) + 
scale_fill_manual(values = getPalette(colourCount),name="Fault") +
stat_summary(fun.y = "mean", geom = "point", shape= 23, size= 1, fill= "white")+scale_y_log10(breaks=10^(0:6),labels=trans_format("log10",math_format(10^.x))) +ggtitle("Boxplot for Chargeable Minutes by 'Fault'") +theme(axis.title.y=element_blank()) + theme(axis.title.x = element_text(face="bold", colour="black", size=10),axis.text.x  = element_text(angle=60, vjust=0.5, size=10),axis.text.y=element_text(angle=0,hjust = 2,size=10)) +geom_jitter(position=position_jitter(width=.2), size=1)
