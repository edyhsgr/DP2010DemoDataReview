##########
#US CENSUS BUREAU PRIVACY PROTECTED 2010 DEMONSTRATION DATA FOR CALIFORNIA
#
#DEMONSTRATION DATA DOWNLOADED MARCH 18, 2022 FROM THE US CENSUS BUREAU
#PUBLISHED 2010 CENSUS DATA DOWNLOADED JULY 15, 2020 FROM IPUMS NHGIS, UNIVERSITY OF MINNESOTA: https://www.nhgis.org/privacy-protected-demonstration-data
#THIS IS UPDATE OF https://shiny.demog.berkeley.edu/DP2010DemoData_CA_Jul2020/
#
#EDDIE HUNSINGER, MARCH 2022
#https://edyhsgr.github.io/
#
#THERE IS NO WARRANTY FOR THIS CODE
#THIS CODE HAS NOT BEEN TESTED AT ALL-- PLEASE LET ME KNOW IF YOU FIND ANY PROBLEMS (edyhsgr@gmail.com)
##########

	##########
	##DOWNLOADING AND SELECTING DATA FROM US CENSUS BUREAU'S MARCH 2022 DEMONSTRATION DATASET
	##https://www2.census.gov/programs-surveys/decennial/2020/program-management/data-product-planning/2010-demonstration-data-products/02-Demographic_and_Housing_Characteristics/2022-03-16_Summary_File/
	##https://content.govdelivery.com/accounts/USCENSUS/bulletins/30eee33
	##########
	#download.file("https://www2.census.gov/programs-surveys/decennial/2020/program-management/data-product-planning/2010-demonstration-data-products/02-Demographic_and_Housing_Characteristics/2022-03-16_Summary_File/2022-03-16_Summary_File_Person/California/ca2010.dhc.zip",
	#	destfile=".../CA_March2022Demonstration.zip")
	#
	#Names_DP<-data.frame(read.table(unzip(".../CA_March2022Demonstration.zip","cageo2010.dhc"),sep="|",fill=TRUE))
	#	Names_DP<-Names_DP[,c(1:9,88)]
	#
	#DataSelectionAge_DP<-data.frame(read.table(unzip(".../CA_March2022Demonstration.zip","ca000022010.dhc"),sep="|",fill=TRUE))
	#	DataSelectionAge_DP<-DataSelectionAge_DP[,c(1:5,150:198)]
	#DataSelectionRaceHisp_DP<-data.frame(read.table(unzip(".../CA_March2022Demonstration.zip","ca000012010.dhc"),sep="|",fill=TRUE))
	#	DataSelectionRaceHisp_DP<-DataSelectionRaceHisp_DP[,c(1:5,25:32)]
	#DataSelection_DP<-merge(DataSelectionAge_DP,DataSelectionRaceHisp_DP,by="V5")
	#Data_DP<-merge(Names_DP,DataSelection_DP,by.x="V8",by.y="V5")
	#
	#Data_DP_counties<-Data_DP[Data_DP$V3==50,]
	#Data_DP_counties<-Data_DP_counties[Data_DP_counties$V5=="00",]
	#Data_DP_counties<-Data_DP_counties[!duplicated(Data_DP_counties), ]
	#
	#Data_DP_places<-Data_DP[Data_DP$V3==160,]
	#Data_DP_places<-Data_DP_places[Data_DP_places$V5=="00",]
	#Data_DP_places<-Data_DP_places[!duplicated(Data_DP_places), ]
	#
	#Data_DP<-rbind(Data_DP_counties,Data_DP_places)
	#colnames(Data_DP)<-seq(1:ncol(Data_DP))
	#
	#head(Data_DP[,1:10],5)
	#
	#write.table(Data_DP,file=".../Select_USCensusBureauMarch2022DP2010Demonstration.csv",sep=",",row.names=FALSE)
	##########

library(shiny)

Names<-read.table(file="https://github.com/edyhsgr/DP2010DemoDataReview/raw/master/nhgis_ppdd_20200527_place_county_names_CA.csv",header=TRUE,sep=",")
Names<-Names$name

ui<-fluidPage(

	tags$h3("U.S. Census Bureau's 2010 Demonstration Data (March 2022 release): Population by Demographic Characteristics, California Counties, Cities, and Places"),
	p("Demonstration data downloaded March 18, 2022 from the ",
	tags$a(href="https://content.govdelivery.com/accounts/USCENSUS/bulletins/30eee33", "US Census Bureau.")),
	p("Published 2010 Census data downloaded July 15, 2020 from ",
	tags$a(href="https://www.nhgis.org/privacy-protected-demonstration-data", "IPUMS NHGIS, University of Minnesota.")),
	p("Update of ",
	tags$a(href="https://shiny.demog.berkeley.edu/eddieh/DP2010DemoData_CA_Jul2020/", "July 2020."),
	"",
	"",
	tags$a(href="", 
	"")
),
  
hr(),

sidebarLayout(
sidebarPanel(

selectizeInput(inputId = "Area", label = "County, city, or census designated place (CDP)", 
choices = Names,
options = list(placeholder = "Type in a county, city, or place to see graphs", multiple = TRUE, maxOptions = 5000, onInitialize = I('function() { this.setValue(""); }'))
),

hr(),

p("This interface was made with ",
tags$a(href="https://shiny.rstudio.com/", 
	"Shiny for R."),
tags$a(href="https://github.com/edyhsgr/DP2010DemoDataReview", 
	"Related GitHub repository."),
"March 2022."),

width=3
),

mainPanel(
	
	plotOutput("plots"),width=3)
)
)

PopData<-read.table(file="https://github.com/edyhsgr/DP2010DemoDataReview/raw/master/nhgis_ppdd_20200527_place_county_CA.csv",header=TRUE,sep=",")
Data_DPMarch2022<-read.table(file="https://github.com/edyhsgr/DP2010DemoDataReview/raw/master/Select_USCensusBureauMarch2022DP2010Demonstration.csv",header=TRUE,sep=",")
PopData<-merge(PopData,Data_DPMarch2022,by.x="name",by.y="X10",all=TRUE)

server<-function(input,output) {	
	output$plots<-renderPlot({
par(mfrow=c(2,2)) #,mai=c(0.5,0.5,0.5,0.5))

AgeMale<-subset(PopData, PopData$name==input$Area)	#input$Area
AgeMale$H76006_sf<-sum(AgeMale$H76006_sf,AgeMale$H76007_sf)
AgeMale$H76008_sf<-sum(AgeMale$H76008_sf,AgeMale$H76009_sf,AgeMale$H76010_sf)
AgeMale$H76018_sf<-sum(AgeMale$H76018_sf,AgeMale$H76019_sf)
AgeMale$H76020_sf<-sum(AgeMale$H76020_sf,AgeMale$H76021_sf)
AgeMale$H76006_dp<-sum(AgeMale$H76006_dp,AgeMale$H76007_dp)
AgeMale$H76008_dp<-sum(AgeMale$H76008_dp,AgeMale$H76009_dp,AgeMale$H76010_sf)
AgeMale$H76018_dp<-sum(AgeMale$H76018_dp,AgeMale$H76019_dp)
AgeMale$H76020_dp<-sum(AgeMale$H76020_dp,AgeMale$H76021_dp)
AgeMale$"X20"<-sum(AgeMale$"X20",AgeMale$"X21")
AgeMale$"X22"<-sum(AgeMale$"X22",AgeMale$"X23",AgeMale$"X24")
AgeMale$"X32"<-sum(AgeMale$"X32",AgeMale$"X33")
AgeMale$"X34"<-sum(AgeMale$"X34",AgeMale$"X35")
AgeMale<-AgeMale[,-c(1:322,327,329,330,339,341,346:698,703,705,706,715,717,722:755)]
AgeMale<-AgeMale[,-c(37:51,56,58:59,68,70,75:110)]

AgeFemale<-subset(PopData, PopData$name==input$Area)
AgeFemale$H76030_sf<-sum(AgeFemale$H76030_sf,AgeFemale$H76031_sf)
AgeFemale$H76032_sf<-sum(AgeFemale$H76032_sf,AgeFemale$H76033_sf,AgeFemale$H76034_sf)
AgeFemale$H76042_sf<-sum(AgeFemale$H76042_sf,AgeFemale$H76043_sf)
AgeFemale$H76044_sf<-sum(AgeFemale$H76044_sf,AgeFemale$H76045_sf)
AgeFemale$H76030_dp<-sum(AgeFemale$H76030_dp,AgeFemale$H76031_dp)
AgeFemale$H76032_dp<-sum(AgeFemale$H76032_dp,AgeFemale$H76033_dp,AgeFemale$H76034_sf)
AgeFemale$H76042_dp<-sum(AgeFemale$H76042_dp,AgeFemale$H76043_dp)
AgeFemale$H76044_dp<-sum(AgeFemale$H76044_dp,AgeFemale$H76045_dp)
AgeFemale$"X44"<-sum(AgeFemale$"X44",AgeFemale$"X45")
AgeFemale$"X46"<-sum(AgeFemale$"X46",AgeFemale$"X47",AgeFemale$"X48")
AgeFemale$"X56"<-sum(AgeFemale$"X56",AgeFemale$"X57")
AgeFemale$"X58"<-sum(AgeFemale$"X58",AgeFemale$"X59")
AgeFemale<-AgeFemale[,-c(1:346,351,353,354,363,365,370:722,727,729,730,739,741,746:755)]
AgeFemale<-AgeFemale[,-c(37:75,80,82:83,92,94,99:110)]

##GRAPHS
if(input$Area=="") {
plot.new()
legend("topleft",legend=c("Select a county, city, or place with the panel to the left"),cex=1.5,bty="n")
}

if(input$Area!="") {
#Graphs 1 and 2
agegroups<-c("0-4","5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
options(scipen = 999)
fill_color = rgb(0,.6,.9,alpha=.5)

barplot(as.matrix(AgeMale[37:54]),horiz=T,names=agegroups,space=0,xlim=c(max(as.matrix(AgeMale[19:36]))*2,0),las=1,col=fill_color,border=rgb(0,0,0,0))
  par(new=TRUE)
barplot(as.matrix(AgeMale[c(19:36)]),horiz=T,names=agegroups,space=0,xlim=c(max(as.matrix(AgeMale[c(19:36)]))*2,0),las=1,col=rgb(0,0,0,0))
  mtext(side=1,line=3,adj=.5,text=expression("Male"),font=.7,cex=1.5)

mtext(side=1,line=-55,at=0,text=paste(c("Population by Age and Sex, ", input$Area),collapse=""),font=1,cex=1.75)

legend(max(as.matrix(AgeMale[19:36]))*1.85, 18, legend=c("2010 Census Data with Differential Privacy","Published 2010 Census Data"), col=c(fill_color,rgb(0,1,1,0)),border=rgb(0,0,0,0), pt.cex=2, pch=15, cex=1.5, bty ="n", y.intersp=1.25)
legend(max(as.matrix(AgeMale[19:36]))*1.85, 18, legend=c("",""), col=c(fill_color, rgb(0,0,0)), pt.cex=2, pch=0, cex=1.5, bty ="n", y.intersp=1.25)

mtext(side=1,line=6,adj=0,text=paste(c("Source: U.S. Census Bureau's 2010 Demonstration Data (March 2022 release). Published 2010 Census data accessed via IPUMS NHGIS, University of Minnesota, July 2020."),collapse=""),font=.5,cex=1)

barplot(as.matrix(AgeFemale[37:54]),horiz=T,names.arg=array("",18),space=0,xlim=c(0,max(as.matrix(AgeMale[19:36]))*2),las=1,col=fill_color,border=rgb(0,0,0,0))
  par(new=TRUE)
barplot(as.matrix(AgeFemale[19:36]),horiz=T,names.arg=array("",18),space=0,xlim=c(0,max(as.matrix(AgeMale[19:36]))*2),las=1,col=rgb(0,0,0,0))
  mtext(side=1,line=3,adj=.5,text=expression("Female"),font=.7,cex=1.5)

##Graphs 3 and 4
Race<-subset(PopData, PopData$name==input$Area)

RaceSF<-c(Race$H7Z003_sf,Race$H7Z004_sf,Race$H7Z005_sf,Race$H7Z006_sf+Race$H7Z007_sf,Race$H7Z008_sf+Race$H7Z009_sf,Race$H7Z010_sf)
RaceDP<-c(Race$X68,Race$X69,Race$X70,Race$X71+Race$X72,Race$X73+Race$X74,Race$X75)
RaceTable<-cbind(RaceSF,RaceDP)
colors<-c(2,3,4,5,6,7)
colors<-adjustcolor(colors, alpha.f = 0.5)

barplot(RaceTable,col=colors,ylim=c(0,sum(RaceSF)*1.35),names.arg=c("Published 2010 Census Data","2010 Census Data with Differential Privacy"),axes=FALSE,cex.names=1.25)
mtext(side=1,line=-42,adj=.18,text=paste(c("Total population: ",sum(RaceSF)),collapse=""),font=.5,cex=1)
mtext(side=1,line=-42*(sum(RaceDP)/sum(RaceSF)),adj=.82,text=paste(c("Total population: ",sum(RaceDP)),collapse=""),font=.5,cex=1)

mtext(side=1,line=-50,at=2.5,text=paste(c("Population by Race and Hispanic Origin Groupings, ", input$Area),collapse=""),font=1,cex=1.75)
mtext(side=1,line=4,adj=0,text=paste(c("Source: U.S. Census Bureau's 2010 Demonstration Data (March 2022 release). Published 2010 Census data accessed via IPUMS NHGIS, University of Minnesota, July 2020."),collapse=""),font=.5,cex=1)

plot.new()
legend("left",legend=rev(c("White, not Hispanic","Black, not Hispanic","American Indian or Alaska Native, not Hispanic","Asian or Pacific Islander, not Hispanic","Two+ or Other, not Hispanic","Hispanic")),fill=rev(c(colors)),cex=1.5,bty="n")

}

},height=1500,width=1100)
		
}

shinyApp(ui = ui, server = server)


