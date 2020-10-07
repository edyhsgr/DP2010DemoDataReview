##########
#US CENSUS BUREAU PRIVACY PROTECTED 2010 DEMONSTRATION DATA FOR AMERICAN INDIAN, ALASKA NATIVE, AND NATIVE HAWAIIAN AREAS
#
#DATA DOWNLOADED JULY 16, 2020 FROM IPUMS NHGIS, UNIVERSITY OF MINNESOTA: https://www.nhgis.org/privacy-protected-demonstration-data
#THIS IS FOLLOWING https://shiny.demog.berkeley.edu/eddieh/DP2010DemoData_CA_Jul2020/
#
#EDDIE HUNSINGER, JULY 2020
#https://edyhsgr.github.io/
#
#THERE IS NO WARRANTY FOR THIS CODE
#THIS CODE HAS NOT BEEN TESTED AT ALL-- PLEASE LET ME KNOW IF YOU FIND ANY PROBLEMS (edyhsgr@gmail.com)
##########

library(shiny)

Data<-read.table(file="https://raw.githubusercontent.com/edyhsgr/DP2010DemoDataReview/master/nhgis_ppdd_20200527_aianhh.csv",header=TRUE,quote = "",sep=",")
Names<-Data$name

ui<-fluidPage(

	tags$h3("U.S. Census Bureau's 2010 Demonstration Data (July 2020 release): Population by Demographic Characteristics, American Indian, and Alaska Native, Native Hawaiian (AIANNH) Areas"),
	p("U.S. Census Bureau data downloaded July 16, 2020 from ",
	tags$a(href="https://www.nhgis.org/privacy-protected-demonstration-data", "IPUMS NHGIS, University of Minnesota.")),
	p("Data viewer following ",
	tags$a(href="https://shiny.demog.berkeley.edu/eddieh/DP2010DemoData_CA_Jul2020/", "format here."),
	"",
	"",
	tags$a(href="", 
	"")
),
  
hr(),

sidebarLayout(
sidebarPanel(

selectizeInput(inputId = "Area", label = "AIANNH Area", 
choices = Names,
options = list(placeholder = "Type in an area to see graphs", multiple = TRUE, maxOptions = 5000, onInitialize = I('function() { this.setValue(""); }'))
),

hr(),

p("This interface was made with ",
tags$a(href="https://shiny.rstudio.com/", 
	"Shiny for R."),
tags$a(href="https://github.com/edyhsgr/DP2010DemoDataReview", 
	"Related GitHub repository."),
"July 2020."),

width=3
),

mainPanel(
	
	plotOutput("plots"),width=3)
)
)

PopData<-Data #read.table(file="https://raw.githubusercontent.com/edyhsgr/DP2010DemoDataReview/master/nhgis_ppdd_20200527_aianhh.csv",header=TRUE,quote = "",sep=",")

server<-function(input, output) {	
	output$plots<-renderPlot({
par(mfrow=c(2,2)) #,mai=c(0.5,0.5,0.5,0.5))

AgeMale<-subset(PopData, PopData$name==input$Area)
AgeMale$H76006_sf<-sum(AgeMale$H76006_sf,AgeMale$H76007_sf)
AgeMale$H76008_sf<-sum(AgeMale$H76008_sf,AgeMale$H76009_sf,AgeMale$H76010_sf)
AgeMale$H76018_sf<-sum(AgeMale$H76018_sf,AgeMale$H76019_sf)
AgeMale$H76020_sf<-sum(AgeMale$H76020_sf,AgeMale$H76021_sf)
AgeMale$H76006_dp<-sum(AgeMale$H76006_dp,AgeMale$H76007_dp)
AgeMale$H76008_dp<-sum(AgeMale$H76008_dp,AgeMale$H76009_dp,AgeMale$H76010_sf)
AgeMale$H76018_dp<-sum(AgeMale$H76018_dp,AgeMale$H76019_dp)
AgeMale$H76020_dp<-sum(AgeMale$H76020_dp,AgeMale$H76021_dp)
AgeMale<-AgeMale[,-c(1:321,326,328,339,338,340,345:697,702,704,705,714,716,721:754)]

AgeFemale<-subset(PopData, PopData$name==input$Area)
AgeFemale$H76030_sf<-sum(AgeFemale$H76030_sf,AgeFemale$H76031_sf)
AgeFemale$H76032_sf<-sum(AgeFemale$H76032_sf,AgeFemale$H76033_sf,AgeFemale$H76034_sf)
AgeFemale$H76042_sf<-sum(AgeFemale$H76042_sf,AgeFemale$H76043_sf)
AgeFemale$H76044_sf<-sum(AgeFemale$H76044_sf,AgeFemale$H76045_sf)
AgeFemale$H76030_dp<-sum(AgeFemale$H76030_dp,AgeFemale$H76031_dp)
AgeFemale$H76032_dp<-sum(AgeFemale$H76032_dp,AgeFemale$H76033_dp,AgeFemale$H76034_sf)
AgeFemale$H76042_dp<-sum(AgeFemale$H76042_dp,AgeFemale$H76043_dp)
AgeFemale$H76044_dp<-sum(AgeFemale$H76044_dp,AgeFemale$H76045_dp)
AgeFemale<-AgeFemale[,-c(1:345,350,352,353,362,364,369:721,726,728,729,738,740,745:754)]

##GRAPHS
if(input$Area=="") {
plot.new()
legend("topleft",legend=c("Select an area with the panel to the left"),cex=1.5,bty="n")
}

if(input$Area!="") {
#Graphs 1 and 2
agegroups<-c("0-4","5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
options(scipen = 999)
fill_color = rgb(0,.6,.9,alpha=.5)

barplot(as.matrix(AgeMale[1:18]),horiz=T,names=agegroups,space=0,xlim=c(max(as.matrix(AgeMale[19:36]))*2,0),las=1,col=fill_color,border=rgb(0,0,0,0))
  par(new=TRUE)
barplot(as.matrix(AgeMale[19:36]),horiz=T,names=agegroups,space=0,xlim=c(max(as.matrix(AgeMale[19:36]))*2,0),las=1,col=rgb(0,0,0,0))
  mtext(side=1,line=3,adj=.5,text=expression("Male"),font=.7,cex=1.5)

mtext(side=1,line=-55,at=0,text=paste(c("Population by Age and Sex, ", input$Area),collapse=""),font=1,cex=1.75)

legend(max(as.matrix(AgeMale[19:36]))*1.85, 18, legend=c("2010 Census Data with Differential Privacy","Published 2010 Census Data"), col=c(fill_color,rgb(0,1,1,0)),border=rgb(0,0,0,0), pt.cex=2, pch=15, cex=1.5, bty ="n", y.intersp=1.25)
legend(max(as.matrix(AgeMale[19:36]))*1.85, 18, legend=c("",""), col=c(fill_color, rgb(0,0,0)), pt.cex=2, pch=0, cex=1.5, bty ="n", y.intersp=1.25)

mtext(side=1,line=6,adj=0,text=paste(c("Source: U.S. Census Bureau's 2010 Demonstration Data (July 2020 release). Accessed via IPUMS NHGIS, University of Minnesota, July 2020."),collapse=""),font=.5,cex=1)

barplot(as.matrix(AgeFemale[1:18]),horiz=T,names.arg=array("",18),space=0,xlim=c(0,max(as.matrix(AgeMale[19:36]))*2),las=1,col=fill_color,border=rgb(0,0,0,0))
  par(new=TRUE)
barplot(as.matrix(AgeFemale[19:36]),horiz=T,names.arg=array("",18),space=0,xlim=c(0,max(as.matrix(AgeMale[19:36]))*2),las=1,col=rgb(0,0,0,0))
  mtext(side=1,line=3,adj=.5,text=expression("Female"),font=.7,cex=1.5)

##Graphs 3 and 4
Race<-subset(PopData, PopData$name==input$Area)

RaceSF<-c(Race$H7Z003_sf,Race$H7Z004_sf,Race$H7Z005_sf,Race$H7Z006_sf+Race$H7Z007_sf,Race$H7Z008_sf+Race$H7Z009_sf,Race$H7Z010_sf)
RaceDP<-c(Race$H7Z003_dp,Race$H7Z004_dp,Race$H7Z005_dp,Race$H7Z006_dp+Race$H7Z007_dp,Race$H7Z008_dp+Race$H7Z009_dp,Race$H7Z010_dp)
RaceTable<-cbind(RaceSF,RaceDP)
colors<-c(2,3,4,5,6,7)
colors<-adjustcolor(colors, alpha.f = 0.5)

barplot(RaceTable,col=colors,ylim=c(0,sum(RaceSF)*1.35),names.arg=c("Published 2010 Census Data","2010 Census Data with Differential Privacy"),axes=FALSE,cex.names=1.25)
mtext(side=1,line=-42,adj=.18,text=paste(c("Total population: ",sum(RaceSF)),collapse=""),font=.5,cex=1)
mtext(side=1,line=-42*(sum(RaceDP)/sum(RaceSF)),adj=.82,text=paste(c("Total population: ",sum(RaceDP)),collapse=""),font=.5,cex=1)

mtext(side=1,line=-50,at=2.5,text=paste(c("Population by Race and Hispanic Origin Groupings, ", input$Area),collapse=""),font=1,cex=1.75)

mtext(side=1,line=4,adj=0,text=paste(c("Source: U.S. Census Bureau's 2010 Demonstration Data (July 2020 release). Accessed via IPUMS NHGIS, University of Minnesota, July 2020."),collapse=""),font=.5,cex=1)

plot.new()
legend("left",legend=rev(c("White, not Hispanic","Black, not Hispanic","American Indian or Alaska Native, not Hispanic","Asian or Pacific Islander, not Hispanic","Two+ or Other, not Hispanic","Hispanic")),fill=rev(c(colors)),cex=1.5,bty="n")

}

},height=1500,width=1100)
		
}

shinyApp(ui = ui, server = server)
