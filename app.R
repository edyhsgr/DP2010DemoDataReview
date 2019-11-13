##########
#US CENSUS BUREAU DIFFERENTIAL PRIVACY 2010 DEMONSTRATION DATA FOR CALIFORNIA
#
#DATA DOWNLOADED NOVEMBER 8, 2019 FROM IPUMS NHGIS, UNIVERSITY OF MINNESOTA: https://www.nhgis.org/differentially-private-2010-census-data
#
#EDDIE HUNSINGER, NOVEMBER 2019
#https://edyhsgr.github.io/eddieh/
#
#THERE IS NO WARRANTY FOR THIS CODE
#THIS CODE HAS NOT BEEN TESTED AT ALL-- PLEASE LET ME KNOW IF YOU FIND ANY PROBLEMS (edyhsgr@gmail.com)
##########

library(shiny)

Names<-read.table(file="https://raw.githubusercontent.com/edyhsgr/DP2010DemoDataReview_CA/master/PlaceNames_DP2010DemonstrationProducts_CA.csv",header=FALSE,sep=",")

ui<-fluidPage(

	tags$h3("Review of U.S. Census Bureau's 2010 Demonstration Data Products, Populations by Age and Sex, California Cities and Places"),
	p("U.S. Census Bureau data downloaded November 8, 2019 from ",
	tags$a(href="https://www.nhgis.org/differentially-private-2010-census-data", "IPUMS NHGIS, University of Minnesota."),
	"",
	tags$a(href="", 
	"")
),
  
hr(),

sidebarLayout(
sidebarPanel(

selectizeInput(inputId = "Area", label = "City or census designated place (CDP)", 
choices = Names[,],
options = list(placeholder = "Type city or place name", maxOptions = 5000)
),

hr(),

p("This interface was made with ",
tags$a(href="https://shiny.rstudio.com/", 
	"Shiny for R."),
tags$a(href="https://github.com/edyhsgr/DP2010DemoDataReview_CA", 
	"Related GitHub repository."),
"November 2019."),

width=3
),

mainPanel(
	
	plotOutput("plots"),width=3)
)
)

server<-function(input, output) {	
	output$plots<-renderPlot({
par(mfrow=c(1,2)) #,mai=c(0.5,0.5,0.5,0.5))

#HousingData<-read.table(file="https://raw.githubusercontent.com/edyhsgr/DP2010DemoDataReview_CA/master/long_dp14_160_CA.csv",header=TRUE,sep=",")
PopData<-read.table(file="https://raw.githubusercontent.com/edyhsgr/DP2010DemoDataReview_CA/master/long_dp1_160_CA.csv",header=TRUE,sep=",")

AgeMale<-subset(PopData, PopData$name_sf==input$Area & 
(PopData$var_code=="H76003" | 
PopData$var_code=="H76004" |
PopData$var_code=="H76005" |
PopData$var_code=="H76006" |
PopData$var_code=="H76007" |
PopData$var_code=="H76008" |
PopData$var_code=="H76009" |
PopData$var_code=="H76010" |
PopData$var_code=="H76011" |
PopData$var_code=="H76012" |
PopData$var_code=="H76013" |
PopData$var_code=="H76014" |
PopData$var_code=="H76015" |
PopData$var_code=="H76016" |
PopData$var_code=="H76017" |
PopData$var_code=="H76018" |
PopData$var_code=="H76019" |
PopData$var_code=="H76020" |
PopData$var_code=="H76021" |
PopData$var_code=="H76022" |
PopData$var_code=="H76023" |
PopData$var_code=="H76024" |
PopData$var_code=="H76025")
)
AgeMale$sf[AgeMale$var_code=="H76006"]<-sum(AgeMale$sf[AgeMale$var_code=="H76006"],AgeMale$sf[AgeMale$var_code=="H76007"])
AgeMale$sf[AgeMale$var_code=="H76008"]<-sum(AgeMale$sf[AgeMale$var_code=="H76008"],AgeMale$sf[AgeMale$var_code=="H76009"],AgeMale$sf[AgeMale$var_code=="H76010"])
AgeMale$sf[AgeMale$var_code=="H76018"]<-sum(AgeMale$sf[AgeMale$var_code=="H76018"],AgeMale$sf[AgeMale$var_code=="H76019"])
AgeMale$sf[AgeMale$var_code=="H76020"]<-sum(AgeMale$sf[AgeMale$var_code=="H76020"],AgeMale$sf[AgeMale$var_code=="H76021"])
AgeMale$dp[AgeMale$var_code=="H76006"]<-sum(AgeMale$dp[AgeMale$var_code=="H76006"],AgeMale$dp[AgeMale$var_code=="H76007"])
AgeMale$dp[AgeMale$var_code=="H76008"]<-sum(AgeMale$dp[AgeMale$var_code=="H76008"],AgeMale$dp[AgeMale$var_code=="H76009"],AgeMale$dp[AgeMale$var_code=="H76010"])
AgeMale$dp[AgeMale$var_code=="H76018"]<-sum(AgeMale$dp[AgeMale$var_code=="H76018"],AgeMale$dp[AgeMale$var_code=="H76019"])
AgeMale$dp[AgeMale$var_code=="H76020"]<-sum(AgeMale$dp[AgeMale$var_code=="H76020"],AgeMale$dp[AgeMale$var_code=="H76021"])
AgeMale<-AgeMale[-c(5,7,8,17,19),]

AgeFemale<-subset(PopData, PopData$name_sf==input$Area & 
(PopData$var_code=="H76027" | 
PopData$var_code=="H76028" |
PopData$var_code=="H76029" |
PopData$var_code=="H76030" |
PopData$var_code=="H76031" |
PopData$var_code=="H76032" |
PopData$var_code=="H76033" |
PopData$var_code=="H76034" |
PopData$var_code=="H76035" |
PopData$var_code=="H76036" |
PopData$var_code=="H76037" |
PopData$var_code=="H76038" |
PopData$var_code=="H76039" |
PopData$var_code=="H76040" |
PopData$var_code=="H76041" |
PopData$var_code=="H76042" |
PopData$var_code=="H76043" |
PopData$var_code=="H76044" |
PopData$var_code=="H76045" |
PopData$var_code=="H76046" |
PopData$var_code=="H76047" |
PopData$var_code=="H76048" |
PopData$var_code=="H76049")
)
AgeFemale$sf[AgeFemale$var_code=="H76030"]<-sum(AgeFemale$sf[AgeFemale$var_code=="H76030"],AgeFemale$sf[AgeFemale$var_code=="H76031"])
AgeFemale$sf[AgeFemale$var_code=="H76032"]<-sum(AgeFemale$sf[AgeFemale$var_code=="H76032"],AgeFemale$sf[AgeFemale$var_code=="H76033"],AgeFemale$sf[AgeFemale$var_code=="H76034"])
AgeFemale$sf[AgeFemale$var_code=="H76042"]<-sum(AgeFemale$sf[AgeFemale$var_code=="H76042"],AgeFemale$sf[AgeFemale$var_code=="H76043"])
AgeFemale$sf[AgeFemale$var_code=="H76044"]<-sum(AgeFemale$sf[AgeFemale$var_code=="H76044"],AgeFemale$sf[AgeFemale$var_code=="H76045"])
AgeFemale$dp[AgeFemale$var_code=="H76030"]<-sum(AgeFemale$dp[AgeFemale$var_code=="H76030"],AgeFemale$dp[AgeFemale$var_code=="H76031"])
AgeFemale$dp[AgeFemale$var_code=="H76032"]<-sum(AgeFemale$dp[AgeFemale$var_code=="H76032"],AgeFemale$dp[AgeFemale$var_code=="H76033"],AgeFemale$dp[AgeFemale$var_code=="H76034"])
AgeFemale$dp[AgeFemale$var_code=="H76042"]<-sum(AgeFemale$dp[AgeFemale$var_code=="H76042"],AgeFemale$dp[AgeFemale$var_code=="H76043"])
AgeFemale$dp[AgeFemale$var_code=="H76044"]<-sum(AgeFemale$dp[AgeFemale$var_code=="H76044"],AgeFemale$dp[AgeFemale$var_code=="H76045"])
AgeFemale<-AgeFemale[-c(5,7,8,17,19),]

#GRAPH
agegroups<-c("0-4","5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

options(scipen = 999)
	  
fill_color = rgb(0,.6,.9,alpha=1)

if(input$Area!="") {
barplot(AgeMale$dp,horiz=T,names=agegroups,space=0,xlim=c(max(AgeMale$sf)*2,0),las=1,col=fill_color,border=fill_color)
  par(new=TRUE)
barplot(AgeMale$sf,horiz=T,names=agegroups,space=0,xlim=c(max(AgeMale$sf)*2,0),las=1,col=rgb(0,0,0,0))
  mtext(side=1,line=3,adj=.5,text=expression("Male"),font=1,cex=1.5)

legend(max(AgeMale$sf)*1.85, 18, legend=c("2010 Data with Differential Privacy","Published 2010 Data"), col=c(fill_color,rgb(0,1,1,0)), pt.cex=2, pch=15, cex=1.5, bty ="n", y.intersp=1.25)
legend(max(AgeMale$sf)*1.85, 18, legend=c("",""), col=c(fill_color, rgb(0,0,0)), pt.cex=2, pch=0, cex=1.5, bty ="n", y.intersp=1.25)


barplot(AgeFemale$dp,horiz=T,names=FALSE,space=0,xlim=c(0,max(AgeMale$sf)*2),las=1,col=fill_color,border=fill_color)
  par(new=TRUE)
barplot(AgeFemale$sf,horiz=T,names=FALSE,space=0,xlim=c(0,max(AgeMale$sf)*2),las=1,col=rgb(0,0,0,0))
  mtext(side=1,line=3,adj=.5,text=expression("Female"),font=1,cex=1.5)

}

},height=900,width=1200)
		
}

shinyApp(ui = ui, server = server)

