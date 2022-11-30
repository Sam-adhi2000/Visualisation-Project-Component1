library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(gridExtra)
library(dashboardthemes)
library("viridis")
ui <- dashboardPage(
  dashboardHeader(title="Customer Personality Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About",tabName = "menu1"),
      menuItem("Univariate Plots",tabName = "menu2"),
      menuItem("Bivariate Plots",tabName = "menu3"),
      menuItem("Conclusion",tabName = "menu4")
    )
  ),
  dashboardBody(
    shinyDashboardThemes("blue_gradient"),
    tabItems(
      tabItem("menu1",h1("ABOUT"),fluidPage(
        tags$b(tags$h2("Abstract")),
        tags$ul(tags$h4("Customer Personality Analysis is a detailed analysis of a company’s ideal customers. It helps a business to better understand its customers and makes it easier for them to modify products according to the specific needs, behaviours and concerns of different types of customers. The individual traits of a potential customer influences their investment patterns and understanding these behavioural patterns helps a business to cater to its target demographic's specificities and better market its products to boost customer loyalty and conversion. The aim of this study is to find out how personal characteristics, different purchasables, success of company's promotions and site of purchase are related.")), 
        tags$b(tags$h2("Scope of study")),
        tags$ul(tags$h4("1) Products that are most invested in")),
        tags$ul(tags$h4("2) Recency of purchase")),
        tags$ul(tags$h4("3) Volume of complaints received in the last 2years")),
        tags$ul(tags$h4("4) Income-Expenditure distribution")),
        tags$ul(tags$h4("5) Education level wise income and spending")),
        tags$ul(tags$h4("6) Company promotion acceptance rate depending on education level")),
        tags$ul(tags$h4("7) Impact of site of purchase on revenue")),
        tags$b(tags$h2("Snippet of the dataset")),
        tableOutput("table"),
        tags$p("For the entire dataset ",tags$a(href="https://www.kaggle.com/datasets/imakash3011/customer-personality-analysis?datasetId=1546318","click here"))
      )),
      tabItem("menu2",h1("Univariate Plots"),fluidPage(
        tabsetPanel(
          tabPanel("Amount spent on different products",fluid=T,
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("bins",
                                   "Number of bins:",
                                   min = 1,
                                   max = 50,
                                   value = 30),
                       selectInput("column1","Choose the product",choices = c("Wine","Fruits","Meat Products","Fish Products","Sweet Products","Gold Products")),
                       actionButton("go1","Confirm")
                     ),
                     mainPanel(plotOutput("chart1"))
                   )
          ),
          tabPanel("Recency of purchase",fluid=T,
                   mainPanel(plotOutput("chart2"),align="center",width = 1000,textOutput("write"))
                  ),
          tabPanel("Volume of complaints",fluid=T,
                   mainPanel(plotOutput("chart3"),width=8,textOutput("write1"))
          ),
          tabPanel("Success of campaign",fluid=T,
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("column2","Choose the campaign number",choices = c("1","2","3","4","5")),
                       selectInput("column3","Choose the education level",choices = c("Graduation","PhD","Master","Basic","2n Cycle")),
                       actionButton("go2","Confirm")
                     ),
                     mainPanel(plotOutput("chart4"),textOutput("w1"))
                   )
          ),
          tabPanel("Number of purchases based on site of purchase",fluid=T,
                   mainPanel(plotOutput("chart5"))
          )
        )
      )),
      tabItem("menu3",h1("Bivariate Plots"),fluidPage(
        tabsetPanel(
          tabPanel("Income-Expenditure Distribution",fluid=T,
                   mainPanel(plotOutput("chart6"))
                   ),
          tabPanel("Income-Expenditure distribution based on education level",fluid=T,
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("column4","Choose the education level",choices = c("Graduation","PhD","Master","Basic","2n Cycle")),
                       actionButton("go3","Confirm")
                     ),
                     mainPanel(plotOutput("chart7"),textOutput("w2"))
                   )
          )
        )
      )),
      tabItem("menu4",h1("Conclusion"),fluidPage(
        tags$b(tags$h4("Insights gained from analysing the dataset:")),
        tags$ul(tags$h4("1) Wine was among the most invested products")),
        tags$ul(tags$h4("2) Not many complaints were lodged in the last 2 years which is a healthy indicator of increased customer satisfaction")),
        tags$ul(tags$h4("3) Promotional campaign number 2 did very poorly among the customers, irrespective of their education level")),
        tags$ul(tags$h4("4) Customers generally preferred in-store purchase over catalogue or web purchase")),
        tags$ul(tags$h4("5) Almost all customers spend a very little portion of their income")),
      ))
    )
  )
)
server <- function(input,output){
  data<-data.frame(read.csv("marketing_campaign.csv",sep="\t")) 
  observeEvent(input$go1,{
               if(input$column1=="Wine"){
                   output$chart1<-renderPlot({
                     ggplot(data,aes(MntWines))+
                     geom_histogram(bins =input$bins,color="blue",fill="steelblue")+
                     xlab("Product:Wine")+theme_classic()+
                     ggtitle("Plot representing amount spent on wine")
                   })
               }
                 else if(input$column1=="Fruits"){
                   output$chart1<-renderPlot({
                     ggplot(data,aes(MntFruits))+
                     geom_histogram(bins =input$bins,colour="darkgreen",fill="#ABDF99")+
                     xlab("Product:Fruits")+theme_classic()+
                     ggtitle("Plot representing amount spent on fruits")
                   })
                 }
                else if(input$column1=="Meat Products"){
                  output$chart1<-renderPlot({
                  ggplot(data,aes(MntMeatProducts))+
                  geom_histogram(bins =input$bins,colour="red",fill="#FFDFFF")+
                  xlab("Product:Meat")+theme_classic()+
                  ggtitle("Plot representing amount spent on meat products")
                  })
                }
              else if(input$column1=="Fish Products"){
                output$chart1<-renderPlot({
                ggplot(data,aes(MntFishProducts))+
                geom_histogram(bins =input$bins,colour="blue",fill="#EEAB66")+
                xlab("Product:Fish")+theme_classic()+
                ggtitle("Plot representing amount spent on fish products")
                })
              }
              else if(input$column1=="Sweet Products"){
                output$chart1<-renderPlot({
                ggplot(data,aes(MntSweetProducts))+
                geom_histogram(bins =input$bins,colour="blue",fill="#2EFA33")+
                xlab("Product:Sweet")+theme_classic()+
                ggtitle("Plot representing amount spent on sweet products")
                })
              }
             else if(input$column1=="Gold Products"){
                output$chart1<-renderPlot({
                ggplot(data,aes(MntGoldProds))+
                geom_histogram(bins =input$bins,colour="blue",fill="#FBAED6")+
                xlab("Product:Gold")+theme_classic()+
                ggtitle("Plot representing amount spent on gold products")
                })
            }
    
    
  }
               
  )
  output$chart2<-renderPlot({ggplot(data,aes(Recency))+
                            geom_bar(width=1,colour="green",fill="lightgreen")+
                            xlab("Recency of purchase")+theme_classic()+
    ggtitle(" Distribution of Recency of Customers' Purchase")})
  output$write<-renderText({
    print("The plot above gives a histogram indicating how frequently purchases are made at the store.")
  })
    output$chart3<-renderPlot({
      p<-ggplot(data,aes(as.character(Complain),fill=Complain))+
      geom_bar(colour="red",fill=c("pink","purple"),width=.1)+
      xlab("Response variable")+
        theme_classic()+
      geom_text(aes(label = ..count..), stat = "count",vjust=-.10)
    grid.arrange(p,top="Plot Representing Number of Cases of Non-complaints vs. Complaints \n in the Last 2 years")
    })
  output$write1<-renderText({
    print("The plot above represents the number of cases of Non-complaints vs. Complaints that have been lodged with the company in the last 2 years where 0 indicates that no complaints were made and 1 indicates that a complaint was lodged.")
  })
  observeEvent(input$go2,
               if(input$column2=="1"){
                 if(input$column3=="Graduation"){
                 output$chart4<-renderPlot({
                   q<-ggplot(data=subset(data,Education=="Graduation"),aes(x=as.character(AcceptedCmp1),y=..count..,fill=AcceptedCmp1))+
                     geom_bar(width=0.1)+xlab("Response variable")+
                     geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                     theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_line(colour = "black"))
                   grid.arrange(q,top="Plot Representing Acceptance of Offer in the First Campaign,\n Customer Education Level being 'Graduation'")
                 })
                 }
                 else if(input$column3=="PhD"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="PhD"),aes(x=as.character(AcceptedCmp1),y=..count..,fill=AcceptedCmp1))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the First Campaign,\n Customer Education Level being 'PhD'")
                   })
                 }
                 else if(input$column3=="Master"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Master"),aes(x=as.character(AcceptedCmp1),y=..count..,fill=AcceptedCmp1))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the First Campaign,\n Customer Education Level being 'Master'")
                   })
                 }
                 else if(input$column3=="Basic"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Basic"),aes(x=as.character(AcceptedCmp1),y=..count..,fill=AcceptedCmp1))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the First Campaign,\n Customer Education Level being 'Basic'")
                   })
                 }
                 else if(input$column3=="2n Cycle"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="2n Cycle"),aes(x=as.character(AcceptedCmp1),y=..count..,fill=AcceptedCmp1))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the First Campaign,\n Customer Education Level being '2n Cycle'")
                   })
                 }
               }
               else if(input$column2=="2"){
                 if(input$column3=="Graduation"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Graduation"),aes(x=as.character(AcceptedCmp2),y=..count..,fill=AcceptedCmp2))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(option = "D")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Second Campaign,\n Customer Education Level being 'Graduation'")
                   })
                 }
                 else if(input$column3=="PhD"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="PhD"),aes(x=as.character(AcceptedCmp2),y=..count..,fill=AcceptedCmp2))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(option = "D")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Second Campaign,\n Customer Education Level being 'PhD'")
                   })
                 }
                 else if(input$column3=="Master"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Master"),aes(x=as.character(AcceptedCmp2),y=..count..,fill=AcceptedCmp2))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(option = "D")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Second Campaign,\n Customer Education Level being 'Master'")
                   })
                 }
                 else if(input$column3=="Basic"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Basic"),aes(x=as.character(AcceptedCmp2),y=..count..,fill=AcceptedCmp2))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(end=0,option="D")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Second Campaign,\n Customer Education Level being 'Basic'")
                   })
                 }
                 else if(input$column3=="2n Cycle"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="2n Cycle"),aes(x=as.character(AcceptedCmp2),y=..count..,fill=AcceptedCmp2))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(option = "D")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Second Campaign,\n Customer Education Level being '2n Cycle'")
                   })
                 }
               }
               else if(input$column2=="3"){
                 if(input$column3=="Graduation"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Graduation"),aes(x=as.character(AcceptedCmp3),y=..count..,fill=AcceptedCmp3))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(begin=.5,end=.9,option="A")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Third Campaign,\n Customer Education Level being 'Graduation'")
                   })
                 }
                 else if(input$column3=="PhD"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="PhD"),aes(x=as.character(AcceptedCmp3),y=..count..,fill=AcceptedCmp3))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(begin=.5,end=.9,option="A")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Third Campaign,\n Customer Education Level being 'PhD'")
                   })
                 }
                 else if(input$column3=="Master"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Master"),aes(x=as.character(AcceptedCmp3),y=..count..,fill=AcceptedCmp3))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(begin=.5,end=.9,option="A")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Third Campaign,\n Customer Education Level being 'Master'")
                   })
                 }
                 else if(input$column3=="Basic"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Basic"),aes(x=as.character(AcceptedCmp3),y=..count..,fill=AcceptedCmp3))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(begin=.5,end=.9,option="A")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Third Campaign,\n Customer Education Level being 'Basic'")
                   })
                 }
                 else if(input$column3=="2n Cycle"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="2n Cycle"),aes(x=as.character(AcceptedCmp3),y=..count..,fill=AcceptedCmp3))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(begin=.5,end=.9,option="A")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Third Campaign,\n Customer Education Level being '2n Cycle'")
                   })
                 }
               }
               else if(input$column2=="4"){
                 if(input$column3=="Graduation"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Graduation"),aes(x=as.character(AcceptedCmp4),y=..count..,fill=AcceptedCmp4))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(end=.8,option="C")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Fourth Campaign,\n Customer Education Level being 'Graduation'")
                   })
                 }
                 else if(input$column3=="PhD"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="PhD"),aes(x=as.character(AcceptedCmp4),y=..count..,fill=AcceptedCmp4))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(end=.8,option="C")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Fourth Campaign,\n Customer Education Level being 'PhD'")
                   })
                 }
                 else if(input$column3=="Master"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Master"),aes(x=as.character(AcceptedCmp4),y=..count..,fill=AcceptedCmp4))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(end=.8,option="C")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Fourth Campaign,\n Customer Education Level being 'Master'")
                   })
                 }
                 else if(input$column3=="Basic"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Basic"),aes(x=as.character(AcceptedCmp4),y=..count..,fill=AcceptedCmp4))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(end=0,option="C")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Fourth Campaign,\n Customer Education Level being 'Basic'")
                   })
                 }
                 else if(input$column3=="2n Cycle"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="2n Cycle"),aes(x=as.character(AcceptedCmp4),y=..count..,fill=AcceptedCmp4))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(end=.8,option="C")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Fourth Campaign,\n Customer Education Level being '2n Cycle'")
                   })
                 }
               }
               else if(input$column2=="5"){
                 if(input$column3=="Graduation"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Graduation"),aes(x=as.character(AcceptedCmp5),y=..count..,fill=AcceptedCmp5))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(begin = 0.2,end=.5,option = "H")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Fifth Campaign,\n Customer Education Level being 'Graduation'")
                   })
                 }
                 else if(input$column3=="PhD"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="PhD"),aes(x=as.character(AcceptedCmp5),y=..count..,fill=AcceptedCmp5))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(begin = 0.2,end=.5,option = "H")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Fifth Campaign,\n Customer Education Level being 'PhD'")
                   })
                 }
                 else if(input$column3=="Master"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Master"),aes(x=as.character(AcceptedCmp5),y=..count..,fill=AcceptedCmp5))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(begin = 0.2,end=.5,option = "H")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Fifth Campaign,\n Customer Education Level being 'Master'")
                   })
                 }
                 else if(input$column3=="Basic"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="Basic"),aes(x=as.character(AcceptedCmp5),y=..count..,fill=AcceptedCmp5))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(begin = 0.2,end=.2,option = "H")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Fifth Campaign,\n Customer Education Level being 'Basic'")
                   })
                 }
                 else if(input$column3=="2n Cycle"){
                   output$chart4<-renderPlot({
                     q<-ggplot(data=subset(data,Education=="2n Cycle"),aes(x=as.character(AcceptedCmp5),y=..count..,fill=AcceptedCmp5))+
                       geom_bar(width=0.1)+xlab("Response variable")+
                       geom_text(aes(label = ..count..), stat = "count",vjust=-.10)+
                       theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                       scale_fill_viridis(begin = 0.2,end=.5,option = "H")
                     grid.arrange(q,top="Plot Representing Acceptance of Offer in the Fifth Campaign,\n Customer Education Level being '2n Cycle'")
                   })
                 }
               })
  output$w1<-renderText({
    print("The plot above represents the number of cases of acceptance of offer in the respective campaign vs. non-acceptance where 0 indicates rejection of the offer and 1 indicates acceptance.")
  })
               output$chart5<-renderPlot({
                 df<-data.frame(c(data[,"NumWebPurchases"],data[,"NumCatalogPurchases"],data[,"NumStorePurchases"]), c(rep("Web Purchases",nrow(data)),rep("Catalogue Purchases",nrow(data)),rep("Store Purchases",nrow(data))))
                 colnames(df) <- c("A","B")
                 ggplot(df, aes(B, A, fill = B)) + geom_boxplot() + scale_fill_discrete("LEGEND")+
                   xlab("Site of Purchase")+
                   ylab("Number of Purchases")+theme_classic()+
                   ggtitle("Boxplot Representing Number of Purchases across Different Sites of Purchase")
               })
               output$chart6<-renderPlot({
                 data$Expenditure<-rowSums(data[,c(10:15)])
                 options(scipen=10)
                 ggplot(data=subset(data,!is.na(Income)))+
                   geom_histogram(aes(x=Expenditure,color="blue",fill='Expenditure'),color="darkblue",bins=20, alpha=I(0.75))+
                   geom_histogram(aes(x=Income,color="red",fill='Income'),color="darkblue",bins=40,alpha=I(0.5))+ geom_freqpoly(aes(Income),bins=30,color="black")+
                   scale_fill_manual(values=c("blue","orange"),'Legend')+
                   ggtitle("Customers' Income-Expenditure Distribution")+
                   xlab("Amount")+theme_classic()
               })
               observeEvent(input$go3,
                            if(input$column4=="Graduation"){
                              output$chart7<-renderPlot({
                                data$Expenditure<-rowSums(data[,c(10:15)])
                                options(scipen=10)
                                ggplot(data=subset(data,Education=="Graduation"))+
                                  geom_histogram(aes(x=Expenditure,color="blue",fill='Expenditure'),color="darkblue",bins=20, alpha=I(0.75))+
                                  geom_histogram(aes(x=Income,color="red",fill='Income'),color="darkblue",bins=30,alpha=I(0.5))+
                                  scale_fill_manual(values=c("lightgreen","purple"),'Legend')+theme_classic()+
                                  ggtitle("Income and Expenditure of Customers with Education Level- 'Graduation' ")+
                                  xlab("Amount")
                              })
                              output$w2<-renderText({
                                print("The income range is approximately between Rs.0 and Rs.100000")
                              })
                            }
                            else if(input$column4=="PhD"){
                              output$chart7<-renderPlot({
                                data$Expenditure<-rowSums(data[,c(10:15)])
                                options(scipen=10)
                                ggplot(data=subset(data,Education=="PhD"))+
                                  geom_histogram(aes(x=Expenditure,color="blue",fill='Expenditure'),color="darkblue",bins=20, alpha=I(0.75))+
                                  geom_histogram(aes(x=Income,color="red",fill='Income'),color="darkblue",bins=30,alpha=I(0.5))+
                                  scale_fill_manual(values=c("yellow","#F44447"),'Legend')+theme_classic()+
                                  ggtitle("Income and Expenditure of Customers with Education Level- 'PhD' ")+
                                  xlab("Amount")
                              })
                                output$w2<-renderText({
                                  print("The income range is approximately between Rs.30000 to Rs.80000")
                                })
                             }
                            else if(input$column4=="Master"){
                              output$chart7<-renderPlot({
                                data$Expenditure<-rowSums(data[,c(10:15)])
                                options(scipen=10)
                                ggplot(data=subset(data,Education=="Master"))+
                                  geom_histogram(aes(x=Expenditure,color="blue",fill='Expenditure'),color="darkblue",bins=20, alpha=I(0.75))+
                                  geom_histogram(aes(x=Income,color="red",fill='Income'),color="darkblue",bins=30,alpha=I(0.5))+
                                  scale_fill_manual(values=c("#AB2735","#FFF467"),'Legend')+theme_classic()+
                                  ggtitle("Income and Expenditure of Customers with Education Level- 'Master' ")+
                                  xlab("Amount")
                              })
                                output$w2<-renderText({
                                  print("The income range is approximately between Rs.20000 to Rs.90000")
                                })
                            }
                            else if(input$column4=="Basic"){
                              output$chart7<-renderPlot({
                                data$Expenditure<-rowSums(data[,c(10:15)])
                                options(scipen=10)
                                ggplot(data=subset(data,Education=="Basic"))+
                                  geom_histogram(aes(x=Expenditure,color="blue",fill='Expenditure'),color="darkblue",bins=20, alpha=I(0.75))+
                                  geom_histogram(aes(x=Income,color="red",fill='Income'),color="darkblue",bins=30,alpha=I(0.5))+
                                  scale_fill_manual(values=c("darkblue","#129384"),'Legend')+theme_classic()+
                                  ggtitle("Income and Expenditure of Customers with Education Level- 'Basic' ")+
                                  xlab("Amount")
                              })
                                output$w2<-renderText({
                                  print("The income range is approximately between Rs.15000 to Rs.28000")
                                })
                            }
                            else if(input$column4=="2n Cycle"){
                              output$chart7<-renderPlot({
                                data$Expenditure<-rowSums(data[,c(10:15)])
                                options(scipen=10)
                                ggplot(data=subset(data,Education=="2n Cycle"))+
                                  geom_histogram(aes(x=Expenditure,color="blue",fill='Expenditure'),color="darkblue",bins=20, alpha=I(0.75))+
                                  geom_histogram(aes(x=Income,color="red",fill='Income'),color="darkblue",bins=30,alpha=I(0.5))+
                                  scale_fill_manual(values=c("#AFBDFD","red"),'Legend')+theme_classic()+
                                  ggtitle("Income and Expenditure of Customers with Education Level- '2n Cycle' ")+
                                  xlab("Amount")
                              })
                                output$w2<-renderText({
                                  print("The income range is approximately between Rs.20000 to Rs.77000")
                                })
                            }
                            
                            )
                 
  output$table<-renderTable({head(data)})
}
shinyApp(ui,server)

