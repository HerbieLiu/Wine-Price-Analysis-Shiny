#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
require(shiny)
library(sf)
require(visNetwork)
require(leaflet)
library(shiny)
library(shinydashboard)
library(plotly)
library(visNetwork)
library(shinythemes)

shinyUI(
  dashboardPage(
   # theme = shinytheme("cerulean"),
    dashboardHeader( title = shiny::span(
      "Wine Pricing",
      style = "font-family:Georgia;color:black;font-style:italic",
      div(img(src = "wine.jpeg"), style="text-align:center;")
    ),
    titleWidth = "260px"),
   skin = "black",
    dashboardSidebar(
      width = 220,
      sidebarMenu(
        id = "tabs",
        menuItem("So...", tabName = "wineimage", icon = icon("leaf")),
        menuItem("Wine Pricing by Region", tabName = "welcome", icon = icon("leaf")),
        #menuItem("Wine Rating", tabName = "wineRegions", icon = icon("leaf")),
        menuItem(
          "Price through Variety",
          icon = icon("leaf"),
          menuSubItem(
            "Flavor",
            tabName = "Flavor",
            icon = icon("angle-right")
          ),
          menuSubItem("Age", tabName = "Age", icon = icon("angle-right"))),
        menuItem("Price through Grading", tabName = "wineGrading", icon = icon("leaf")),
        menuItem("Insight",tabName = "wineInsight",icon = icon("book")),
        menuItem("Last but not least...",tabName = "Cheers",icon = icon("leaf"))
        )),

dashboardBody(
  tags$head(
    tags$style(HTML("
                    .content-wrapper {
                    background-color: white !important;
                    }
                    .main-sidebar {
                    background-color: white !important;
                    }
                    "))),
  tabItems(
    tabItem(tabName ="wineimage",
            fluidPage(
              fluidRow(
                column(12, align="center",
                       h1(textOutput("welcome1")),
                       tags$style(type = "text/css", '#welcome1{
                                  font-size: 25px;
                                  font-style:italic;
                                  font-family:Georgia;
                                  margin-bottom: 30px;
                                  }'))),
              fluidRow(
                style='height:100vh',
                br(),br(),
                column(6,align="center",imageOutput("wine1")),
                column(6, align = "center",imageOutput("wine2")))
              
            )),
    tabItem(tabName ="welcome",
            fluidPage(
              fluidRow(
                tabBox(
                width = 12,
              tabPanel(title = "World Map",
                  fluidRow(
                    style='height:100vh',
                    column(width = 12, leafletOutput("leafletworldmap")),
                    column(3),
                    column(6, br(),br(),align = "center", 
                           textOutput("insight1"),
                           tags$style(type = "text/css", '#insight1{
                                                        font-size: 20px;
                                      margin-bottom: 30px;
                                      }')),
                    column(3)
                    )),
                
              tabPanel(title = "US Map",
                fluidRow(
                  style='height:120vh',
                  column(12, align = "center", h3(textOutput("usmap1"),
                     tags$style(type = "text/css", '#usmap1{
                                                        font-size: 25px;
                                font-style:italic;
                                font-family:Georgia;
                                margin-bottom: 30px;
                                }'))),
                  column(width = 12, plotlyOutput("usmap", height = 420)),
                  column(width = 4, 
                         br(),
                         tableOutput("table1"), align = "right",
                         br(),
                         tableOutput("table3"),align = "right"),
                  column(width = 7, plotlyOutput("table2"), align = "center")))
                  #column(width = 12, plotOutput("plot"))
                ))
              )),
    tabItem(tabName = "wineRegions",
            fluidRow(
              column(12, align="center",
                     h3(textOutput("welcome3")),
                     tags$style(type = "text/css", '#welcome3{
                                                        font-size: 25px;
                                font-style:italic;
                                font-family:Georgia;
                                margin-bottom: 30px;
                                }'),
                     column(width = 12,plotlyOutput("boxplot", height = 480, width = 1000),align = "center")
              ))),
    
    tabItem(tabName = "Flavor",
            fluidRow(
              tabBox(
                width = 12,
                tabPanel(title = "Variety Median",
                         fluidRow(
                           style='height:100vh',
                           column(9,align = "center",
                                  plotlyOutput("varietymedium",height = 500)),
                           column(3, align = "center",
                                  br(),br(),br(),
                                  h5(textOutput("variety3")),
                                  tags$head(tags$style("#variety3{
                                  font-size: 18px }")), 
                                  hr(),
                                  h5(textOutput("variety4")),
                                  tags$head(tags$style("#variety4{
                                  font-size: 18px
                                 }")),
                                  hr(),
                                  h5(textOutput("variety8")),
                                  tags$head(tags$style("#variety8{
                                  font-size: 18px}")))
                                  )),
                tabPanel(title = "Flavor by Variety",
                         fluidRow(
                           style='height:120vh',
                           column(width = 3,
                                  align = "center",
                                  selectInput("Variety", label = "Type of Variety", selected = "Pinot Noir", 
                                              choices = c("Pinot Noir", "Chardonny","Cabernet_Sauvignon",
                                                          "Merlot","Riesling","Rose","Sangiovese",
                                                          "Syrah","Sauvignon_Blanc","Zinfandel")),
                                  submitButton("Drink!", icon = icon("leaf"),width = "50%")),
                           br(),br(),br(),
                                  width = 7,imageOutput("wordcloud"),align = "center")
                          ),
                
               tabPanel(title = "Variety Visual Network",
                        fluidRow(
                          style='height:110vh',
                          column(8,br(),align = "left",
                                 visNetworkOutput("network")),
                          br(),br(),br(),br(),
                          column(3,align = "left",
                                 h5(textOutput("varietynetwork1")),
                                 h5(textOutput("varietynetwork2")),
                                 h5(textOutput("varietynetwork3")),
                                 tags$head(tags$style("#varietynetwork1{
                                  font-size: 16px }")),
                                 tags$head(tags$style("#varietynetwork2{
                                  font-size: 16px }")),
                                 tags$head(tags$style("#varietynetwork3{
                                  font-size: 16px }"))),
                          column(1),
                          column(1),
                          column(10, br(),br(),br(),br(),align = "center",
                                 h5(textOutput("variety1"),
                                    tags$head(tags$style("#variety1{
                                  font-size: 16px }")),
                                  hr(),
                                    textOutput("variety2"),
                                  tags$head(tags$style("#variety2{
                                  font-size: 16px }")),
                                  hr(),
                                    textOutput("variety9"),
                                  tags$head(tags$style("#variety9{
                                  font-size: 16px }")),
                                  hr(),
                                    textOutput("variety10"),
                                  tags$head(tags$style("#variety10{
                                  font-size: 16px }")))),
                          column(1)
                                 )),
               
               tabPanel(title = "Flavor Median",
                        fluidRow(
                          style='height:100vh',
                          column(9,align = "center",
                                 plotlyOutput("flavormedium",height = 580)),
                          column(3,
                                 align = "center",
                                 h5(textOutput("variety5"),
                                    tags$head(tags$style("#variety5{
                                  font-size: 16px
                                 }")),
                                    hr(),
                                h5(textOutput("variety6"),
                                   tags$head(tags$style("#variety6{
                                  font-size: 16px
                                 }"))),
                                hr(),
                                h5(textOutput("variety7"),
                                   tags$head(tags$style("#variety7{
                                  font-size: 16px}"))),
                                hr(),
                                h5(textOutput("variety11"),
                                   tags$head(tags$style("#variety11{
                                                        font-size: 16px
                                                        }")))
                          ))
                           )))
              )),
    
    tabItem(tabName = "Age",
            fluidRow(
              style='height:100vh',
              column(12,align = "center",
                     h3(textOutput("welcome2")),
                     tags$style(type = "text/css", '#welcome2{
                                                        font-size: 25px;
                                font-style:italic;
                                font-family:Georgia;
                                margin-bottom: 30px;
                                }')),
                     #tags$img(src = "./www/age_scatter.png", width=800))
              column(1),
              column(3,align = "center",
                     br(),br(),br(),br(),
                     h5(textOutput("age1"),
                        tags$head(tags$style("#age1{
                                  font-size: 17px}"))),
                     hr(),
                     h5(textOutput("age2"),
                        tags$head(tags$style("#age2{
                                  font-size: 17px}")))),
              column(8,imageOutput("agescatter"),align = "left")
               )),
    
    tabItem(tabName = "wineInsight",
            fluidRow(
              style='height:150vh',
              column(2),
              column(8, align = "center",
                     br(),br(),br(),br(),
                     h5(textOutput("con1"),
                        tags$head(tags$style("#con1{
                                             font-size: 17px}"))),
                     hr(),
                     h5(textOutput("con2"),
                        tags$head(tags$style("#con2{
                                             font-size: 17px}"))),
                     hr(),
                     h5(textOutput("con3"),
                        tags$head(tags$style("#con3{
                                             font-size: 17px}"))),
                     hr(),
                     h5(textOutput("con4"),
                        tags$head(tags$style("#con4{
                                             font-size: 17px}"))),
                     hr(),
                     h5(textOutput("con5"),
                        tags$head(tags$style("#con5{
                                             font-size: 17px}"))),
                     hr(),
                     h5(textOutput("con6"),
                        tags$head(tags$style("#con6{
                                             font-size: 17px}")))),
              column(2)
                        )),
    tabItem(tabName = "Cheers",
            fluidRow(
              br(),br(),
              column(12,align = "center", h3(textOutput("final")),
                     tags$style(type = "text/css", '#final{
                                                        font-size: 30px;
                                font-style:italic;
                                font-family:Georgia;
                                margin-bottom: 30px;
                                }')),
              column(12,align = "center",imageOutput("cheers",height = 400))
            )),
    tabItem(tabName = "wineGrading",
            fluidRow(
              style='height:150vh',
              column(12,align = "center",offset = 0, h3(textOutput("welcome4")),
                     tags$style(type = "text/css", '#welcome4{
                                                        font-size: 25px;
                                font-style:italic;
                                font-family:Georgia;
                                margin-bottom: 30px;
                                }')),
              column(width = 12,offset = 0, style='padding:0px;',
                            align = "center",imageOutput("price",height = 400),
              imageOutput("pricelog",height = 400)),
              column(2),
              column(width = 8, align = "center",
                     br(),
                     h5(textOutput("grading1"),
                        tags$head(tags$style("#grading1{
                                  font-size: 17px}"))),
                     hr(),
                     h5(textOutput("grading2"),
                        tags$head(tags$style("#grading2{
                                  font-size: 17px}"))))
            ))
            ))
))
              
            