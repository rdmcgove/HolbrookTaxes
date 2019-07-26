#
#title: "Holbrook MA, Taxes Dashboard"
#author: "Ryan McGovern"
#date: "Julyne 24, 2019"
# UI

library(shiny)
library(shinydashboard)
library(formattable)
library(leaflet)
library(scales)
library(htmltools)
library(knitr)
library(dplyr)

#import the data
histdata <- read.csv("Shiny_Data.csv")
histdata_d <- histdata %>% mutate(percent_zestimate = (TotalValue/zestimate)*100) %>% mutate(est_zillow_diff= zestimate - TotalValue)

#this will be the list of variables for the dendogram
histdata_d <- histdata_d %>% select(-LUC, -Zipcode, -Parcel.ID, -SaleDate, -description,
                                  -BookPage, -Location, -est_zillow_diff, -type, -NHood)

#define the main dashboard structure, sidebar and title
dashboardPage(skin = "red",
  dashboardHeader(title = "Holbrook, MA Taxes"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle"), badgeLabel = "Start Here", badgeColor = "green"),
      menuItem("Data Exploration", tabName = "dashboard", icon = icon("search")),
      menuItem("Map", tabName = "Map", icon = icon("map")),
      menuItem("Principal components Analysis", tabName = "pca", icon = icon("chart-bar")),
      menuItem("Modeling", tabName = "modeling", icon = icon("chart-bar")),
      menuItem("Data", tabName = "data", icon = icon("glasses"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      
      #Intro Tab
      tabItem(tabName = "intro",
              fluidRow(
                tags$h2("Single Family Real Estate Taxes"),
                #Overall Stats (valueBoxs)
                valueBox(textOutput("SoldYearMedian"), "Median Sold Year", icon = icon("sign")),
                valueBox(textOutput("TotalHomes"), "Total Number of Homes", icon = icon("home")),               
                valueBox(textOutput("popularType"), "Most Common Type", icon = icon("home")),               
                valueBox(textOutput("SaleMean"), "Averge Sale Price", icon = icon("dollar-sign")),
                valueBox(textOutput("ValueMean"), "Averge Assessed Value", icon = icon("dollar-sign")),
                valueBox(textOutput("EstimateMean"), "Averge Estimated Value", icon = icon("chart-line")),
                br(),
                p(tags$strong("Holbrook"), "has one of the highest property tax rates in the 
                               state and property tax assessments, the value that the town 
                               assigns to a property, is hot topic for residents. A common 
                               question a homeowner might ask is if a property is being fairly 
                               assessed."),
                br(),
                p("A good starting point is", tags$a(href="https://www.zillow.com/", "Zillow"),
                  "an online real estate database company that provides their trademark Zestimate
                   value for properties. Zillow determines a Zestimate for a property based on a 
                   range of publicly available information, including public data and sales of 
                   comparable houses in a neighborhood."),
                br(),
                p(paste("The next step would be to stop by the Tax Assessors office to see what 
                         they have listed for an assessed value. This information is publicly 
                         available thru their online property record cards accessible "),
                tags$a(href="http://holbrook.patriotproperties.com/default.asp", "here.")),
                br(),
                p("For this dashboard both data sources were pulled from their respective databases as of 7/19/2019 and
                   combined. The focus will be to investigate what drives the difference in the assessed value of a home 
                   and the estimated value from Zillow. Some questions that will be addressed:"),
                tags$ul(
                  tags$li("What are the key drivers of difference in the towns assessed value and the Zestimate?"), 
                  tags$li("What properties appear to be underassessed?"), 
                  tags$li("What properties are over assessed?"),
                  tags$li("Are certain sections of the town consistently over/under assessed?"),
                  tags$li("If my property is over/underassessed what properties are like mine so
                           that I can bring this list to the board of assessors when I appeal my homes valuation?")
                ),
                br(),
                p("For starters look at the distribution of assessed values vs the distribution of their Zestimates "),
                tags$h2("Distribution of Assessed Value vs Zestimate"),
                plotOutput("plota"),
                br(),
                p("For the most part the distribution of Zestimate is consistantly higher when compared to the distribution
                   of assessed values. The distance between the two plotted lines is what the focus is on. Are homes in all of 
                   the price ranges the same percentage below their zestimate when compared to other ranges? 
                   What we are seeing makes sense as Zestimates can change daily but an assessed
                   value is static for the year. Properties are commonly assessed lower than fair market value 
                   to account for pricing fluctuations. Because of this we would expect the overall distribution of
                   Zestimate to appear like a larger shadow of the distribution of assessed values when overlaid looking
                  at this plot we can see that some ranges of property values differ greatly from their Zestimates where
                  as others are assessed much closer to their Zestimates."),
                br(),
                p("What is the typical deviation from a Zestimate?"),
                br(),
                tags$h2("Distribution of Properties as a Percentage of their Zestimate"),
                withMathJax(),
                helpText('$$Percent = \\frac{Assessed Value}{Zestimate}$$'),
                plotOutput("plotb"),
                p("The distribution appears normal with euqal mean and median"),
                br(),
                valueBox(textOutput("MeanPercent_zestimate"), "Mean % Of Zestimate", icon = icon("percent")),
                valueBox(textOutput("MedianPercent_zestimate"), "Median % Of Zestimate", icon = icon("percent")),               
                valueBox(textOutput("StdPercent_zestimate"), "Standard Deviation", icon = icon("percent")),    
                br(),
                p("What does this look like as a Percentage of a home's value computed as:"),
                tags$h2("Distribution of the Difference of Assessed Value and Zestimate"),
                withMathJax(),
                helpText('$$Difference = Zestimate - Assessed$$'),
                plotOutput("plotc"),
                br(),
                valueBox(textOutput("MeanEst_zillow_diff"), "Mean difference from Zestimate", icon = icon("dollar")),
                valueBox(textOutput("MedianEst_zillow_diff"), "Median difference from Zestimate", icon = icon("dollar")),               
                valueBox(textOutput("StdEst_zillow_diff"), "Standard Deviation (Zestimate - Assessed)", icon = icon("dollar")),
                br(),
                p("What is the typical deviation from a Zestimate in $? This is another normal distribution but for those on
                  the higher end one worth looking into more closely. With the current tax rate of over $20 dollars per thousand
                  assessed even small variances should be scrutinized"),
                br()
      )),

      ##################
      # Dashboard Tab###
      ##################
      tabItem(tabName = "dashboard",
              fluidRow(
                tags$h2("Assessed Value / Zestimate, What is a Fair Percentage?"),
                p("When considering an estimate what should the assessed value be in relation to this
                   number? In a perfect world these numbers would equal but typically assessed
                  values are slightly less than what the house could sell for. The tool below allows
                  you to select a value that you would consider a fair multiple and it will display 
                  some charts and graphs to show how the town falls under your criteria"),
                br(),
                p(tags$strong("Pick a value representing Assessed/Zestimate to split the properties into above and below.")),
                numericInput("DashInput", "Percentage to Split on", min = 0, max = 200, value =80),
                br(),
                p(tags$em("In laymans terms this would equate to 'I would expect most properties to be assessed at X% of their current Zestimate'")),
                br(),
                plotOutput("DashPlot1"),
                plotOutput("DashPlot2"),
                plotOutput("DashPlot3"),
                plotOutput("DashPlot4"),
                plotOutput("DashPlot5"),
                plotOutput("DashPlot6"),
                plotOutput("DashPlot7"),
                h2("Numeric Summaries"),
                br(),
                h3("Five Number Summaries For Properties Above the Threshold"),
                tableOutput("fivenumYes"),
                br(),
                h3("Five Number Summaries For Properties Below the Threshold"),
                tableOutput("fivenumNo")
      )),
      
      #######################
      ##########map##########
      #######################
      tabItem(tabName = "Map",
              h2("Where are these places?"),
              br(),
              p("Using the sliders below explore where properties are located that are over or under assessed
                in relation to their Zestimate. When you hover over a marker the following values will be displayed"),
              p(tags$ul(tags$li("Location / Zestimate- Assessed / Assessed over Zestimate as a percentage"))),
              p(tags$strong("Take it slow, using the sliders to quickly can cause this to crash!")),
              br(),
      leafletOutput("my_leaf"),
      br(),
      box(sliderInput(inputId = "sliderMap", 
                      label = "Assessed Value as a % of Zestimate",
                      min = -0,
                      max = 200,
                      value = c(50,120),
                      step = 1,
                      post = " %")),
      box(sliderInput(inputId = "sliderMapDollar", 
                      label = "Difference (Zestimate - Assessed Value)",
                      min = -400000,
                      max = 300000,
                      value = c(-400000,300000),
                      step = 25000, pre = "$")),
      p("The sliders work together. For instance, if you want to base the selection on a percentage only then select the entire range
         of Difference so that all properties with the selected percentages are displayed."),
      tags$strong(textOutput("MapmetCriteria")),
      br(),
      downloadButton("downloadData", "Save To File")
      ),
      
      #####################################
      # Principal Component analysis Tab###
      #####################################
      tabItem(tabName = "pca",
              h3("Principal Compenent Analysis"),
              p("use this tool to segment values into subgroups and recognize patterns"),
              box(selectInput('xcol', 'X Variable', names(histdata_d), selected=names(histdata_d)[[9]])),
              box(selectInput('ycol', 'Y Variable', names(histdata_d),
                          selected=names(histdata_d)[[10]])),
              box(numericInput('clusters', 'Cluster count', 3,
                           min = 1, max = 9)),
              box(numericInput('iteration', '# of Iterations of Algorithm', value = 1, min = 1, max = 10)),
              box(plotOutput('hclustplot1')),
              box(plotOutput('dendo')),
              br(),
              p("Using this tool we can find subgroups in the data. Groups where members are similar.
                 Numerically the average of distances between pairs of points is minimized  between all pairs
                of points. The tool uses two different methods, K-Means clustering and hierarchial clustering")
      ),
      
      ##################
      # modeling  Tab###
      ##################
      tabItem(tabName = "modeling",
              h2("Develop Some Models"),
              p("Lets develop some models to see what variables relate to % Zestimate and what variables 
                drive this relationship. What factors influence a higher or lower assessment/Zestimate ratio?"),
              br(),
              h2("Exploratory Data Analysis"),
              p("What variables are correlated with the percent Assessed/Zestimate?"),
              selectInput("corrCoef", "Variable:",
                          c("Year Built" = "Built",
                            "Total Value" = "TotalValue",
                            "Sale Price" = "SalePrice",
                            "Number of Bedrooms"="Beds",
                            "Number of Bathrooms"="Baths",
                            "Finished Size"="FinishedSize",
                            "LotSize"="LotSize",
                            "Sale Year"="SaleYear",
                            "Zestimate"="zestimate",
                            "Latitude"="latitude",
                            "Longitude"="longitude",
                            "Zestimate - Assessed"="est_zillow_diff")),
              plotOutput("pearson"),
              downloadButton('downloadPlot', 'Download Plot'),
              br(),
              br(),
              p("Look at distribution of categorical predictors"),
              br(),
              plotOutput("Boxplot_type"),
              br(),
              plotOutput("Boxplot_nhood"),
              br(),
              h2("Decision Tree Model"),
              p("We will split up the predictor space into regions with each region having a different
                 prediction. The eventual outcome will identify a value for % Zestimate."),
              plotOutput("TreePlot"),
              box(sliderInput(inputId = "sliderTreeSize", 
                          label = "Font Size for Labels",
                          min = .1,
                          max = 1,
                          value = c(.8),
                          step = .1)),
              box(sliderInput(inputId = "NumLeaves", 
                              label = "Number of Leaves",
                              min = 1,
                              max = 10,
                              value = c(8),
                              step = 1)),
              br(),
              h2("Bagged Tree Model"),
              p("For this method we will perform  Bootstrap Aggregation (Bagging). The difference
                 between this and the previous method is that rather than using one sample we will
                 perform multiple samples from our initial sample repeatedly and take the averages
                 of these resamples."),
              plotOutput("BaggedTreePlot"),
              p("For the bagged model finished size was the leading component in the model and to a 
                 lesser extent sale year. The decision tree had lot size and finished size leading the
                ratio difference. At least spot checking the data some properties that appear grossly 
                under-assessed are new costruction where an assessor might not have visited yet. 
                using the decision tree it seems that if you bought a house with a lot size under 5025 sqft (.12 acres)
                with a finished size >826sqft that was built before 1972 you are in great shape. Not so much if you 
                bought a large house on a large lot. Knowing the background of the town it seems like if you are a grove resident
                where small homes were built on small lots you are more likely to have your property assessed at a lower ratio compared with the zestimate."),
              h3("Model Modification and Exploration"),
              p("Select what variables should be included in the model"),
              uiOutput("choose_vars"),
              br(),
              h3("Lets Use our models"),
              numericInput("MBuilt", "Year Built:", 1990, min = 1950, max = 2019),
              numericInput("MTotalValue", "Total Value:", 300000, min = 100000, max = 900000),
              numericInput("MSalePrice", "Sale Price:", 300000, min = 100000, max = 900000),
              numericInput("MBeds", "Beds:", 2, min = 1, max = 9),
              numericInput("MLotSize", "Lot Size:", 300000, min = 100000, max = 9000000),
              selectizeInput("MNHood", "NHood", choices = levels(as.factor(histdata$NHood))),
              numericInput("MBaths", "Baths:", 1, min = 1, max = 8),
              numericInput("MFinishedSize", "Finished Size:", 1000, min = 800, max = 4000),
              numericInput("MSaleYear", "Year Sold:", 2000, min = 1950, max = 2019),
              numericInput("Mzestimate", "Zestimate:", 300000, min = 100000, max = 900000),
              numericInput("Mlongitude", "longitude:", -71.00, min = -71.030058, max = -71.030058),
              numericInput("Mlatitude", "latitude:", 42.14, min = 42.119866, max = 42.168359),
              p("Using these values and the decision tre the predicted %Zestimate for the values
                entered would be:"),
              textOutput("TreePred"),
              br(),
              p("Using these values and the bagged tree the predicted %Zestimate for the values
                entered would be:"),
              textOutput("BagPred")
      ),
      
      ##################
      # data      Tab###
      ##################
      tabItem(tabName = "data",
              uiOutput("choose_columns"),
              br(),
              dataTableOutput("data_table"),
              br(),
              h2("Do you want to Find Similar properties to yours?"),
              br(),
              p("Filtering uses the values you enter +/- 1 standard deviation and 
                 +/- 7 Years as a criteria to determine that a home is similar to the one you enter"),
              checkboxInput("sim", h4("Find Similar Properties")),
              br(),
              box(numericInput("DLotSize", "Lot Size:", 5000, min = 2500, max = 1858270)),
              box(numericInput("DFinishedSize", "Finished Size:", 1650, min = 300, max = 6080)),
              box(numericInput("DBuilt", "Year Built:", 1963, min = 1700, max = 2019)),
              box(numericInput("Dlatitude", "latitude:", 42.13, min = 42.119866, max = 42.168359)),
              br(),
              p("The above selection will be used to find similar properties based on the collected responses"),
              br(),
              downloadButton("downloadDataD", "Save To File")
                )
      )
      
    
  )
)