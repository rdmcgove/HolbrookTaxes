#
#title: "Holbrook MA, Taxes Dashboard"
#author: "Ryan McGovern"
#date: "Julyne 24, 2019"
# server

library("shiny")
library("shinydashboard")
library("tidyverse")
library("readxl")
library("ggplot2")
library("leaflet")
library("htmltools")
library("DT")
library("knitr")
library("GGally")
library("readxl")
library("formattable")
library("scales")
library("rvest")
library("dplyr")
library("rgl")
library("MuMIn")
library("Metrics")
library("modelr")
library("caret")
library("e1071")
library("tree")
library("randomForest")
library("gbm")
library("rpart")

# Define server logic required to draw a histogram
function(input, output, session) {
  
  #import the data table, there is another non shiny program that was used to source the data and to merge it to zillow
  #i didnt want to be constantly be hitting zillow
  histdata <- read.csv("Shiny_Data.csv")
  
  #add the percent_zestimate value and the difference variable
  histdata <- histdata %>% mutate(percent_zestimate = (TotalValue/zestimate)*100) %>% mutate(est_zillow_diff= zestimate - TotalValue)
  
  #plot for the introduction
  AssessedValue <- data.frame(Price=histdata$TotalValue)
  EstimatedValue <- data.frame(Price=histdata$zestimate)
  
  # Now, combine two dataframes into one.  
  # First make a new column that will identify where they came from
  AssessedValue$Value <- 'Assessed'
  EstimatedValue$Value <- 'Estimated'
  
  # and combine into your new data frame vegLengths
  valueCompare <- rbind(AssessedValue, EstimatedValue)

  #compute the values for the first bank of value boxes in the intro tab
  output$SoldYearMedian <- renderText({median(histdata$SaleYear, na.rm = TRUE)})
  output$TotalHomes <- renderText({paste(format(unlist(count(histdata)), big.mark=",", scientific=FALSE))})
  output$popularType <- renderText({names(sort(summary(as.factor(histdata$type)), decreasing=T)[1][1])})
  output$SaleMean <- renderText({paste(currency(mean(histdata$SalePrice, na.rm = TRUE), digits = 0L))})
  output$ValueMean <- renderText({paste(currency(mean(histdata$TotalValue, na.rm = TRUE), digits = 0L))})
  output$EstimateMean <- renderText({paste(currency(mean(histdata$zestimate, na.rm = TRUE), digits = 0L))})
  
  #create static histograms
  #create a plot for the first tab, this will show the range of assessed values vs estimated values
  output$plota <- renderPlot({
    ggplot(valueCompare, aes(x=Price, fill = Value)) + geom_area(alpha = 0.5, stat = "bin", binwidth = 25000) + theme(legend.position="bottom") + scale_x_continuous(labels=dollar_format(prefix="$"))
    })
    
  #create the plots to show the distribution of zestimate vs assessed
  output$plotb <- renderPlot({
    ggplot(histdata, aes(x=percent_zestimate)) + 
    geom_area(alpha = 0.5, stat = "bin", binwidth = 5) +
    scale_x_continuous(labels=number_format(suffix = "%")) + 
      xlab("Percent of Zestimate") +
      ylab("Number of Properties")
  })
  
  #calculate the stats for the second bank of the into tab
  output$MeanPercent_zestimate <- renderText({paste(number(mean(histdata$percent_zestimate, na.rm = TRUE), digits = 2L), "%")})
  output$MedianPercent_zestimate <- renderText({paste(number(median(histdata$percent_zestimate, na.rm = TRUE), digits = 2L), "%")})
  output$StdPercent_zestimate <- renderText({paste(number(sd(histdata$percent_zestimate, na.rm = TRUE), digits = 2L), "%")})
  
  #third plot
  output$plotc <- renderPlot({
    ggplot(histdata, aes(x=est_zillow_diff)) + 
      geom_area(alpha = 0.5, stat = "bin", binwidth = 25000) +
      scale_x_continuous(labels=number_format(prefix = "$")) + 
      xlab("Difference from Zestimate") +
      ylab("Number of Properties")
  })
  
  #calculate the stats for the third bank of the into tab
  output$MeanEst_zillow_diff <- renderText({paste(currency(mean(histdata$est_zillow_diff, na.rm = TRUE), digits = 0L))})
  output$MedianEst_zillow_diff <- renderText({paste(currency(median(histdata$est_zillow_diff, na.rm = TRUE), digits = 0L))})
  output$StdEst_zillow_diff <- renderText({paste(currency(sd(histdata$est_zillow_diff, na.rm = TRUE), digits = 0L))})
  
  #################################################################################
  #####################Begin Data ExplorationSection###############################
  #################################################################################
  
  #create the discrete plots
  output$DashPlot1 <- renderPlot({
    histdata2 <- histdata %>%mutate(split = ifelse(percent_zestimate>input$DashInput[1],"yes","No"))
    
    #discrete example (beds, baths, LOC, NHood, Type, SaleYear)
    g <- ggplot(histdata2, aes(x = type, fill = split))
    g + geom_bar(position = "dodge") + xlab("Property Type")  +scale_fill_discrete(name = "Above Threshold?") + ylab("Number of Properties") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  #create the discrete plots
  output$DashPlot2 <- renderPlot({
    histdata2 <- histdata %>%mutate(split = ifelse(percent_zestimate>input$DashInput[1],"yes","No"))
    
    #discrete example (beds, baths, LOC, NHood, Type, SaleYear)
    g <- ggplot(histdata2, aes(x = as.character(Baths), fill = split))
    g + geom_bar(position = "dodge") + xlab("Number of Bathrooms")  +scale_fill_discrete(name = "Above Threshold?") + ylab("Number of Properties")
  })
  
  output$DashPlot3 <- renderPlot({
    histdata2 <- histdata %>%mutate(split = ifelse(percent_zestimate>input$DashInput[1],"yes","No"))
    
    #discrete example (beds, baths, LOC, NHood, Type, SaleYear)
    g <- ggplot(histdata2, aes(x = as.character(Beds), fill = split))
    g + geom_bar(position = "dodge") + xlab("Number of Bedrooms")  +scale_fill_discrete(name = "Above Threshold?") + ylab("Number of Properties")
  })
  
  #Create the coninuous plots
  output$DashPlot4 <- renderPlot({
    histdata2 <- histdata %>%mutate(split = ifelse(percent_zestimate>input$DashInput[1],"yes","No"))
    
    #discrete example (beds, baths, LOC, NHood, Type, SaleYear)
    a <- ggplot(histdata2, aes(x = SaleYear))
    a + geom_area(alpha = 0.5, aes(fill = split), color = "white", stat ="bin", bins = 5) + ylab("Number of Properties") + labs(fill = "Above Threshold")
  })
  
  output$DashPlot5 <- renderPlot({
    histdata2 <- histdata %>%mutate(split = ifelse(percent_zestimate>input$DashInput[1],"yes","No"))
    
    #discrete example (beds, baths, LOC, NHood, Type, SaleYear)
    a <- ggplot(histdata2, aes(x = Built))
    a + geom_area(alpha = 0.5, aes(fill = split), color = "white", stat ="bin", bins = 5) + ylab("Number of Properties") + labs(fill = "Above Threshold")
  })
  
  output$DashPlot6 <- renderPlot({
    histdata2 <- histdata %>%mutate(split = ifelse(percent_zestimate>input$DashInput[1],"yes","No"))
    
    #discrete example (beds, baths, LOC, NHood, Type, SaleYear)
    a <- ggplot(histdata2, aes(x = LotSize))
    a + geom_area(alpha = 0.5, aes(fill = split), color = "white", stat ="bin", bins = 5) + ylab("Number of Properties") + labs(fill = "Above Threshold")
  })
  
  output$DashPlot7 <- renderPlot({
    histdata2 <- histdata %>%mutate(split = ifelse(percent_zestimate>input$DashInput[1],"yes","No"))
    
    #discrete example (beds, baths, LOC, NHood, Type, SaleYear)
    a <- ggplot(histdata2, aes(x = FinishedSize))
    a + geom_area(alpha = 0.5, aes(fill = split), color = "white", stat ="bin", bins = 5) + ylab("Number of Properties") + labs(fill = "Above Threshold")
  })
  
  #boxplot section
  output$Boxplots <- renderPlot({
    histdata2 <- histdata %>%mutate(split = ifelse(percent_zestimate>input$DashInput[1],"yes","No"))
    
  g <- ggplot(data = histdata2, aes(x=type, y=percent_zestimate))
  g + geom_boxplot() + scale_x_discrete() + ggtitle("Boxplot of Percent Zestimate") + ylab("Percent Zestimate") + xlab("Property Type")
    })
  
  #5 number summary
  output$fivenumYes <- renderTable ({
    histdata2 <- histdata %>%filter(percent_zestimate>input$DashInput[1])
    
    histdata2_t1 <- histdata2 %>% select(TotalValue, SalePrice, LotSize, FinishedSize, zestimate, percent_zestimate, est_zillow_diff)
    
    #calculate the summart information
    histdata2_t2 <- as.data.frame(summary(histdata2_t1[1:7], )) %>% separate(Freq, c("key","value"), ":", convert = TRUE) 
    
    #break out the stats by the : delimitor
    histdata2_t3 <- spread(histdata2_t2[2:4], key=Var2, value=value ) 
    
    #reorder the rows
    histdata2_t4 <- format.data.frame(histdata2_t3[c(6,1,5,4,2,3),], big.mark=",")
  })
  
  output$fivenumNo <- renderTable ({
    histdata2 <- histdata %>%filter(percent_zestimate<=input$DashInput[1])
    
    histdata2_t1 <- histdata2 %>% select(TotalValue, SalePrice, LotSize, FinishedSize, zestimate, percent_zestimate, est_zillow_diff)
    
    #calculate the summart information
    histdata2_t2 <- as.data.frame(summary(histdata2_t1[1:7], )) %>% separate(Freq, c("key","value"), ":", convert = TRUE) 
    
    #break out the stats by the : delimitor
    histdata2_t3 <- spread(histdata2_t2[2:4], key=Var2, value=value ) 
    
    #reorder the rows
    histdata2_t4 <- format.data.frame(histdata2_t3[c(6,1,5,4,2,3),], big.mark=",")
  })
  
  #####################################################################
  #####################Begin Map Section###############################
  #####################################################################
  
  #this will generate data for the map
  df <- data.frame(latitude = histdata$latitude,
                   longitude = histdata$longitude,
                   value = histdata$percent_zestimate, 
                   name = histdata$Location,
                   dollar_diff = histdata$est_zillow_diff)
  
  ## create the map before any filtering occures
  output$my_leaf <- renderLeaflet({
    leaflet(data=df) %>%
      addProviderTiles('Hydda.Full') %>%
      setView(lat = 42.15, lng = -71.01, zoom = 14) %>% 
      addMarkers(~longitude, ~latitude, popup = ~as.character(name), 
                 label = ~paste(
                   name, "/", currency(dollar_diff, digits = 0L), "/",
                   paste(number(value), "%")
                   ))
  })
  
  ## filter data based on the slider responses
  df_filtered <- reactive({
    df %>% filter(value >= input$sliderMap[1] & value <= input$sliderMap[2] &
                  df$dollar_diff > input$sliderMapDollar[1] & df$dollar_diff < input$sliderMapDollar[2])
                    })
  
  ## respond to the filtered data
  observe({
    leafletProxy(mapId = "my_leaf", data = df_filtered()) %>%
      clearMarkers() %>%   ## clear previous markers
      addMarkers(~longitude, ~latitude, popup = ~as.character(name), 
                 label = ~paste(
                   name, "/", currency(dollar_diff, digits = 0L), "/",
                   paste(number(value), "%")
                 ) )
  })
  
  #report how many places meet the filtering criteria
  #create text info
  output$MapmetCriteria <- renderText({
    #get filtered data
    newData <- df_filtered()
    paste(nrow(newData), "Properties meet the filtering criteria")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("List_Of_Addresses", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df_filtered() %>% select(name, value, dollar_diff), file, row.names = FALSE)
    })
  #####################################################################
  #####################Begin PCA Section###############################
  #####################################################################
  histdata_d <- histdata %>% select(-LUC, -Zipcode, -Parcel.ID, -SaleDate, -description,
                                    -BookPage, -Location, -est_zillow_diff, -type, -NHood)
  
  selectedData <- reactive({
    histdata_d[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    set.seed(10)
    kmeans(selectedData(), input$clusters, iter.max = input$iteration, algorithm = "MacQueen")
  })
  
  output$hclustplot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  output$dendo <- renderPlot({
    hierClust <- hclust(dist(data.frame(histdata_d[, c(input$xcol, input$ycol)])))
    plot(hierClust, xlab = "")
  })
  
  #####################################################################
  #################Begin Modeling Section##############################
  #####################################################################
  plotInput <- reactive ({
    ggpairs(data=histdata, columns=c(input$corrCoef, "percent_zestimate"))
  })
  
  output$pearson <- renderPlot({
    print(plotInput())
  })
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$corrCoef, '.png', sep='') },
    content = function(file) {
      ggsave(file,plotInput())
    }
  )
  
  #create the static boxplots
  #make boxplots
  output$Boxplot_type <- renderPlot({
    g <- ggplot(data = histdata, aes(x=type, y=percent_zestimate))
    g + geom_boxplot() + ggtitle("Boxplot of Percent Zestimate") + ylab("Percent Zestimate")  + xlab("Property Type")
  })
  
  output$Boxplot_nhood<- renderPlot({
    g <- ggplot(data = histdata, aes(x=as.factor(NHood), y=percent_zestimate))
    g + geom_boxplot() + ggtitle("Boxplot of Percent Zestimate") + ylab("Percent Zestimate")  + xlab("Neighborhood")
  })
  
  
  getTreeModel <- reactive({
    Zillow_pull5 <- histdata[, input$model_vars, drop = FALSE]
    
    #determine the number of rows for the training and the test datasets
    train <- sample(1:nrow(Zillow_pull5), size = nrow(Zillow_pull5)*0.8)
    test <- dplyr::setdiff(1:nrow(Zillow_pull5), train)
    
    #create the testing and training datasets
    zillowTrain <- Zillow_pull5[train, ]
    zillowTest <- Zillow_pull5[test, ]
    
    #determine the needed means and stds from the training dataset
    preProcValues <- preProcess(zillowTrain, method = c("center", "scale"))
    
    #apply the calculated meanss and stds from the training dataset to both the training and the testing dataset
    trainTransformed <- predict(preProcValues, zillowTrain)
    testTransformed <- predict(preProcValues, zillowTest)
    
    treeFit <- tree(percent_zestimate ~ ., data = zillowTrain)
    pred <- predict(treeFit, newdata = dplyr::select(zillowTest, -percent_zestimate))
    
    plot(treeFit)
    text(treeFit, all=TRUE, cex=input$sliderTreeSize)
    
    prune.trees <- prune.tree(treeFit, best=input$NumLeaves)
  })
  
  output$TreePlot <- renderPlot({
    prune.trees <- getTreeModel()
    
    plot(prune.trees)
    text(prune.trees, all=TRUE, cex=input$sliderTreeSize)
    })
  
  getBagModel <- reactive({
    Zillow_pull5 <- histdata[, input$model_vars, drop = FALSE]
  
    bagFit <- randomForest(percent_zestimate ~ ., data = Zillow_pull5, mtry = ncol(Zillow_pull5) - 1, 
                           ntree = 200, importance = TRUE)
  })
  
  output$BaggedTreePlot <- renderPlot({
    bagFit <- getBagModel()

    #focus on percent increase in MSE, this drives the model
    varImpPlot(bagFit, main="")
  })
  
  # create the check boxes to reduce the number of variables
  output$choose_vars <- renderUI({
    
    # Get the data set with the appropriate name
    dat <- histdata %>% select(-LUC, -Zipcode, -Parcel.ID, -SaleDate, -description,
                               -BookPage, -Location, -est_zillow_diff, -type)
    colnames <- names(dat)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_vars", "Pick Variables", 
                       choices  = colnames,
                       selected = c("Built", "SalePrice", "Beds", "LotSize", "NHood",
                                    "Baths", "FinishedSize", "SaleYear", "latitude",
                                    "longitude", "percent_zestimate"), inline = TRUE)
  })
  
  #create the tree diagram prediction
  output$TreePred <- renderText({
    prune.trees <- getTreeModel()
    predict(prune.trees, newdata = data.frame(Built = input$MBuilt, 
                                              TotalValue= input$MTotalValue,
                                              SalePrice = input$MSalePrice,
                                              Beds=input$MBeds,
                                              LotSize=input$MLotSize,
                                              NHood=as.numeric(input$MNHood),
                                              Baths=input$MBaths,
                                              FinishedSize=input$MFinishedSize,
                                              SaleYear=input$MSaleYear,
                                              zestimate=input$Mzestimate,
                                              latitude=input$Mlatitude,
                                              longitude=input$Mlongitude))
  })
 
  #create the Bagged prediction
  output$BagPred <- renderText({
    bagFit <- getBagModel()
    predict(bagFit, newdata = data.frame(Built = input$MBuilt, 
                                              TotalValue= input$MTotalValue,
                                              SalePrice = input$MSalePrice,
                                              Beds=input$MBeds,
                                              LotSize=input$MLotSize,
                                              NHood=as.numeric(input$MNHood),
                                              Baths=input$MBaths,
                                              FinishedSize=input$MFinishedSize,
                                              SaleYear=input$MSaleYear,
                                              zestimate=input$Mzestimate,
                                              latitude=input$Mlatitude,
                                              longitude=input$Mlongitude
                                              ))
  })
  
  #####################################################################
  #####################Begin data table################################

  
  # create the check boxes to reduce the number of variables
  output$choose_columns <- renderUI({
    
    # Get the data set with the appropriate name
    dat <- histdata[,c(2,3,6,13,7,14,12,4,20,21,22,5,8,9,10,11,15,16,17,18,19,1)]
    colnames <- names(dat)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Pick Variables", 
                       choices  = colnames,
                       selected = colnames, inline = TRUE)
  })
    
  #Conditionally filter data based on the entered responses
  data_filtered <- reactive({
    SD_LotSize <-sd(histdata$LotSize, na.rm = TRUE)
    SD_Built <- 7#sd(histdata$Built, na.rm = TRUE)
    SD_FinishedSize <-sd(histdata$FinishedSize, na.rm = TRUE)
    SD_latitude <-sd(histdata$latitude, na.rm = TRUE)
    
    #to identify similar properties we will look at the values entered +/- 1 SD
    if(input$sim){
       histdata[,c(2,3,6,13,7,14,12,4,20,21,22,5,8,9,10,11,15,16,17,18,19,1)] %>% filter(between(LotSize, input$DLotSize-SD_LotSize, input$DLotSize +SD_LotSize)) %>% filter(between(Built, input$DBuilt-SD_Built, input$DBuilt +SD_Built)) %>% filter(between(FinishedSize, input$DFinishedSize-SD_FinishedSize, input$DFinishedSize +SD_FinishedSize)) %>% filter(between(latitude, input$Dlatitude-SD_latitude, input$Dlatitude +SD_latitude)) 
  }else{histdata[,c(2,3,6,13,7,14,12,4,20,21,22,5,8,9,10,11,15,16,17,18,19,1)]}
    })
    
  # Output the data table
  output$data_table <- renderDataTable({
    # Get the data set
    dat = data_filtered()
    
    # Keep the selected columns and add formatting so the table looks good
    dat <- dat[, input$columns, drop = FALSE]
    datatable(dat) %>%  formatCurrency('percent_zestimate',currency = "", interval = 3, mark = ",", digits=2) %>%
                        formatCurrency('TotalValue',currency = "$", interval = 3, mark = ",", digits=0) %>%
                        formatCurrency('zestimate',currency = "", interval = 3, mark = ",", digits=0) %>%
                        formatCurrency('LotSize', currency = "", interval = 3, mark = ",", digits=0) %>%
                        formatCurrency('FinishedSize', currency = "", interval = 3, mark = ",", digits=0)
  })
  

  #allow the user to download all of the records or just ones that relate to their property
  output$downloadDataD <- downloadHandler(
    filename = function() {
      paste("List_Of_Addresses", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_filtered(), file, row.names = FALSE)
    })

  
}
