packages.used=c("dplyr", "plotly", "shiny", "leaflet", "scales", 
                "lattice", "htmltools", "maps", "data.table", 
                "dtplyr", "mapproj", "randomForest", "ggplot2", "rpart", "zipcode", "geosphere", "fmsb")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))

# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}
library(visNetwork)
library(openintro)
library(widgetframe)
library(shiny)
library(leaflet)
library(scales)
library(lattice)
library(dplyr)
library(htmltools)
library(maps)
library(plotly)
library(data.table)
library(dtplyr)
library(mapproj)
library(randomForest)
library(ggplot2)
library(rpart)
library(zipcode)
library(geosphere)
library(googleVis)
library(fmsb)
library(sf)

# Import function from scripts in lib
#source("../lib/datatable_func.R")

# Import data for the key statistics section
#data_general3 = read.csv("../output/Hospital_count_by_state.csv", header = FALSE)
#colnames(data_general3) = c("state.abb","hospital.number")

#data1 <- read.csv("../data/data1.csv")
#data2 <- read.csv("../data/data2.csv")

shinyServer(function(input, output,session){
  #read data
  #load("./importance.RData")
  #load("./df.RData")
  load("./wine_map1.RData")
  #load("./wine_map2.RData")
  load("./data_box.RData")
  load("./data_variety.RData")
  load("./flavors.RData")
  wine <- readRDS("wine.RData")
  variety <- readRDS("winerycount.RData")
  wine_df4 <- readRDS("wine_map2.RData")
  nodes <- readRDS("nodes.RData")
  edges <- readRDS("edges.RData")
  grading <- readRDS("provinceage.RData")
  pd <- readRDS("pd.RData")
  
  
  data <- wine_df4
  data$stateabb <- state2abbr(data$province)
  
 
  # Outputs
 
  output$welcome1 <- renderText({"The Secret behind Quality Wine Pricing"})
  output$welcome2 <- renderText({"Price by Variety by Age"})
  output$welcome3 <- renderText({"Average Rating for the Most Popular Varietal Produced by Each Region:"})
  output$welcome4 <- renderText({"Wine Ratings Grades vs Price"})
  output$welcome5 <- renderText({"Price"})
  output$welcome6 <- renderText({"LogPrice"})
  
  output$usmap1 <- renderText({"Average Wine Price by State in US"})
  
  output$insight1 <- renderText({"Most expensive wines are from UK, France, Italy, Hungary, Germany, and Switzerland, all above $40 per bottle, 
which is not surprising since Europe is the most famous continent in the world for wine production.
    "})
  output$insight2 <- renderText({"In the US, the most expensive wines are from California, Oregon, Nevada, 
    and the most grape varieties are Pinot Noir, Cabernet Sauvignon, and Chardonnay."})
  
  output$varietynetwork1 <- renderText({"The network provides a user-friendly interactive plot that can be used for customers to choose their preferred wine."})
  
  output$varietynetwork2 <- renderText({"The drop-down “select by id” menu contains all top 10 types of variety and by selecting each, users can see what are the distinguished flavors of the particular variety. 
    The drop-down menu of “select by group” can target the plot to only present flavor or variety information. "})
  output$varietynetwork3 <- renderText({"Furthermore, if the user like particular taste, they can select the flavor and all the varieties containing the flavor would be highlighted."})
  
  output$variety1 <- renderText({"Red wines, white wines and rose wines form their own clusters: square represents red wine, triangle represents white wine, and star represents rose."})
  
  output$variety2 <- renderText({"Although all different colors of wines share some similar flavors, wines of the same color share more similar flavors and hence share the same edges, 
resulting in the same clusters."})
 
  output$variety3 <- renderText({"Most expensive varieties are Pinot Noir, Cabernet Sauvignon, Syrah, and Sangiovese, 
which all happen to be red wines.
    "})
  
  output$variety4 <- renderText({"The median prices of these wines are above our overall/population median price, 28 dollars."})
  output$variety5 <- renderText({"We take top 10 flavors from the 10 most popular grape varieties and put them into a flavor pool. Then we extract wines from the original dataset that contain any flavors from the flavor pool. 
    We next group by flavor and calculate median price for each flavor and plot the graph of flavor vs. price."})
  
  output$variety6 <- renderText({"The blue line represents the wine population median price, 28 dollars."})
  output$variety7 <- renderText({"The ‘most expensive’ flavors includes blackberry, dark, cola, raspberry, cranberry, chocolate, cherry, currant, 
    tannin, rich, earth, and spice."})
  
  output$variety8 <- renderText({"In general red wines have higher prices than white wines and rose. We assume flavor difference and flavor complexity might be the cause of the price difference, 
    so in the next graph we analyze review contents."})
  output$variety9 <- renderText({"Besides shared flavors, wines of different colors have their unique flavors as well. White wines’ unique flavors are mostly sweet and fruity, 
    whereas red wines’ unique flavors are more diverse and complex, including earthy, spice, herb, tobacco, etc."})
  output$variety10 <- renderText({"The visual network confirms our assumption that red wines and white wines have different flavors and flavor complexity, 
    but whether this is related to price or not remains a question, which is explained in the next graph."})
  
  output$variety11 <- renderText({"More than half of the top flavors whose median prices are higher than population median price(colored in red) are unique flavors of red wines. This further consolidates our assumption 
    that red wines are generally more expensive than white wines because they have more flavors, both in number and in complexity."})
  
  output$age1 <- renderText({"This plot presents age versus price by variety graph, as aging might have different effects on different grape varieties. 
As a result, the prices of Cabernet Sauvignon and Sauvignon Blanc increase when age increase, especially after 20 years."})
  output$age2 <- renderText({"The price of Sangiovese increases between age 0 and 8 years.
  Other grape varieties have a rather stable price throughout the year."})
  
  output$grading1 <- renderText({"
    This plot indicates the relationship between wine grading and wine price. 
It is very intuitive and shows a distinct positive correlation between wine price and its grading. 
    Customers do get better wines when they pay more.
    "})
  output$grading2 <- renderText({"The trend is pretty obvious for grades vs. price. The price range is from 0 to 1000 which is too wide and unnecessary.  
    By utilizing logarithm with base 2, we scale our price and the trend is more distinct."})
  
  output$con1 <- renderText({"We investigate wine pricing from the wine quality perspective, which includes wine variety, wine flavors, wine ages, and wine score.
"})
  output$con2 <- renderText({"We find out that the most expensive grape varieties among popular varieties are Pinot Noir, Cabernet Sauvignon, and Syrah, which all happen to be red wines. 
"})
  output$con3 <- renderText({"By doing word cloud, we realize that red wines are more flavorful than white wines: it not only contains fruity flavors, but also other flavors like earth, spice, currant, and chocolate.
"})
  output$con4 <- renderText({"The flavor versus price graph consolidates our view on flavors: the top most expensive flavors contain mostly extraordinary flavors that can be find in red wines.
"})
  output$con5 <- renderText({"For some grape variety like Cabernet Sauvignon, Sauvignon Blanc, and Sangiovese, increasing age also contributes to increasing price, while for other grape varieties, age doesn't seem to be a contributing factor to price.
"})
  output$con6 <- renderText({"Most importantly, we find that wine review scores correlate perfectly with wine price, which means that as customers, we do get what we pay for!
"})
  
  output$final <- renderText("Work Hard, Play Hard, Drink Hard!")

  output$cheers <- renderImage({
    list(src = "cheer.png",height = 520, width = 720, deleteFile = FALSE)
  },deleteFile = FALSE)
  
  output$agescatter <- renderImage({
    list(src = "age_scatter.png",height = 600, width = 720, deleteFile = FALSE)
    },deleteFile = FALSE)

  output$wine1 <- renderImage({
    list(src = "wine1.png",height = 400, width = 400, deleteFile = FALSE)
  },deleteFile = FALSE)
  
  output$wine2 <- renderImage({
    list(src = "wine2.png",height = 400, width = 400, deleteFile = FALSE)
  },deleteFile = FALSE)
  
  output$price <- renderImage({
    list(src = "price.png",height = 400, width = 800, deleteFile = FALSE)
  },deleteFile = FALSE
  )
  
  output$pricelog <- renderImage({
    list(src = "pricelog.png",height = 400, width = 800, deleteFile = FALSE)
  },deleteFile = FALSE
  )
  
  
  output$varietymedium <- renderPlotly({
    box_plot_variety <- 
      data_variety %>%
      ggplot(aes(x = reorder(variety, median), 
                 y = price,
                 fill = type)) +
      geom_boxplot(outlier.size=0.1, outlier.colour = "black", outlier.shape = 1) +
      labs(x = 'Variety',
           y = 'Price') +
      theme(axis.text = element_text(size = 8),
            axis.title = element_text(size = 10)) +
      scale_fill_manual(values = c( "firebrick", "lightpink","antiquewhite")) +
      geom_hline(yintercept=28, colour = "blue", size=.3) +
      ggplot2::annotate("text", x = 0.7, y = 39, colour="blue", size = 2, 
                        label = "Population Median Price") +
      coord_flip() +
      theme_classic()
    
    ggplotly(box_plot_variety)
  })

    
  output$one <- renderImage({
    list(src="1.png",height = 150, width = , deleteFile = FALSE)
  }, deleteFile = FALSE)
  
  output$wordcloud <- renderImage({
    if(input$Variety == "Pinot Noir"){
      list(src="Pinot_Noir.png",deleteFile = FALSE)
    } 
      else if(input$Variety == "Chardonny"){
        list(src = "Chardonnay.png", deleteFile = FALSE)
      }
      else if(input$Variety == "Syrah"){
        list(src = "Syrah.png",deleteFile = FALSE)
      }
    else if(input$Variety == "Cabernet_Sauvignon"){
      list(src = "Cabernet_Sauvignon.png", deleteFile = FALSE)
    }
    else if(input$Variety == "Merlot"){
      list(src = "Merlot.png", deleteFile = FALSE)
    }
    else if(input$Variety == "Riesling"){
      list(src = "Riesling.png", deleteFile = FALSE)
    }
    else if(input$Variety == "Rose"){
      list(src = "Rose.png",deleteFile = FALSE)
    }
    else if(input$Variety == "Sangiovese"){
      list(src = "Sangiovese.png",deleteFile = FALSE)
    }
    else if(input$Variety == "Sauvignon_Blanc"){
      list(src = "Sauvignon_Blanc.png", deleteFile = FALSE)
    }
    else if(input$Variety == "Zinfandel"){
      list(src = "Zinfandel.png", deleteFile = FALSE)
    }
    },deleteFile = FALSE)

  output$network <- renderVisNetwork({
    library(networkD3)
    
    forceNetwork(Links = edges, Nodes = nodes,
                 Source = "from", Target = "to",
                 Value = "weight", NodeID = "label", 
                 Group = "group", opacity = 0.8)
    
    library(visNetwork)
    
    v1 <- visNetwork(nodes, edges, height = "700px", width = "100%") %>%
      visOptions(selectedBy = "group", 
                 highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, values  = c(1:10))) %>% 
      visInteraction(keyboard = TRUE, tooltipDelay = 0) %>%
      visPhysics(stabilization = FALSE)
    
    v1
    #library(widgetframe)
    #frameWidget(v1)
    
  }
    )
  
  output$usmap <- renderPlotly({
    g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    lakecolor = toRGB('white')
  )
  
    plot_ly(z = data$Average_Price, 
            text = data$province, 
            locations = data$stateabb,
          type = 'choropleth', locationmode = 'USA-states',colors = "Purples") %>%
    layout(geo = g)
  })

  
  output$table1 <- renderTable({
    s <- event_data("plotly_click")
    d <- s$z
    a <- wine_df4[which(wine_df4$Average_Price == d),]$province
    count <- as.data.frame(variety[which(variety$province == a),])
    colnames(count)[1]<-"State"
    colnames(count)[2]<-"Numer of Wineries"
    count
  })
  
  output$table3 <- renderTable({
    s <- event_data("plotly_click")
    d <- s$z
    a <- wine_df4[which(wine_df4$Average_Price == d),]$province
    grade <- as.data.frame(grading[which(grading$province == a),])
    colnames(grade)[1]<-"State"
    colnames(grade)[2]<-"Average Rating"
    grade
  })
  
  output$table2 <- renderPlotly({
    s <- event_data("plotly_click")
    d <- s$z
    a <- wine_df4[which(wine_df4$Average_Price == d),]$province
    gg <- wine %>% 
      filter(province == a) %>%
      group_by(variety) %>%
      summarize(variety_count = n()) %>%
      arrange(desc(variety_count)) %>% 
      top_n(3) %>%
      ggplot(aes(x = reorder(variety, variety_count, decreasing = T), y = variety_count)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      theme_classic()+
      geom_text(aes(label = variety_count), hjust = -0.2, size = 2) +
      xlab("Variety") +
      ylab("Wine count")
    
    ggplotly(gg) %>% layout(height = 260, width = 620)
    
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click")
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(typeof(s$z))
    }
  })
  
  
  output$leafletworldmap <- renderLeaflet({
    WorldCountry <-geojsonio::geojson_read("countries.geo.json", what = "sp")
    WorldCountry@data <- left_join(WorldCountry@data, pd, by = c("name" = "country"))
    dMap <- WorldCountry[WorldCountry@data$name %in% pd$country,]
    bins <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, Inf)
    pal <- colorBin("YlOrRd", domain = WorldCountry@data$Average_Price.x, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong>  <br/>$%g /bottle on average",
      dMap$name, dMap$Average_Price
    ) %>% lapply(htmltools::HTML)
    
    
    Map <- leaflet(dMap) %>% addTiles()
    Map <- Map %>% addPolygons(
      fillColor = ~pal(Average_Price),
      weight = 1,
      opacity = 1,
      color = 'white',
      dashArray = "3",
      fillOpacity = 0.7,
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
    
    
    Map %>%
      addLegend(pal = pal, values = ~Average_Price, opacity = 0.7,   position = "bottomright")
    
    
  })


  output$flavormedium <-renderPlotly({
    box_plot_flavors<- flavors %>%
      ggplot(aes(x = reorder(flavor, median),
                 y = price,
                 fill = type)) +
      geom_boxplot(outlier.size=0.1, outlier.colour = "red", outlier.shape = 1) +
      labs(x = 'Flavor',
           y = 'Price') +
      theme(axis.text = element_text(size = 4),
            axis.title = element_text(size = 10),
            plot.title = element_text(size = 15)) +
      geom_hline(yintercept=28, colour = "blue", size=.3) +
      ggplot2::annotate("text", x = 0.55, y = 34, colour="blue", size = 1.5, angle = 90,
                        label = "Population Median Price") +
      coord_flip() +
      theme_classic() +
      scale_fill_manual(values = c("firebrick", "antiquewhite"))
    
    ggplotly(box_plot_flavors)
  })
  
  output$boxplot <- renderPlotly({
    ggplot(data_box,
           aes(x = reorder(variety, median), 
               y = price,
               fill = color)) +
      geom_boxplot(outlier.size=0.1) +
      labs(x = 'Variety',
           y = 'Price') +
      theme(axis.text = element_text(size = 4),
            axis.title = element_text(size = 10),
            plot.title = element_text(size = 15)) +
      scale_fill_manual(values = c("lightpink", "firebrick", "antiquewhite"))+theme_bw()
    ggplotly()
  })
  
  
  
})