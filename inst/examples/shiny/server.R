library(shiny)
library(networkD3)

data(MisLinks)
data(MisNodes)

shinyServer(function(input, output) {
    
  output$simple <- renderSimpleNetwork({
    src <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")
    target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")
    networkData <- data.frame(src, target)
    simpleNetwork(networkData, opacity = input$opacity)
  })
  
  output$force <- renderForceNetwork({
    forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "name",
                 Group = "group", opacity = input$opacity, height=600)
  })
  
  output$sankey <- renderSankeyNetwork({
    library(RCurl)
#     URL <- "https://raw.githubusercontent.com/christophergandrud/d3Network/sankey/JSONdata/energy.json"
#     Energy <- getURL(URL, ssl.verifypeer = FALSE)
#     EngLinks <- JSONtoDF(jsonStr = Energy, array = "links")
#     EngNodes <- JSONtoDF(jsonStr = Energy, array = "nodes")
    Energy.file <- file.path(dbpath, "GitHub", "networkD3", "JSONdata", "energy.json")
    EngLinks <- JSONtoDF(file = Energy.file, array = "links")
    EngNodes <- JSONtoDF(file = Energy.file, array = "nodes")
    
    sankeyNetwork(Links = EngLinks, Nodes = EngNodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  fontsize = 12, nodeWidth = 30)
  })

  output$rt <- renderTreeNetwork({
    library(RCurl)
#     Flare <- getURL("https://gist.githubusercontent.com/mbostock/4063550/raw/a05a94858375bd0ae023f6950a2b13fac5127637/flare.json")
#     Flare <- rjson::fromJSON(Flare)
    Flare.file <- file.path(dbpath, "GitHub", "networkD3", "JSONdata", "flare.json")
    Flare <- rjson::fromJSON(file = Flare.file)
    treeNetwork(List = Flare, fontSize = 10, opacity = 0.9, margin=0)
  })
  
})
