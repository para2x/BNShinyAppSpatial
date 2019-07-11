#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
suppressPackageStartupMessages({
  library(shiny)
  library(highcharter)
  library(rJava)
  library(dplyr)
  library(leaflet)
  library(leaflet.extras)
  library(sp)
  library(geojsonio)
  library(rgdal)
})

source('UtilFuncs.R')$value
source('bayesserver.R')$value
options(shiny.trace = FALSE)



# Define UI for application that draws a histogram
ui <- fluidPage(
  br(),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width = 3,


                fluidRow(
                column(12,
                       fluidRow(column(
                         12, 
                           textInput("BpH", label = h5("Biochar pH "), value = ""),
                           textInput(
                             "HT",
                             label = h5("Highest temperature "),
                             value = ""
                           ),
                           textInput("BC", label = h5("Biochar C "), value = "")
                         
                       ))))


      ),
      
      # Show a plot of the generated distribution
      mainPanel(width = 9,

        fluidRow(
          column(12,
            fluidRow(leafletOutput("mymap"))
            
          ))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  values <- reactiveValues(networkg=NULL,
                           compdf=data.frame(mRR=numeric(0),varRR=numeric(0),PRR=numeric(0),name=character(0)),
                           lRRvar=NULL,lmRR=NULL,lPRR=NULL, Farmobj=NULL)
  
  values$networkG<-buildnet()
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      setView(lng = -94.0589, lat = 42.3601, zoom = 15) %>%
      addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google')%>%
#      addProviderTiles(providers$OpenTopoMap,
#                       options = providerTileOptions(opacity = 0.75)) %>%
      addProviderTiles(providers$Stamen.TonerLabels)%>%
      addDrawToolbar(
        polylineOptions=FALSE,
        circleOptions=FALSE,
        markerOptions=FALSE,
        circleMarkerOptions=FALSE,
        singleFeature=TRUE,
        polygonOptions=drawPolygonOptions(
          shapeOptions = drawShapeOptions(fillColor = "Transparent")
          
        ),
        rectangleOptions=drawRectangleOptions(
          shapeOptions = drawShapeOptions(fillColor = "Transparent", weight = 3)
        ),
        #clearFeatures=TRUE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
      
  })
  
  
  observeEvent(input$mymap_draw_new_feature, {
    isolate({
      ## Finding the farm boundry
      obj<-input$mymap_draw_new_feature
      class(obj)<-"geo_list"  
      boundry<-geojsonio::geojson_sp(geojsonio::as.json(obj))
      cent<-rgeos::gCentroid(boundry)
      ###
      zone<-utmzonefinder(cent@coords[1])
      tmp.proj.soil<-sp::spTransform(boundry, CRS(paste0("+proj=utm +zone=",zone," ellps=WGS84")))
      TotalArea<-(rgeos::gArea(tmp.proj.soil,F))*(2.47)/(10000) 
     
      message(TotalArea)
      if(TotalArea<200){
        values$Farmobj$Polybounds<-boundry
        values$Farmobj$Centriod<-cent
        values$Farmobj$Area<-TotalArea
        values$Farmobj$UTMZone<-zone
        
        ## progress bar
        withProgress(message = 'Calculating ...', value = 0, {
          incProgress(0.1, message = paste("Loading shape files"))
          
          new.point<-bbox(values$Farmobj$Polybounds)
          bbxstr<-paste0(round(new.point[1,1],3),",",round(new.point[2,1],3)," ",round(new.point[1,2],3),
                         ",",round(new.point[2,2],3))
          ##downling the desire bbox
          fileName <- tempfile()
          downstr <-
            paste0(
              "https://sdmdataaccess.sc.egov.usda.gov/Spatial/SDMWM.wfs?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetFeature&TYPENAME=MapunitPoly&FILTER=<Filter><BBOX><PropertyName>Geometry</PropertyName><Box%20srsName=",
              paste("'","EPSG:4326","'",sep="")  
              ,"><coordinates>",
              bbxstr,
              "</coordinates></Box></BBOX></Filter>"
            )
          
          message(downstr)
          
          tryCatch({
          httr::GET(downstr, httr::write_disk(fileName), verbose=T)

          soilfile <- rgdal::readOGR(dsn = fileName,
                                     disambiguateFIDs=TRUE,require_geomType="wkbPolygon")
          ## deteling temporary file downloaded
          unlink(fileName)
          #projecting the map
          proj4string(soilfile) <- CRS("+init=epsg:3857")
          soilfile <- spTransform(soilfile,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
          clip <- rgeos::gIntersection(values$Farmobj$Polybounds, soilfile, byid = T) #clip polygon soil file with polygon SP
          # what polygons are in the cliped, keep them
          keep<-unlist(lapply(row.names(clip),function(x){trimws(substr(x,2,nchar(x)))}))
          newdata<-soilfile@data[as.numeric(keep),]
          ## some change in name ID of datframe for matching it with polygons
          row.names(newdata)<-unlist(lapply(clip@polygons, function(x){x@ID}))
          # adding new dataframe to the cliped polygons
          soilfile <-SpatialPolygonsDataFrame(clip,data=newdata)
          ################################ retieving the soil parameters
          incProgress(0.1, message = paste("Loading soil data"))
          dfsL<-soildataret(soilfile$mukey)
          incProgress(0.4, message = paste("Manipulating soil parameters"))
          ##merging shap file and data
          aggr<-dfsL%>%group_by(mukey,muname)%>%summarise_all(funs(mean))
          mrg<-merge(aggr,soilfile@data, by.x="mukey", by.y="mukey",all=TRUE)
          soilfile@data<-mrg
          #
          ## finding area of each polygon
          tmp.proj.soil<-spTransform(soilfile, CRS(paste0("+proj=utm +zone=",values$Farmobj$UTMZone," ellps=WGS84")))
          soilfile@data$muareaacres<-(rgeos::gArea(tmp.proj.soil,T))*(2.47)/(10000)  # convert m2 to ac
          
          #browser()
          
            soilfile@data<-as_tibble(soilfile@data) %>%
              tidyr::nest(-mukey)%>%
              mutate(Biochar=purrr::map(data, function(layerdata){
                layerdata
                res<-quary.BN(
                  values$networkG,
                  as.numeric(layerdata$sandtotal_r) %>% mean(.,na.rm = T) / 100,
                  as.numeric(layerdata$silttotal_r) %>% mean(.,na.rm = T)/ 100,
                  as.numeric(layerdata$claytotal_r) %>% mean(.,na.rm = T)/ 100,
                  as.numeric(layerdata$om_r)%>% mean(.,na.rm = T),
                  as.numeric(layerdata$ph1to1h2o_r)%>% mean(.,na.rm = T),
                  as.numeric(layerdata$cec7_r)%>% mean(.,na.rm = T),
                  NA,
                  NA,
                  NA,
                  as.numeric(4),
                  as.numeric(500),
                  as.numeric(2),
                  NA,
                  NA,
                  as.numeric(200),
                  "1",
                  "1",
                  "1"
                )
                
                t(unlist(res)) %>%
                  as.data.frame() %>%
                  `colnames<-`(c("Mean", "Var")) %>%
                  mutate(Pr = pnorm(0,
                                    res[[1]],
                                    sqrt(res[[2]]),
                                    lower.tail = F)
)
              })
              ) %>%
              tidyr::unnest(Biochar)

          
          values$Farmobj$Soil<-soilfile
          
          #leaflet proxy
          palettea<-"RdYlGn"
          
          pal <- colorNumeric(palette =palettea , domain = soilfile@data$Pr)

        leafletProxy("mymap", data = soilfile) %>%
          addPolygons(fillColor = ~pal(Pr),
                      weight=1,
                       popup = ~Pr,
                      color = "white",
                      dashArray = "3",
                      smoothFactor = 0.9,
                      fillOpacity = 0.4)%>%
          addLegend("bottomright", pal = pal,
                    values = ~Pr,
                    title = "",
                    opacity = 1
          )%>%
          flyTo(values$Farmobj$Centriod@coords[1,1],
                values$Farmobj$Centriod@coords[1,2],
                zoom=16)
        
          },
          error = function(e) {
            showModal(modalDialog(
              title = "Error in Bayesian Network",
              conditionMessage(e)
            ))
            return(NULL)
            
          })        

          
          
          #browser()
        })# end progress
        
      }else{
        showModal(modalDialog(
          title = "Exceeded the area limit",
          "The area of the selected polygon is more than 200 ac. \n Please start with drawing a smaller polygon.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    ##
    
    
    #updatevisual()
    
  })


}

# Run the application 
shinyApp(ui = ui, server = server)

