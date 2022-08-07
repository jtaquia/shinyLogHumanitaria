library(shiny)
library(reticulate)
library(shinythemes)
library(geosphere)
library(tidyverse)
library(readxl)
library(leaflet)
library(leaflet.extras)
#library(leaflet.extras2)
library(magrittr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
#require(geosphere)
require(measurements)
require(maps)
library(ggplot2)

#devtools::install_github("rstudio/reticulate")
#virtualenv_create(envname = "optimization", python= "python3")
#virtualenv_remove(envname = 'optimization', packages = 'pip')
#virtualenv_install("optimization", packages = c('pandas','gurobipy','haversine','geopy','openpyxl'))
#use_virtualenv("optimization", required = TRUE)

#reticulate::use_virtualenv("optimization", required = TRUE)
#Sys.setenv(RETICULATE_PYTHON = "/usr/local/bin/python3")

demandaGrupo2 <- read_excel("demandaGrupoAsignacion3.xlsx",sheet = "Hoja1")

dataLocales<- read_excel("geolocalizacionAsignacion3Centros.xlsx")



#demand is proportional to population
#  dplyr::mutate(demand = ceiling(pop/10))


ui = fluidPage( 
    theme = shinytheme("cerulean"),
    navbarPage(
        
        "Caso Logistica Humanitaria",
        tabPanel("ModeloBase",
                 sidebarPanel(
                     fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE)
                     ,
                     #textInput("txt", "Text input:", "general"),
                     #sliderInput("slider", "Slider input:", 1, 100, 30),
                     #tags$h5("Default actionButton:"),
                     actionButton("action", "Search"),
                     
                     tags$h5("Ejecutar modelo optimizado con Gurobi"),
                     #actionButton("action2", "Gurobi optimizer button", class = "btn-primary"),
                     sliderInput("bins5","Centros ",min = 1,max = 5, value = 3),
                     hr(),
                     sliderInput("bins6","Distancia al centro en metros:",min = 950,max = 1500, value = 1000, step = 50),
                     hr(),
                     h5("Built with",
                        img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                        "by",
                        img(src = "https://www.r-project.org/logo/Rlogo.svg", height = "25px"),
                        
                        img(src = "https://www.python.org/static/community_logos/python-logo-inkscape.svg", height = "25px")),
                     
                     h5("Elaborado por José Antonio Taquía Gutiérrez")
                     ,
                     
                     helpText(a("www.taquiagutierrez.com",target="_blank", href="https://www.taquiagutierrez.com")
                     )
                     
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Escenario base Heatmaps",
                                  h4("Table"),
                                  
                                  h4("Resultados del modelo emergencia"),
                                  #verbatimTextOutput("txtout"),
                                  h1("Demanda"),
                                  # h2("Header 2"),
                                  # h3("Header 3"),
                                  # h4("Header 4"),
                                  h5("Asignacion distancias minimas - sin Optimizar"),
                                  #tableOutput("action2")
                                 
                                  
                                  leafletOutput("newmap"),
                                  
                                  plotOutput("plot"),
                                  tableOutput("files")
                                  
                                  
                                 
                                  #tableOutput("values")
                                  
                         ),
                         #tabPanel("Tab 2", "This panel is intentionally left blank"),
                         tabPanel("Escenario base con puntos de emergencia", 
                                  
                                  leafletOutput("newmap2")
                     )
                 )
        )
        )
        
        ,
        tabPanel("Modelo respuesta logistica con Optimizacion", 
        
                 sidebarPanel(
                     
                    tags$small(paste0(
                    "Se ejecuta modelo de optimizacion MIP con GNU Linear Programming Kit",
                     " y se adicionan los centros de ayuda."
                 )),
                 hr(),
                  
                                actionButton("submitbuttonOptmizado", "Submit", class = "btn btn-primary"),
                 
                 
                              hr(),
                              sliderInput("bins","Centros de respuesta:",min = 1,max = 5, value = 4),
                              hr(),
                              sliderInput("bins2","Sectores afectados:",min = 10,max = 140, value = 80, step=10),
                 br(),
                 h5("Built with",
                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                    "by",
                    img(src = "https://www.r-project.org/logo/Rlogo.svg", height = "25px"),
                    
                 img(src = "https://www.python.org/static/community_logos/python-logo-inkscape.svg", height = "25px")),
               
                 h5("Elaborado por José Antonio Taquía Gutiérrez")
                 ,
                 
                 helpText(a("www.taquiagutierrez.com",target="_blank", href="https://www.taquiagutierrez.com")
                 )
                 
                 
                 ),
                 mainPanel(
                     
                     tags$label(h3('Status/Output')),#column(4)),
                     leafletOutput("newmapOptimizado"),
                     hr(),
                     tableOutput("values")
                     
                 )
                 
                 
                 
                 ),
        tabPanel(" Asignacion Recursos ",
                 
                 sidebarPanel(
                     tags$small(paste0(
                         "Se ejecuta codigo en python para asignacion de recursos de ayuda"
                         
                     )), hr(),
                     actionButton("submitbutton100", "Gurobi optimizer button", class = "btn-primary"),
                     #actionButton("submitbutton100", "Opt Python", class = "btn btn-primary"),
                     hr(),
                     sliderInput("bins3","Centros de respuesta:",min = 1,max = 5, value = 3),
                     hr(),
                     sliderInput("bins4","Sectores afectados:",min = 10,max = 140, value = 12, step=5),
                     br(),
                     h5("Built with",
                        img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                        "by",
                        img(src = "https://www.r-project.org/logo/Rlogo.svg", height = "25px"),
                        
                        img(src = "https://www.python.org/static/community_logos/python-logo-inkscape.svg", height = "25px")),
                     
                     h5("Elaborado por José Antonio Taquía Gutiérrez")
                     ,
                     
                     helpText(a("www.taquiagutierrez.com",target="_blank", href="https://www.taquiagutierrez.com")
                     )
                     
                     
                 ),
                 mainPanel(
                     
                     textOutput("valuesPython")
                     
                     #tableOutput("valuesPython")
                     
                 )
                 
                 
                 )
        
       
        
          )
    )




server = function(input, output) {
    # output$txtout <- renderText({
    #   paste(input$txt, input$slider, format(input$date), sep = ", ")
    # })
    
    
    
     
output$plot<-renderPlot({
        
  #      setwd("D:/DOCUMENTOS/UNIVERSIDAD_DE_LIMA/IDIC/2021/humanitariaLogistics/modeloOptimizadoLogisticaHumanitaria/shinyModeloOptimizadoReactivo")
      
        demandaGrupo2 <- read_excel("demandaGrupoAsignacion3.xlsx",sheet = "Hoja1")
        dataLocales<- read_excel("geolocalizacionAsignacion3Centros.xlsx")
        
        rows1<-length(na.omit(demandaGrupo2$Address))
        rows2<-length(na.omit(dataLocales$Address))
        
        
        # Initial list:
        myList <- list()
        
        # Now the new experiments
        for(i in 1:rows1){
            element<-c(demandaGrupo2$Longitud[i],demandaGrupo2$Latitud[i])
            myList[[length(myList)+1]] <- list(element)
        }
        
        # Initial list:
        myListLocales <- list()
        # Now the new experiments
        for(i in 1:input$bins5){
            element<-c(dataLocales$lng[i],dataLocales$lat[i])
            myListLocales[[length(myListLocales)+1]] <- list(element)
        }
        
        length(myListLocales)
        localAsignado<-c()
        fueraRango<-c()
        asignacion<-list()
        valores<-list()
        i=1
        
        
        while(i<=length(myList)) {
            
            detectaMin<-c()
            
            #este loop calcula todas las distancia de UN sector a cada local
            for (j in 1:length(myListLocales)){
                valor<-distm(myListLocales[[j]][[1]], myList[[i]][[1]], fun = distHaversine) #one half the versed sine of a given angle or arc
                #The shortest distance between two points (i.e., the 'great-circle-distance' or 'as the crow flies'), according to the 'haversine method'. 
                #This method assumes a spherical earth, ignoring ellipsoidal effects.
                #The versine or versed sine is a trigonometric function found in some of the earliest (Vedic Aryabhatia I) trigonometric tables. The versine of an angle is 1 minus its cosine.
                if (valor<input$bins6){
                #There are several related functions, most notably the coversine and haversine. The latter, half a versine, is of particular importance in the haversine formula of navigation.
                detectaMin<-c(detectaMin,valor)}
                else{
                   
                     fueraRango<-c(fueraRango,i)
                    
                    }
                
                
            }
            
          
                localAsignado<-which.min(detectaMin)
                asignacion[[length(asignacion)+1]]<-list(localAsignado)
                
                if (i<length(myList)){
                    valor1<-distm(myListLocales[[localAsignado]][[1]], myList[[i]][[1]], fun = distHaversine)
                    valores[[length(valores)+1]]<-list(valor1) # solo guarda las distancias minimas de los que cumplen la condicion
                }
                
                
       
          i=i+1
            
            # Seleccionamos la lista respectiva del local para guardar el valor de distancia minimo contra la tienda
            # Cada local tiene su propia lista de minimos que servira para encontrar el radio del mapa de calor
            #CREAMOS una lista nueva "valores" solo con los minimos en la secuencia de la lista "asignacion"
            #el objetivo es luego reconstruir por cada numero de local las listas con sus valores minimos
           
        }
         
        
        
        valores2<-unlist(asignacion)
        locales_con_demanda<-list()
        locales_alcance<-list()
        radios<-list()
        nuevos_valores_min<-list()
        nuevos_valores_max<-list()
        resultados_min<-0
        resultados_max<-0
        vacios<-0
        
        for (i in 1:length(myListLocales)){
            
            if (length(which(valores2==i))!=0){
                
                locales_con_demanda[[length(locales_con_demanda)+1]]<-i
                resultados_min<-min(unlist(valores[valores2==i])) # busca la distancia minima entre los indices de todos los valores guardados en la lista "valores"
                resultados_max<-max(unlist(valores[valores2==i])) # busca la distancia maxima entre los indices de todos los valores guardados en la lista "valores"
                radio<-round(resultados_max-resultados_min, digits = 0)
                
                nuevos_valores_min[[length(nuevos_valores_min)+1]]<-resultados_min
                nuevos_valores_max[[length(nuevos_valores_max)+1]]<-resultados_max
                radios[[length(radios)+1]]<-radio
            }
            
            else {vacios<-vacios+1}
            
        }
        
        
        
        locales_alcance <- dataLocales %>% 
            slice(unlist(locales_con_demanda))
        
        
        locales_alcance$intensidad<-unlist(radios)
        
        intensidad_maxima<-max(unlist(locales_alcance$intensidad))
        
        locales_alcance<-data.frame(locales_alcance)
        
        list1<-1:nrow(locales_alcance)
        str(list1)
        list2 <- rep(intensidad_maxima,length(list1))
        locales_alcance<- cbind(locales_alcance, list2)
        locales_alcance$intensidad<-round(locales_alcance$intensidad/locales_alcance$list2*100, digits = 0)
        #str(locales_alcance)
        
        
        CentrosAtencion <- unlist(asignacion, use.names = FALSE)
      #str(lst2)  
        # Basic histogram
       # ggplot(data.frame(lst2), aes(lst2)) + geom_histogram()
        
        h <-hist(CentrosAtencion,ylim=c(0,160),main="Cantidad Sectores por Centro",breaks=7, col="cyan")
        text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
    })
    
    

    
    
    output$files <- renderTable(input$upload)
    
    
    
    
   
    output$valuesPython <- renderText({ 
        #use_python("C:/Users/Intel/Anaconda3/envs/optimization/python.exe")
        #py_version()
        use_virtualenv("optimization")
        source_python("NcentrosNsectoresExportarDatos1.py")
        
        if (input$submitbutton100>0) { 
            
            
            py_capture_output(crea_demanda(input$bins4,input$bins3))
   
            
        } else {
            
           # py_capture_output(crea_demanda(input$bins4,input$bins3))
            
            }
        
    })
    
    output$newmap <- renderLeaflet({
      
      #setwd("D:/DOCUMENTOS/UNIVERSIDAD_DE_LIMA/IDIC/2021/humanitariaLogistics/modeloOptimizadoLogisticaHumanitaria/shinyModeloOptimizadoReactivo")
      
      #demandaGrupo1 <- read_excel("demandaGrupo1.xlsx")
      demandaGrupo2 <- read_excel("demandaGrupoAsignacion3.xlsx",sheet = "Hoja1")
      dataLocales<- read_excel("geolocalizacionAsignacion3Centros.xlsx")
      
      rows1<-length(na.omit(demandaGrupo2$Address))
      rows2<-length(na.omit(dataLocales$Address))
      
      
      # Initial list:
      myList <- list()
      
      # Now the new experiments
      for(i in 1:rows1){
        element<-c(demandaGrupo2$Longitud[i],demandaGrupo2$Latitud[i])
        myList[[length(myList)+1]] <- list(element)
      }
      
      #
      
      
      # Initial list:
      myListLocales <- list()
      # Now the new experiments
      for(i in 1:input$bins5){
        element<-c(dataLocales$lng[i],dataLocales$lat[i])
        myListLocales[[length(myListLocales)+1]] <- list(element)
      }
      myListLocales
      
      length(myListLocales)
      localAsignado<-c()
      asignacion<-list()
      valores<-list()
      i=1
      
      
      while(i<=length(myList)) {
        
        detectaMin<-c()
        for (j in 1:length(myListLocales)){
          valor<-distm(myListLocales[[j]][[1]], myList[[i]][[1]], fun = distHaversine) #one half the versed sine of a given angle or arc
          #The shortest distance between two points (i.e., the 'great-circle-distance' or 'as the crow flies'), according to the 'haversine method'. 
          #This method assumes a spherical earth, ignoring ellipsoidal effects.
          #The versine or versed sine is a trigonometric function found in some of the earliest (Vedic Aryabhatia I) trigonometric tables. The versine of an angle is 1 minus its cosine.
          
          #There are several related functions, most notably the coversine and haversine. The latter, half a versine, is of particular importance in the haversine formula of navigation.
         if (valor<input$bins6){
          
           detectaMin<-c(detectaMin,valor)  
        
            }
           
          
          
        }
        
        #if (valor<12000){
          
          localAsignado<-which.min(detectaMin)
          asignacion[[length(asignacion)+1]]<-list(localAsignado)
          
          if (i<length(myList)){
            valor1<-distm(myListLocales[[localAsignado]][[1]], myList[[i]][[1]], fun = distHaversine)
            valores[[length(valores)+1]]<-list(valor1) # solo guarda las distancias minimas de los que cumplen la condicion
          }
          
          
        #}
        
        
        # Seleccionamos la lista respectiva del local para guardar el valor de distancia minimo contra la tienda
        # Cada local tiene su propia lista de minimos que servira para encontrar el radio del mapa de calor
        #CREAMOS una lista nueva "valores" solo con los minimos en la secuencia de la lista "asignacion"
        #el objetivo es luego reconstruir por cada numero de local las listas con sus valores minimos
        i=i+1
      }
      
      valores2<-unlist(asignacion)
      locales_con_demanda<-list()
      locales_alcance<-list()
      radios<-list()
      nuevos_valores_min<-list()
      nuevos_valores_max<-list()
      resultados_min<-0
      resultados_max<-0
      vacios<-0
      
      for (i in 1:length(myListLocales)){
        
        if (length(which(valores2==i))!=0){
          
          locales_con_demanda[[length(locales_con_demanda)+1]]<-i
          resultados_min<-min(unlist(valores[valores2==i])) # busca la distancia minima entre los indices de todos los valores guardados en la lista "valores"
          resultados_max<-max(unlist(valores[valores2==i])) # busca la distancia maxima entre los indices de todos los valores guardados en la lista "valores"
          radio<-round(resultados_max-resultados_min, digits = 0)
          
          nuevos_valores_min[[length(nuevos_valores_min)+1]]<-resultados_min
          nuevos_valores_max[[length(nuevos_valores_max)+1]]<-resultados_max
          radios[[length(radios)+1]]<-radio
        }
        
        else {vacios<-vacios+1}
        
      }
      
      
      
      locales_alcance <- dataLocales %>% 
        slice(unlist(locales_con_demanda))
      
      
      locales_alcance$intensidad<-unlist(radios)
      
      intensidad_maxima<-max(unlist(locales_alcance$intensidad))
      
      locales_alcance<-data.frame(locales_alcance)
      
      list1<-1:nrow(locales_alcance)
      list2 <- rep(intensidad_maxima,length(list1))
      locales_alcance<- cbind(locales_alcance, list2)
      locales_alcance$intensidad<-round(locales_alcance$intensidad/locales_alcance$list2*100, digits = 0)
      #str(locales_alcance)
      
      
      #https://www.supplychaindataanalytics.com/es/mapas-de-calor-con-leaflet-en-r/
      
      # definir el centro del mapa
      lat_center<-mean(dataLocales$lat) 
      long_center <- mean(dataLocales$lng) 
      
      
         leaflet(locales_alcance) %>% 
            addTiles() %>% 
            addProviderTiles(providers$OpenStreetMap.DE) %>% 
            setView(long_center,lat_center,14) %>% addHeatmap(lng=~locales_alcance$lng,lat=~locales_alcance$lat, intensity= locales_alcance$intensidad,max=100,radius=20,blur=10)
        
    })
    
    output$newmap2 <- renderLeaflet({
      
      #setwd("D:/DOCUMENTOS/UNIVERSIDAD_DE_LIMA/IDIC/2021/humanitariaLogistics/asignacionLogisticaHumanitariaDemandaAplicacionEnLinea")
      
      #demandaGrupo1 <- read_excel("demandaGrupo1.xlsx")
      demandaGrupo2 <- read_excel("demandaGrupoAsignacion3.xlsx",sheet = "Hoja1")
      dataLocales<- read_excel("geolocalizacionAsignacion3Centros.xlsx")
      
      rows1<-length(na.omit(demandaGrupo2$Address))
      rows2<-length(na.omit(dataLocales$Address))
      
      
      # Initial list:
      myList <- list()
      
      # Now the new experiments
      for(i in 1:rows1){
        element<-c(demandaGrupo2$Longitud[i],demandaGrupo2$Latitud[i])
        myList[[length(myList)+1]] <- list(element)
      }
      
      #
      
      
      # Initial list:
      myListLocales <- list()
      # Now the new experiments
      for(i in 1:input$bins5){
        element<-c(dataLocales$lng[i],dataLocales$lat[i])
        myListLocales[[length(myListLocales)+1]] <- list(element)
      }
      myListLocales
      
      length(myListLocales)
      localAsignado<-c()
      asignacion<-list()
      valores<-list()
      i=1
      
      
      while(i<=length(myList)) {
        
        detectaMin<-c()
        for (j in 1:length(myListLocales)){
          valor<-distm(myListLocales[[j]][[1]], myList[[i]][[1]], fun = distHaversine) #one half the versed sine of a given angle or arc
          #The shortest distance between two points (i.e., the 'great-circle-distance' or 'as the crow flies'), according to the 'haversine method'. 
          #This method assumes a spherical earth, ignoring ellipsoidal effects.
          #The versine or versed sine is a trigonometric function found in some of the earliest (Vedic Aryabhatia I) trigonometric tables. The versine of an angle is 1 minus its cosine.
          if(valor<input$bins6){
          #There are several related functions, most notably the coversine and haversine. The latter, half a versine, is of particular importance in the haversine formula of navigation.
          detectaMin<-c(detectaMin,valor)  
          }
        }
        
       # if (valor<input$bins6){
         
          localAsignado<-which.min(detectaMin)
          asignacion[[length(asignacion)+1]]<-list(localAsignado)
          
          if (i<length(myList)){
            valor1<-distm(myListLocales[[localAsignado]][[1]], myList[[i]][[1]], fun = distHaversine)
            valores[[length(valores)+1]]<-list(valor1) # solo guarda las distancias minimas de los que cumplen la condicion
          }
          
          
     #   }
        
        
        # Seleccionamos la lista respectiva del local para guardar el valor de distancia minimo contra la tienda
        # Cada local tiene su propia lista de minimos que servira para encontrar el radio del mapa de calor
        #CREAMOS una lista nueva "valores" solo con los minimos en la secuencia de la lista "asignacion"
        #el objetivo es luego reconstruir por cada numero de local las listas con sus valores minimos
        i=i+1
      }
      
      valores2<-unlist(asignacion)
      locales_con_demanda<-list()
      locales_alcance<-list()
      radios<-list()
      nuevos_valores_min<-list()
      nuevos_valores_max<-list()
      resultados_min<-0
      resultados_max<-0
      vacios<-0
      
      for (i in 1:length(myListLocales)){
        
        if (length(which(valores2==i))!=0){
          
          locales_con_demanda[[length(locales_con_demanda)+1]]<-i
          resultados_min<-min(unlist(valores[valores2==i])) # busca la distancia minima entre los indices de todos los valores guardados en la lista "valores"
          resultados_max<-max(unlist(valores[valores2==i])) # busca la distancia maxima entre los indices de todos los valores guardados en la lista "valores"
          radio<-round(resultados_max-resultados_min, digits = 0)
          
          nuevos_valores_min[[length(nuevos_valores_min)+1]]<-resultados_min
          nuevos_valores_max[[length(nuevos_valores_max)+1]]<-resultados_max
          radios[[length(radios)+1]]<-radio
        }
        
        else {vacios<-vacios+1}
        
      }
      
      
      
      locales_alcance <- dataLocales %>% 
        slice(unlist(locales_con_demanda))
      
      
      locales_alcance$intensidad<-unlist(radios)
      
      intensidad_maxima<-max(unlist(locales_alcance$intensidad))
      
      locales_alcance<-data.frame(locales_alcance)
      
      list1<-1:nrow(locales_alcance)
      list2 <- rep(intensidad_maxima,length(list1))
      locales_alcance<- cbind(locales_alcance, list2)
      locales_alcance$intensidad<-round(locales_alcance$intensidad/locales_alcance$list2*100, digits = 0)
      #str(locales_alcance)
      
      
      #https://www.supplychaindataanalytics.com/es/mapas-de-calor-con-leaflet-en-r/
      
      # definir el centro del mapa
      lat_center<-mean(dataLocales$lat) 
      long_center <- mean(dataLocales$lng) 
      
         leaflet(locales_alcance) %>% 
            addTiles() %>% 
            addCircleMarkers(lng = ~demandaGrupo2$Longitud, lat = ~demandaGrupo2$Latitud)%>%
            addProviderTiles(providers$OpenStreetMap.DE) %>% 
            setView(long_center,lat_center,14) %>% addHeatmap(locales_alcance$lng,lat=~locales_alcance$lat, intensity= locales_alcance$intensidad,max=100,radius=40,blur=20)
        
    })
    
###################################################
    output$values <- renderTable({
        
        if (input$submitbuttonOptmizado>0) { 
        
        cantidad_centros = input$bins
        demandaGrupo2 <- read_excel("demandaGrupoAsignacion3.xlsx",sheet = "Hoja1")
        demandaGrupo2 = demandaGrupo2[1:input$bins2,]
        valorDemanda<-demandaGrupo2$demand
        #create a distance matrix between the demand points and DCs
        customer_dc_distmat <- geosphere::distm(
            x=cbind(demandaGrupo2$Longitud,demandaGrupo2$Latitud),
            y=cbind(dataLocales$lng,dataLocales$lat)) %>% 
            #convert from meters (default) to kilometers
            measurements::conv_unit('m','km')
        row.names(customer_dc_distmat) = paste0('Customer_',demandaGrupo2$Address)
        colnames(customer_dc_distmat) = paste0('DC_',dataLocales$Address)
        
        #create a matrix that is the metric you wish to minimize or maximize.
        #in this example, the metric is unit-miles
        #just multiply the demand by city, into the distance from that city to each DC
        unitmiles_customer_dc_matrix <- 
            valorDemanda * customer_dc_distmat
        
        customer_count <- nrow(demandaGrupo2)
        dc_option_count <- nrow(dataLocales)
        
        #now make optimization model
        dc_location_model <- ompr::MIPModel() %>%
            #binary decision variables: for each customer, which DC to align to?  Yes/no decisions, align Customer A to DC B yes, or no?
            add_variable(customer_dc_align[customerindex,dcindex],
                         customerindex=1:customer_count,
                         dcindex=1:dc_option_count,type='binary') %>%
            #binary decision variable: open a DC or no?
            add_variable(open_dc_binary[dcindex],dcindex=1:dc_option_count,type='binary') %>%
            #first constraint: each customer aligned to 1 and only 1 DC
            add_constraint(sum_expr(customer_dc_align[customerindex,dcindex],
                                    dcindex=1:dc_option_count)==1,
                           customerindex=1:customer_count) %>%
            #add in "Big M" constraints that activate open_dc_binary when
            #any customers are aligned to a DC
            add_constraint(sum_expr(customer_dc_align[customerindex,dcindex],
                                    customerindex=1:customer_count)<=
                               99999*open_dc_binary[dcindex],dcindex=1:dc_option_count) %>%
            
            #limit the number of opened DCs to EXACTLY 2
            add_constraint(sum_expr(open_dc_binary[dcindex],dcindex=1:dc_option_count)==cantidad_centros) %>%
            #set objective function, the sumproduct
            #of the customer/DC alignment integer variables,
            #and the matrix of the unit-miles for each customer/DC pair
            #sense is either "min" or "max", minimize or maximize the values?
            set_objective(sum_expr(customer_dc_align[customerindex,dcindex]*
                                       unitmiles_customer_dc_matrix[customerindex,dcindex],
                                   customerindex=1:customer_count,
                                   dcindex=1:dc_option_count),sense='min')
        
        solution <- ompr::solve_model(dc_location_model,with_ROI(solver = "glpk"))
        
        customer_dc_alignment_df <- get_solution(solution,customer_dc_align[customerindex,dcindex]) %>%
            dplyr::filter(value==1) %>%
            dplyr::select(customerindex,dcindex) %>%
            #add in customer and DC names and lat/long
            dplyr::mutate(Customer_City = demandaGrupo2$Address[customerindex],
                          Customer_Lat = demandaGrupo2$Latitud[customerindex],
                          Customer_Lng = demandaGrupo2$Longitud[customerindex],
                          DC_City = dataLocales$Address[dcindex],
                          DC_Lat = dataLocales$lat[dcindex],
                          DC_Lng = dataLocales$lng[dcindex]) %>%
            dplyr::select(Customer_City,Customer_Lat,Customer_Lng,
                          DC_City,DC_Lat,DC_Lng)
        
        #verify each Customer City is only present once - meaning that it's only aligned to a single DC
        #table(customer_dc_alignment_df$Customer_City)
        
        #verify only two DCs selected, should be Dallas and Houston
        Centros_Atencion<- unique(customer_dc_alignment_df$DC_City)
        
        data.frame(Centros_Atencion)
        
        } else {
            
            
        }
        
    })
    
    
    
################################################
        output$newmapOptimizado <- renderLeaflet({
           
            
            
            if (input$submitbuttonOptmizado>0) { 
                cantidad_centros = input$bins
                demandaGrupo2 <- read_excel("demandaGrupoAsignacion3.xlsx",sheet = "Hoja1")
                demandaGrupo2 = demandaGrupo2[1:input$bins2,]
                valorDemanda<-demandaGrupo2$demand
                #create a distance matrix between the demand points and DCs
                customer_dc_distmat <- geosphere::distm(
                    x=cbind(demandaGrupo2$Longitud,demandaGrupo2$Latitud),
                    y=cbind(dataLocales$lng,dataLocales$lat)) %>% 
                    #convert from meters (default) to kilometers
                    measurements::conv_unit('m','km')
                row.names(customer_dc_distmat) = paste0('Customer_',demandaGrupo2$Address)
                colnames(customer_dc_distmat) = paste0('DC_',dataLocales$Address)
                
                #create a matrix that is the metric you wish to minimize or maximize.
                #in this example, the metric is unit-miles
                #just multiply the demand by city, into the distance from that city to each DC
                unitmiles_customer_dc_matrix <- 
                    valorDemanda * customer_dc_distmat
                
                customer_count <- nrow(demandaGrupo2)
                dc_option_count <- nrow(dataLocales)
                
                #now make optimization model
                dc_location_model <- ompr::MIPModel() %>%
                    #binary decision variables: for each customer, which DC to align to?  Yes/no decisions, align Customer A to DC B yes, or no?
                    add_variable(customer_dc_align[customerindex,dcindex],
                                 customerindex=1:customer_count,
                                 dcindex=1:dc_option_count,type='binary') %>%
                    #binary decision variable: open a DC or no?
                    add_variable(open_dc_binary[dcindex],dcindex=1:dc_option_count,type='binary') %>%
                    #first constraint: each customer aligned to 1 and only 1 DC
                    add_constraint(sum_expr(customer_dc_align[customerindex,dcindex],
                                            dcindex=1:dc_option_count)==1,
                                   customerindex=1:customer_count) %>%
                    #add in "Big M" constraints that activate open_dc_binary when
                    #any customers are aligned to a DC
                    add_constraint(sum_expr(customer_dc_align[customerindex,dcindex],
                                            customerindex=1:customer_count)<=
                                       99999*open_dc_binary[dcindex],dcindex=1:dc_option_count) %>%
                    
                    #limit the number of opened DCs to EXACTLY 2
                    add_constraint(sum_expr(open_dc_binary[dcindex],dcindex=1:dc_option_count)==cantidad_centros) %>%
                    #set objective function, the sumproduct
                    #of the customer/DC alignment integer variables,
                    #and the matrix of the unit-miles for each customer/DC pair
                    #sense is either "min" or "max", minimize or maximize the values?
                    set_objective(sum_expr(customer_dc_align[customerindex,dcindex]*
                                               unitmiles_customer_dc_matrix[customerindex,dcindex],
                                           customerindex=1:customer_count,
                                           dcindex=1:dc_option_count),sense='min')
                
                solution <- ompr::solve_model(dc_location_model,with_ROI(solver = "glpk"))
                
                customer_dc_alignment_df <- get_solution(solution,customer_dc_align[customerindex,dcindex]) %>%
                    dplyr::filter(value==1) %>%
                    dplyr::select(customerindex,dcindex) %>%
                    #add in customer and DC names and lat/long
                    dplyr::mutate(Customer_City = demandaGrupo2$Address[customerindex],
                                  Customer_Lat = demandaGrupo2$Latitud[customerindex],
                                  Customer_Lng = demandaGrupo2$Longitud[customerindex],
                                  DC_City = dataLocales$Address[dcindex],
                                  DC_Lat = dataLocales$lat[dcindex],
                                  DC_Lng = dataLocales$lng[dcindex]) %>%
                    dplyr::select(Customer_City,Customer_Lat,Customer_Lng,
                                  DC_City,DC_Lat,DC_Lng)
                
                #verify each Customer City is only present once - meaning that it's only aligned to a single DC
                #table(customer_dc_alignment_df$Customer_City)
                
                #verify only two DCs selected, should be Dallas and Houston
                dc_cities_selected <- unique(customer_dc_alignment_df$DC_City)
                #dc_cities_selected
                #n<-lenght(list(dc_cities_selected))
                str(dc_cities_selected)
                
                #dc_cities_selected[4]
                
                customer_dc_alignment_df %<>% dplyr::mutate(
                    leaflet_dc_color = dplyr::case_when(DC_City==dc_cities_selected[1]~'red',
                                                        DC_City==dc_cities_selected[2]~'cyan',
                                                        DC_City==dc_cities_selected[3]~'green',
                                                        DC_City==dc_cities_selected[4]~'orange',
                                                        DC_City==dc_cities_selected[5]~'blue'))
                
                
                
                leaflet(customer_dc_alignment_df) %>% addTiles() %>%
                    addCircleMarkers(lat=~Customer_Lat,lng=~Customer_Lng,
                                     color=~leaflet_dc_color,radius=8) %>% addMarkers(data=dataLocales,lng = ~lng, lat = ~lat)
                
                
                
            } else {
                
                
            }
            
        })
        
}



# Run the application 
shinyApp(ui = ui, server = server)

