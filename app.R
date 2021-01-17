library(shiny)
library(sf) # pour manipuler des données géospatiales
library(leaflet) # pour créer des cartes interactives
library(leaflet.extras)
library(tidyverse) # pour faciliter la programmation (entre autres, opérateur "%>%")
library(DT) # pour créer des tableau interactifs

### Importer les données###

##ajouter les peuplements forestiers##
st_read("data/Peup_for.shp") -> peup_for 
if (!st_crs(peup_for)$epsg %in% c(4326)) st_transform(peup_for, 4326) -> peup_for

ui <- fluidPage(
  fluidRow(column(width=8,offset= 2,h2("Carte du lot"),
                  leafletOutput(outputId = "map",height = 600))),
  fluidRow(column(width=10,offset= 1,dataTableOutput(outputId = "tableau"))))

server <- function(input, output){
  
  ### Code pour la map###
  output$map <- renderLeaflet({leaflet() %>%
      
      ##fonction pour déterminer l'ordre des couches. les placettes par dessus la couche des peuplements##
      addMapPane("highlighted_polygon",zIndex = 410)%>%
      addMapPane("Peup",zIndex = 300)%>%
      
      ##Ajoute les polygones de peuplements et les étiquettes##
      addPolygons(data = peup_for,
                  weight = 1.5,
                  opacity = 2,
                  fillOpacity = 0,
                  options = pathOptions(pane = "Peup"),
                  layerId = peup_for$No_Peup,
                  group = "Peuplement forestier")%>%
      
      ##Ajoute les photos satellite##
      addProviderTiles('Esri.WorldImagery', group = "Satellite")%>%
      
      ##Ajoute le MNT_ombré##
      addWMSTiles("https://geoegl.msp.gouv.qc.ca/ws/mffpecofor.fcgi?", 
                  layers = "lidar_ombre", 
                  options = WMSTileOptions(format = "image/png", transparent = TRUE),group = "Relief")%>%
      
      ##ajoute le MHC##
      addWMSTiles("https://geoegl.msp.gouv.qc.ca/ws/mffpecofor.fcgi?", 
                  layers = "lidar_mhc", 
                  options = WMSTileOptions(format = "image/png", transparent = TRUE),
                  group = "Hauteur de canopée")%>%
      
      ##ajoute la légende du MHC##
      addWMSLegend(position = "bottomright",  "https://geoegl.msp.gouv.qc.ca/ws/mffpecofor.fcgi?version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=lidar_mhc&format=image/png&STYLE=default", 
                   layerId = "Hauteur de canopée")%>%
      
      ##Contrôle de la carte pour afficher les fonds de carte et les couches##
      addLayersControl(overlayGroups = c("Peuplement forestier"),
                       baseGroups= c("Satellite","Hauteur de canopée","Relief"),
                       options = layersControlOptions(collapsed = TRUE))%>%
      
      ##barre d'échelle##
      addScaleBar(position = "bottomleft")
      

    
  })
  
  ################# Tableau des peuplements ##################
  
  peup_for%>%
    st_drop_geometry %>% #supprime la géométrie
    subset(select= -OBJECTID)%>% #supprime les champs inutiles
    mutate(sup_ha= round(sup_ha,2))%>% # arrondit le champ "sup_ha"
    arrange(No_Peup)->info # trier le tableau en ordre croissant en fonction du champ "No_Peup"
  

  output$tableau<- renderDataTable({
    datatable(info,
              colnames = c("# Peuplement", "Affectation", "Groupement d'essence", "Classe de densité", "Classe de hauteur", "Classe d'âge", "Type de couvert", "Travaux suggérés","Échéancier","Superficie (ha)"),
              rownames = TRUE,
              options = list(
                pageLength = 10,
                stateSave=TRUE), 
              selection = 'single')})
  
  ## Méchanisme qui permet de mettre en rouge les peuplements selectionnés dans la carte ###
  
  row_selected <- reactive({
    input$tableau_rows_selected
  })
  
  observe({
    if(!is.null(row_selected())){
      observeEvent(input$tableau_rows_selected,
                   {row_selected = input$tableau_rows_selected
                   peup_surling<- subset(peup_for,peup_for$No_Peup==row_selected)
                   
                   leafletProxy('map')%>%
                     removeShape("highlighted_polygon") %>%
                     addPolygons(data = peup_surling,
                                 weight = 3,
                                 opacity = 100,
                                 fillOpacity = 0,
                                 color = "red",
                                 layerId = "highlighted_polygon")
                   
                   })}
    else{leafletProxy("map") %>%
        removeShape("highlighted_polygon")
    }
    
  })
  
  ## méchanisme qui permet de selectionner une rangée dans la table quand un polygone est cliqué ##  
  
  observeEvent( input$map_shape_click, { 
    clickid<-input$map_shape_click$id
    dataTableProxy("tableau") %>%
      selectRows(which(input$tableau_rows_all == clickid)) %>%
      selectPage(which(input$tableau_rows_all == clickid) %/% input$tableau_state$length + 1)
  })
}
  

shinyApp(ui = ui, server = server)
