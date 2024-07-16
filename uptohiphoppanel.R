# sunday_2024_COVID19 works

library(shiny)
library(dplyr)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(shinydashboard)
library(bcrypt)
library(visNetwork)
library(shinycssloaders)
library(igraph)
library(ggplot2)
library(ggrepel)
library(shinyjs)
library(shinyBS)
library(stringr)
library(shinyjqui)

options(rsconnect.max.bundle.size=3145728000)

source(file.path("modules", "2022_March13_GOENRICH.R"))
source(file.path("modules" ,"2022_March13_functions.R"))
source(file.path("modules" ,"2024_April10_signature.R"))
source(file.path("modules" ,"2024_April11_visNetwork.R"))

fdat      <-     readRDS("24Jan8/2021_July19_fdat.RDS")

xsig      <-     readRDS("24Jan8/x21.RDS")

phop      <-     readRDS("24Jan8/phop.RDS")
pmarjhip    <-   readRDS("24Jan8/phip.RDS")
pelke     <-     readRDS("24Jan8/pelke.RDS")

d3        <-     readRDS("24Jan8/d3.RDS")
de3       <-     readRDS("24Jan8/de3.RDS")
dn3       <-     readRDS("24Jan8/dn3.RDS")
dint      <-     readRDS("24Jan8/dint.RDS")
delke     <-     readRDS("24Jan8/delke.RDS")

xcoinh      <-   readRDS("24Jan8/xcoinh.RDS")
xpval       <-   readRDS("24Jan8/xcoinh_pval.RDS")
xcofit      <-   readRDS("24Jan8/xcofit.RDS")
xpfit       <-   readRDS("24Jan8/xcofit_pval.RDS")

xcohh    <-      readRDS("24Jan8/xhiphop_coinh.RDS")
xpvhh    <-      readRDS("24Jan8/xhiphop_coinh_pval.RDS")
xfithh   <-      readRDS("24Jan8/xhiphop_cofit.RDS")
xpithh   <-      readRDS("24Jan8/xhiphop_cofit_pval.RDS")

xelke    <-      readRDS("24Jan8/xelke.RDS")
xelkepv     <-   readRDS("24Jan8/xelke_pval.RDS")
xefit       <-   readRDS("24Jan8/xelke_cofit.RDS")
xefitpv     <-   readRDS("24Jan8/xelke_cofit_pval.RDS")

d3          <-   d3[,phop$name]
de3         <-   de3[,pmarjhip$name]
dn3         <-   dn3[,pmarjhip$name]
dint        <-   dint[,pmarjhip$name]=
delke       <-   delke[,pelke$name]

noness = fdat%>% filter(essential == "noness")
ess = fdat%>% filter(essential == "ess")

st = rev(seq(0,1,0.05))

sq = rev(seq(0,1,0.001))

##########################

header <-  dashboardHeader(title =  span("Chemogenomic Profiling COVID19",
           style = "font-weight:bold; font-size:18px; text-align: right;"),
           titleWidth = 1200)

sidebar <- dashboardSidebar(width = 220,
  tags$style(HTML(".main-sidebar {
  background-color: #000066; color: #fff; font-weight:bold}
  .treeview-menu>li>a {font-weight:bold; color: #fff;}")),

sidebarMenu(id = 'tabs',selected = "HOmozygous Profiles",

menuItem("HOmozygous Profiles", tabName="hiphop",icon = icon("bullseye"), selected = T),

menuItem('GO enrichments', tabName = 'goenrich', icon = icon('dna')),

menuItem('Signatures', tabName = 'signature', icon = icon('database')),

menuItem("Gene fitness profiles", tabName="genebyscreen", icon = icon("chart-line"))))


body <-  dashboardBody(tags$style(HTML(".main-sidebar { font-size: 16px;
  background-color: #000066; color: #fff; font-weight:bold}
 .treeview-menu>li>a { font-size: 16px;  font-weight:bold; color: #fff;}")),

tags$style("#controls {
    background-color: #dcd0ff;opacity: 1;}
    #controls:hover{opacity: 1;}
    #econtrols {
    background-color: #dcd0ff;opacity: 1;};  }"),

tags$style("#controls {
    background-color: #dcd0ff;opacity: 1;}
    #econtrols:hover{opacity: 1;}
    #econtrols {
    background-color: #dcd0ff;opacity: 1;};  }"),

tags$head(tags$style(HTML("#point_einfo tr.selected {background-color:white},
    #point_info tr.selected {background-color:white}"))),

#makes pngs same size as window after rescaling
tags$script("$(document).on('shiny:connected', function(event) {
    var myWidth = $(window).width();
    Shiny.onInputChange('shiny_width',myWidth)});"),

useShinyjs(),

tags$script("$(document).on('shiny:connected', function(event) {
  var myHeight = $(window).height();
  Shiny.onInputChange('shiny_height',myHeight)});"),

tags$script(func <- JS('function(event, ui){return $(event.target).offset();}')),

tags$script(HTML("
  //Get mouse coordinates
  var mouseX, mouseY;$(document).mousemove(function(e) {
  #mouseX = e.pageX;
  #mouseY = e.pageY;}).mouseover();
  //Function to position draggable, place on current mouse coordinates
  Shiny.addCustomMessageHandler ('placeDraggable',function (message) {
  var element = $('#click_info').parent();
  #element.css({'top': mouseY + 'px', 'left' : mouseX + 'px'})});

  //Show or hide draggable
  Shiny.addCustomMessageHandler ('hideDraggable',function (message) {
  if(message.hide == true){
  $('#click_info').parent().hide();} else{
  $('#click_info').parent().show();}});

  //Get mouse coordinates
  var mouseX, mouseY;$(document).mousemove(function(e) {
  #emouseX = e.pageX;
  #emouseY = e.pageY;}).mouseover();
  //Function to position draggable, place on current mouse coordinates
  Shiny.addCustomMessageHandler ('placeDraggable',function (message) {
  var element = $('#click_info').parent();
  #element.css({'top': mouseY + 'px', 'left' : mouseX + 'px'})});

    //Show or hide draggable
  Shiny.addCustomMessageHandler ('hideDraggable',function (message) {
  if(message.hide == true){
  $('#eclick_info').parent().hide();} else{
  $('#eclick_info').parent().show();}});

  //Show or hide draggable
  Shiny.addCustomMessageHandler ('ehideDraggable',function (message) {
  if(message.hide == true){
  $('#eclick_info').parent().hide();} else{
  $('#eclick_info').parent().show();}}),")),

tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "chemogenomics.css")),

shinyjs::useShinyjs(),

tags$style(HTML(".selectize-input{white-space: nowrap;
  font-size: 14px; font-weight:bold!important;")),


##############################################################################
#############################  START TABITEMS ################################
##############################################################################


tabItems(

  tabItem(tabName = "genebyscreen",

fluidRow(

    column(width = 3,

      box(title = "Enter gene of interest:",

        textInput(inputId = "gene", label = NULL, value   = "ARG4"),

      status = "primary", solidHeader = T, width = "100%", height = 250),

    column(width = 9,

      box(title = "Gene descriptor and function:",

        dataTableOutput('geneinfo'),

      status = "primary", solidHeader = T, width = "100%", height = 250)))), # ok

fluidRow(

    column(width = 12,

      box(title = "Fitness defect score across conditions:",

        h5("click OR drag to select screens of interest;
          corresponding profiles can be foundin compound menu
          on hiphop tab; cyan: p-value < 0.001; triangle:
          screens identifying drug target candidates"),

        plotOutput("genebydrug",

          click = clickOpts(id = "clickpts"),

          brush = brushOpts(id = "brushpts"), height = 800),

        status = "primary", solidHeader = T, width = "100%"))), # ok

fluidRow(

    box(title = "Experimental detail; select row to view corresponding

      HOP chemogenomic profile",

      dataTableOutput("tabpts"),

     status = "primary", solidHeader = T, width = 12, height = 250)), # ok

fluidRow(

    box(title = "GO enrichment network: select a node to view details;
      right click to save image",

      width = 8, status = "primary", solidHeader = TRUE, height = 800,

      uiOutput("GOterms"),

      visNetworkOutput("network_Proxy", width = "85%", height = 652)),

    box(title = "GO term set enrichment details:",

        width = 4,

        uiOutput("GoTable"),

    solidHeader = T, status = "primary", height = 200),

    box(title = "Top-contributing genes:",

      width = 4, br(),

      uiOutput("leadingEdge"),

    solidHeader = T, status = "primary", height = 580)), # ok

fluidRow(

    box(title = "GO enrichment network: select a node to view details;
      right click to save image",

      width = 8, status = "primary", solidHeader = TRUE, height = 800,

      uiOutput("GOterms"),

      visNetworkOutput("network_Proxy", width = "85%", height = 652)),

    box(title = "GO term set enrichment details:",

        width = 4,

        uiOutput("GoTable"),

    solidHeader = T, status = "primary", height = 200),

    box(title = "Top-contributing genes:",

      width = 4, br(),

      uiOutput("LeadingEdge"),

    solidHeader = T, status = "primary", height = 580)), # ok

fluidRow(

    box(title = "GO enrichment table:",

      DT::dataTableOutput("EnrichTable"),

    status = "primary", background = "navy",solidHeader = T, width = 12)), # ok

fluidRow(

    width = 3,

    box(align = "center", title = "Download CoFitness:",

      br(),

      downloadButton("downloadcofitness", "HOP CoFitness"),

    status = "primary", background = "navy",solidHeader = T, width = "100%", height = 150), # box ok

    column(width = 3,

      box(align = "center", title = "Download EnRichments:",

        br(),

        downloadButton("enrichdownload", "GO EnRichments"),

     status = "primary", background = "navy",solidHeader = T,

     width = "100%", height = 150)))), # tab ok

tabItem("hiphop", class = "active",

fluidRow(

    column(width = 3,

      box(title = "Select input datasets:",

        br(),

        prettyRadioButtons("site", label = NULL,

        choices = c(

          "2008 Erikson PLOS Genetics" = "elke",
          "2021 HIPHOP Marjan" = "marjhip",
          "2020:2024 HOP Marjan" = "marjhop"),

           outline = T, fill = FALSE, thick = T, shape = "square",

           bigger = FALSE, selected = "marjhop", inline = F),

      status = "primary", background = "navy",solidHeader = T, width = "100%", height = 160)),

    column(width = 9,

      box(title = "Select screen:",

        selectizeInput('cmpSERV', label = NULL,

          choices = phop$name, multiple = F, selected = "azithromycin_1.75mM",

          options = list('plugins' = list('autofill_disable'))),

      status = "primary", solidHeader = T, width = "100%", height = 160))), # fluidRow ok

fluidRow(

  column(width = 3,

      box(title = "Fitness score threshold:",

      fluidRow(

          column( align = "center", width = 12,

          sliderInput("scorethreshSERV", label = NULL, min = 0, value = 1.0,

            step = 0.1, max = 5.0))), # sliderInput ok

    status = "primary", solidHeader = T, width = "100%", height = 200)), # column ok

  column(width = 3,

    box(align = "center", title = "Reset compound menu:",

      br(),

        actionButton("resetCmp", "Reset cmpMenu"),

    status = "primary", solidHeader = T, width = "100%", height = 200)), # column ok

  column(width = 3,

     uiOutput("ylimits")), # fluidRow ok

     uiOutput("hiphoppanel")), # tab ok

tabItem("signature",

fluidRow(sigNetworkModuleUI("sigNetworkModule1")),

fluidRow(

    box(title = "HOP profiles in response signature:",

      dataTableOutput("screenResp"),

    status = "primary", solidHeader = T, width = "100%", height = 12))),

tabItem("goenrich",

fluidRow(visNetworkModuleUI("visNetwork1")))))

ui<-dashboardPage(header, sidebar, body, skin = "blue",
  setBackgroundColor(color = "#e6e6ff",shinydashboard = TRUE))

# server <- function(input, output, session) {

##############################################################################
########################### START HIPHOPPANEL ################################
##############################################################################

output$hiphoppanel  = renderUI({

  welke  = which(colnames(delke)%in% input$cmpSERV)
  whop   = which(colnames(d3) %in% input$cmpSERV)
  wboth  = which(colnames(dint) %in% input$cmpSERV)

  options <-  list(shiny = list(abs_position = list(
  dragcreate = func, # send returned value back to shiny when interaction is created.
  drag = func)))# send returned value to shiny when dragging.

if(length(whop) > 0){

tabItem("HOP",

fluidRow(

    box(title = "Signature & compound information:",

    HTML("<h5><b>Click datatable row to view response signature:</b></h5>"),

      DT::dataTableOutput("targethip"),

    status = "primary", solidHeader = T, width = 12)), # fluidRow ok

fluidRow(

    box(title = "HOP profile: mouse click on points to view gene description",

      plotOutput("nfd", width = "90%", height = 800,

      click = clickOpts(id = "plot_click")),

    status = "primary", solidHeader = T, width = 12),

    jqui_draggable(absolutePanel(id = "controls", width = 600,
    draggable = TRUE,uiOutput("click_info")),
    options = list(cancel = ".shiny-input-container"))),# fluidRow ok

fluidRow(

    box(title="HOP genes: mouse click on row links to gene fitness profile;

      mouse click on genename links to SGD",

    dataTableOutput("hoptab"),

    status = "primary", solidHeader = T,width = 12), # fluidRow ok

   box(title="download HOP:",

     column(width =6,

     downloadButton("downloadhop", "HOPdownload")),

     column(width = 6,

       downloadButton("ExportnonPlot", "HOPfitness plot")),

    status = "primary", solidHeader = T,width = 12)),

fluidRow(

    box(title="Compound information: CoInhibitory (CoI) value,
      pvalue, PubChem ID (CID) link, Mechanism of Action (MoA) link,
      indication and mechanism",

      h5(strong("Mouse click on row to view CoInhibitory (CoI)
        Homozygous Profile (HOP) and corresponding GO enrichments in next tab"),

      br(),

         dataTableOutput('coInhib'),

    status = "primary", solidHeader = T,width = 12)),

fluidRow(align = "center",

    box(title="download CoInhibition:",

      downloadButton('downloadcoinhib', 'CoInhibition'),

  status = "primary", solidHeader = T,width = 3)))) # tabItem ok

} else if(length(wboth) > 0){

tabItem("HIPHOP",

fluidRow(

    box(title = "HIP fitness profile:",

      plotOutput("efd", width = "90%",height = 800,

      click = clickOpts(id="plot_eclick")),

    status = "primary", solidHeader = T, width = 6),

    box(title = "HOP fitness profile:
      mouse click on points to view gene description",

      plotOutput("nfd", width = "90%", height = 800,

      click = clickOpts(id="plot_click")),

    status = "primary", solidHeader = T, width = 6),

    jqui_draggable(absolutePanel(id = "controls", width = 600,
    draggable = TRUE,uiOutput("click_info")),
    options = list(cancel = ".shiny-input-container"))), # fluidRow oK

fluidRow(

    box(title="HIP genes: mouse click on row links to gene fitness profile",

      dataTableOutput("hiptab", width = "100%"),

    status = "primary", solidHeader = T,width = 6),

    box(title="HOP genes: mouse click on genename links to SGD",

      dataTableOutput("hoptab",width = "100%"),

    status = "primary", solidHeader = T, width = 6)), # fluidRow oK

fluidRow(

    box(title="download HIP:",

      column(width = 6,

        downloadButton("downloadhip", "HIPdownload")),

      column(width = 6,

      downloadButton("ExportessPlot", "HIPfitness plot")),

    status = "primary", solidHeader = T,width = 6),

   box(title="download HOP:",

     column(width =6,

     downloadButton("downloadhop", "HOPdownload")),

     column(width = 6,

     downloadButton("ExportnonPlot", "HOPfitness plot")),

    status = "primary", solidHeader = T,width = 12)), # fluidRow oK

fluidRow(

    box(title="Compound information: CoInhibitory (CoI) value,
      pvalue, PubChem ID (CID) link, Mechanism of Action (MoA) link,
      indication and mechanism",

      h5(strong("Mouse click on row to view CoInhibitory (CoI)
        Homozygous Profile (HOP) and corresponding GO enrichments in next tab"),

      br(),

      dataTableOutput('coInhib'),

    status = "primary", solidHeader = T,width = 12)), # fluidRow ok

    box(title="download HOP:",

      column(width = 6,

        downloadButton("downloadhop", "HOPdownload")),

      column(width = 6,

       downloadButton("ExportnonPlot", "HOPfitness plot")),

   status = "primary", solidHeader = T,width = 6)), # fluidRow ok

fluidRow(

     box(title="Compound information: CoInhibitory (CoI) value,
       pvalue, PubChem ID (CID) link, Mechanism of Action (MoA) link,
       indication and mechanism",

        h5(strong("Mouse click on row to view CoInhibitory (CoI)
          Homozygous Profile (HOP) and corresponding GO enrichments in next tab")),

          dataTableOutput('coInhib'),status = "primary", solidHeader = T, width = 12)),

fluidRow(align = "center",

    box(title="download CoInhibition:",

      downloadButton('downloadcoinhib', 'CoInhibition'),

    status = "primary", solidHeader = T,width = 3))) # tabItem ok

} else if(length(welke) > 0){

tabItem ("HOPELKE",

fluidRow(

    box(title = "Signature & compound information:",

    HTML("<h5><b>Click datatable row to view response signature:</b></h5>"),

      DT::dataTableOutput("targethip"),

    status = "primary", solidHeader = T, width = 12)), # fluidRow ok

fluidRow(

    box(title = "HOP profile: mouse click on points to view gene description",

      plotOutput("nfd", width = "90%", height = 800,

      click = clickOpts(id = "plot_click")),

    status = "primary", solidHeader = T, width = 12),

    jqui_draggable(absolutePanel(id = "controls", width = 600,
    draggable = TRUE,uiOutput("click_info")),
    options = list(cancel = ".shiny-input-container"))),# fluidRow ok

fluidRow(

    box(title="HOP genes: mouse click on row links to gene fitness profile;

      mouse click on genename links to SGD",

    dataTableOutput("hoptab"),

    status = "primary", solidHeader = T,width = 12), # fluidRow ok

   box(title="download HOP:",

     column(width =6,

     downloadButton("downloadhop", "HOPdownload")),

     column(width = 6,

       downloadButton("ExportnonPlot", "HOPfitness plot")),

    status = "primary", solidHeader = T,width = 12)),

fluidRow(

    box(title="Compound information: CoInhibitory (CoI) value,
      pvalue, PubChem ID (CID) link, Mechanism of Action (MoA) link,
      indication and mechanism",

      h5(strong("Mouse click on row to view CoInhibitory (CoI)
        Homozygous Profile (HOP) and corresponding GO enrichments in next tab"),

      br(),

         dataTableOutput('coInhib'),

    status = "primary", solidHeader = T,width = 12)),

fluidRow(align = "center",

    box(title="download CoInhibition:",

      downloadButton('downloadcoinhib', 'CoInhibition'),

  status = "primary", solidHeader = T,width = 3)))) # tabItem ok

  }})

##############################################################################
########################### END HIPHOPPANEL ##################################
##############################################################################

output$hiphoppanel  = renderUI({
  welke = which(colnames(delke)%in% input$cmpSERV)
  whop = which(colnames(d3) %in% input$cmpSERV)
  wboth = which(colnames(dint) %in% input$cmpSERV)


  options <-  list(shiny = list(abs_position = list(
    dragcreate = func, # send returned value back to shiny when interaction is created.
    drag = func)))# send returned value to shiny when dragging.

  if(length(whop) > 0){

tabItem("HOP",

fluidRow(

  box(
    title = "Signature & compound information:",
    status = "primary",

    HTML(
      "<h5><b>Click datatable row to view response signature:</b></h5>"),


    solidHeader = T,
    width = 12,
    DT::dataTableOutput("targethip"))

  ),


fluidRow(

  box(title = "HOP profile: mouse click on points to view gene description",
      status = "primary",
      plotOutput("nfd", width = "90%",height = 800,
     click = clickOpts(id="plot_click")),
      solidHeader = T,width = 12),

  if(input$site == "marjhop") {jqui_draggable(
    absolutePanel(id = "controls", width = 600,
      draggable = TRUE,uiOutput("click_info")),
    options = list(cancel = ".shiny-input-container"))}
),

fluidRow(
  box(title="HOP genes: mouse click on row links to gene fitness profile;
      mouse click on genename links to SGD",
      dataTableOutput("hoptab"),
      status = "primary", solidHeader = T,width = 12)
),
fluidRow(


  box(title="download HOP:",
      column(width =6,
 downloadButton("downloadhop", "HOPdownload")
      ),

      column(width = 6,
 downloadButton("ExportnonPlot", "HOPfitness plot")),
      status = "primary", solidHeader = T,width = 12)
),

fluidRow(

  box(title="Compound information: CoInhibitory (CoI) value,
    pvalue, PubChem ID (CID) link, Mechanism of Action (MoA) link,
    indication and mechanism",
    h5(strong("Mouse click on row to view CoInhibitory (CoI)
      Homozygous Profile (HOP) and corresponding GO enrichments in next tab")),
    br(),
    dataTableOutput('coInhib'),status = "primary", solidHeader = T,width = 12)

),#fluid row

fluidRow(align = "center",

         box(title="download CoInhibition:",
 downloadButton('downloadcoinhib', 'CoInhibition'),
 status = "primary", solidHeader = T,width = 3)

))#fluidRow tabItem

  } else if(length(wboth) > 0){

    tabItem("HIPHOP",


fluidRow(

  box(title = "HIP fitness profile:",status = "primary",
      plotOutput("efd", width = "90%",height = 700,
     click = clickOpts(id="plot_eclick")),

      solidHeader = T,width = 6),


  jqui_draggable(absolutePanel(id = "econtrols", width = 600,
                                           draggable = TRUE,uiOutput("eclick_info")),
                             options = list(cancel = ".shiny-input-container")),



              box(title = "HOP fitness profile:",status = "primary",
                  plotOutput("nfd", width = "90%",height = 700,
                             click = clickOpts(id="plot_click")),

                  solidHeader = T,width = 6),

              jqui_draggable(
                absolutePanel(id = "controls", width = 600,
                              draggable = TRUE,uiOutput("click_info")),
                options = list(cancel = ".shiny-input-container"))
            ),


            fluidRow(
              box(title="HIP genes: mouse click on row links to gene fitness profile",
                  dataTableOutput("hiptab",width = "100%"),
                  status = "primary", solidHeader = T,width = 6),

              box(title="HOP genes: mouse click on genename links to SGD",
                  dataTableOutput("hoptab",width = "100%"),
                  status = "primary", solidHeader = T,width = 6)
            ),

            fluidRow(
              box(title="download HIP:",
                  column(width = 6,
                    downloadButton("downloadhip", "HIPdownload")),

                  column(width = 6,
                   downloadButton("ExportessPlot", "HIPfitness plot")),
                  status = "primary", solidHeader = T,width = 6),


              box(title="download HOP:",
                column(width = 6,
                  downloadButton("downloadhop", "HOPdownload")
                  ),

                  column(width = 6,
                  downloadButton("ExportnonPlot", "HOPfitness plot")),
             status = "primary", solidHeader = T,width = 6)
            ),

            fluidRow(
              box(title="Compound information: CoInhibitory (CoI) value,
                  pvalue, PubChem ID (CID) link, Mechanism of Action (MoA) link, indication & mechanism",
                  h5(strong("Mouse click on row to view CoInhibitory (CoI)
                            HaploInsufficiency Profile (HIP) & Homozygous Profile (HOP)
                            and corresponding GO enrichments in next tab")),
                  br(),
                  dataTableOutput('coInhib'),status = "primary", solidHeader = T,width = 12)
            ),#fluid row

            fluidRow(align = "center",
              box(title="download CoInhibition:",
                downloadButton('downloadcoinhib', 'CoInhibition'),
                status = "primary", solidHeader = T,width = 3)
            )
    ) #tab item
  } else if(length(welke) > 0){

    tabItem ("HOPELKE",


             fluidRow(

               box(title = "HOmozygous Profile (HOP): mouse click on points to view gene description",status = "primary",
                   plotOutput("nfd", width = "90%",height = 800,
                              click = clickOpts(id="plot_click")),
                   solidHeader = T,width = 12),

               if(input$site == "elke") {jqui_draggable(
                 absolutePanel(id = "controls", width = 600,
                               draggable = TRUE,uiOutput("click_info")),
                 options = list(cancel = ".shiny-container"))},

             ),

             fluidRow(

               box(title="HOP genes: mouse click on row links to gene fitness profile; mouse click on genename links to SGD",

                   dataTableOutput("hoptab"),
                   status = "primary", solidHeader = T,width = 12)
             ),
             fluidRow(

               box(title="download HOP:",
                   column(width =6,
                          downloadButton("downloadhop", "HOPdownload")
                   ),

                   column(width = 6,
                          downloadButton("ExportnonPlot", "HOPfitness plot")),
                   status = "primary", solidHeader = T,width = 12)
             ),

             fluidRow(

               box(title="Compound information:
                CoInhibitory (CoI) value, pvalue, PubChem ID (CID) link,
                Mechanism of Action (MoA) link, indication and mechanism",
                h5(strong("Mouse click on row to view CoInhibitory (CoI)
                  Homozygous Profile (HOP) and corresponding GO enrichments in next tab")),
                br(),
                dataTableOutput('coInhib'),
                status = "primary", solidHeader = T,width = 12)
             ),#fluid row

             fluidRow(align = "center",

                      box(title="download CoInhibition:",
                          downloadButton('downloadcoinhib', 'CoInhibition'),
                          status = "primary", solidHeader = T,width =3 )

             ))
  }
})

