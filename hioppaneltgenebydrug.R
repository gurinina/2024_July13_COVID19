
##############################################################################
##########################  START CALL MODULES ##############################@
##############################################################################

tabSEND = reactiveValues(

  tab = NULL)

xinput = reactive({

  req(input$site)

  site = input$site

  if(site == "marjhop"){

    xinput = d3[,phop$name]

  } else if(site == "marjhip"){

    xinput = dint

  } else {xinput = delke}

})

pdata = reactive({

  req(input$site)

  site = input$site

  if(site == "marjhop"){

    pdata = phop

  } else if(site == "elke"){

    pdata = pmarjhip

  } else {pdata = pelke}

})

respSEND = reactiveValues(resp = NULL)

threshSEND = reactiveValues(thresh = NULL)

cmpSEND = reactiveValues(cmp = NULL)

observeEvent(input$tabs, {

  tabSEND$tab = input$tabs
  cmpSEND$cmp = input$cmpSERV

}, ignoreInit = FALSE, ignoreNULL = T)

returnedMOD = callModule(visNetworkModule,'visNetwork1',xinput = xinput,
  cmpInput = reactive(cmpSEND$cmp),
  threshInput = reactive(threshSEND$thresh),
  siteInput = reactive(input$site),
  tabsInput = reactive(tabSEND$tab))

cmpRETURN = reactive({
  print(paste("cmpRETURNED",returnedMOD[[1]]()))
  returnedMOD[[2]]()})

threshRETURN = reactive({
  print(paste("threshRETURNED",returnedMOD[[2]]()))
  returnedMOD[[2]]()})

returnedSIG <- callModule(sigNetworkModule, 'sigNetworkModule1',
  xRespDat = reactive(xsig),
  xRespInput = reactive(respSEND$resp),
  inputTab  = reactive(tabSEND$tab))

sigRETURN = reactive({
  print(paste("sigRETURN", returnedSIG[[1]]()))
  returnedSIG[[1]]()})

tabRETURN = reactive({
 print(paste("tabservRETURN", returnedSIG[[2]]()))
 returnedSIG[[2]]()})

########################################################################
########################## END CALL MODULES ############################
########################################################################

########################################################################
########################## START OBSERVEEVENT ##########################
########################################################################

observeEvent(cmpRETURN(), {

  req(cmpRETURN())

  tabNOThop = input$tabs != "hiphop"

  threshMOD = threshRETURN()

  cmpMOD = cmpRETURN()

  req(cmpMOD)

  req(cmpMOD!="")

  req(!is.null(input$cmpSERV))

  cmpDIFF = cmpMOD!=input$cmpSERV

  req(tabNOThop == TRUE | tabNOThop == FALSE)

  if(cmpMOD %in% phop$name){choices = phop$name} else if(cmpMOD %in% pmarjhip$name){

    choices = pmarjhip$name}else if(cmpMOD %in% pelke$name){choices = pelke$name}

  if(cmpMOD %in% phop$name){site = "marjhop"} else if(cmpMOD %in% pmarjhip$name){

    site = "marjhip"} else if(cmpMOD %in% pelke$name){site = "elke"}


    updatePrettyRadioButtons(session,"site", label = NULL,
      c("2008 Erikson PLOS Genetics" = "elke",
        "2021 HIPHOP Marjan" = "marjhip",
        "2020:2024 HOP Marjan" = "marjhop"), selected = site)

  if(cmpMOD!=input$cmpSERV){

    updateSelectizeInput(session,"cmpSERV",

    label = NULL, choices = choices,

    selected = cmpRETURN(),server = T)}

}, ignoreInit = FALSE, ignoreNULL = T)

observeEvent(threshRETURN(), {

  req(threshRETURN())

  tabNOThop = input$tabs != "hiphop"

  threshMOD = threshRETURN()

  req(threshMOD)

  req(threshMOD!="")

  req(!is.null(input$cmpSERV))

  threshDIFF = threshMOD!=input$scorethreshSERV

  if(threshMOD!=input$scorethreshSERV){

   updateSliderInput(session, "scorethreshSERV", "",

   value = threshMOD, step = 0.1, min = 0, max = 5.0)
                 }

}, ignoreInit = FALSE, ignoreNULL = T)

observeEvent(input$cmpSERV, {

    req(input$cmpSERV)

    req(!is.null(input$tabs))

    req(input$tabs!= "")

    updateSliderInput(session, "scorethreshSERV", label = NULL,
    value = 1.0, step = 0.1, min = 0, max = 5.0)

    cmpSEND$cmp = input$cmpSERV

    threshSEND$thresh = input$scorethreshSERV

    tabSEND$tabs = input$tabs

}, ignoreInit = FALSE, ignoreNULL = T)

observeEvent(input$scorethreshSERV,{

  req(input$cmpSERV)

  cmpSEND$cmp = input$cmpSERV

  threshSEND$thresh = input$scorethreshSERV

  tabSEND$tabs = input$tabs

}, ignoreInit = FALSE, ignoreNULL = T)

observeEvent(input$site, {

  req(input$site)

  site = input$site

  req(site)

  if(site == "marjhop") {

    choices = phop$name} else if(site == "marjhip"){

    choices = pmarjhip$name} else if(site == "elke"){

    choices = pelke$name}

  if(site == "marjhop") {

    selected = "azithromycin_1.75mM"} else if(site == "marjhip"){

    selected = "23May09:doxorubicin-hydrochloride_13uM"} else if(site == "elke"){

    selected =  "sertraline hydrochloride"}

    updateSelectizeInput(session,"cmpSERV", label = NULL,

    choices = choices,selected = selected,server = T)

}, ignoreNULL = TRUE, ignoreInit = FALSE)

#######################################################
################## END OBSERVEEVENTS ##################
#######################################################

#######################################################
############# START SIGNATURE EVENTS ##################
#######################################################

output$screenResp = renderDataTable({

  w = which(phop$signature %in% sigRETURN())

  validate(need(length(w) > 0 , message =
  "please enter a valid signature"))

  df = data.frame(

  screen = phop$name[w],

  mechanism = phop$mechanism[w],

  PCID = phop$PCID[w],

  compound = phop$cmp[w],

  stringsAsFactors = F)

  opts = list(

  pageLength = 25,

  autoWidth = FALSE,

  scrollX = FALSE,

  columnDefs = list(

  list(className = 'dt-left', targets = c(0, 1, 2, 3)),

  list(width = c('100px'), targets = c(2)),

  list(width = c('500px'), targets = c(1)),

  list(width = c('300px'), targets = 0)))

  df = df %>% dplyr::arrange(compound)

  df =  DT::datatable(df,

  options = opts, rownames = FALSE, escape = FALSE,
  selection = "single") %>%

  formatStyle(c(1:4), fontWeight = 'bold', fontSize = '14px')

})

observeEvent(input$screenResp_rows_selected, {

  w = which(phop$signature %in% sigRETURN())

  validate(need(length(w) > 0 , message = "please enter a valid signature"))

  d = phop$name[w]

  df = data.frame(

  screen = phop$name[w],

  mechanism = phop$mechanism[w],

  PCID = phop$PCID[w],

  compound = phop$cmp[w],

  stringsAsFactors = F)

  df = df %>% dplyr::arrange(compound)

  row = input$screenResp_rows_selected

  w = which(df$screen %in% df$screen[row])

  validate(need(length(w) > 0 , message = "please enter a valid signature"))

  selected = df$screen[row]

  cmpSEND$cmp = selected

  updateSelectizeInput(session, "cmpSERV", label = NULL,

    choices = phop$name, selected = selected, server = T)

  newtab <- switch(input$tabs,

   "signature" = "hiphop",

   "hiphop" = "signature")

  updateTabItems(session, "tabs", newtab)

})

output$targethip <- DT::renderDataTable({

  w = which(phop$name %in% input$cmpSERV)

  d = phop[w, c(
  "name",
  "cmp",
  "signature",
  "PCID",
  "image",
  "mechanism"
  )]

  PCID = '<a href="https://pubchem.ncbi.nlm.nih.gov" target="_blank">PCID</a>'

  names(d)[5] = "structure"

  names(d)[1] = "screen"

  names(d)[4] = PCID

  names(d)[2] = "compound"

  opts = list(dom = 'Bfrtip', paging = FALSE, target = "cell", searching = FALSE,
    info = FALSE, autowidth = T, scrollX = TRUE, ordering = FALSE,
    columnDefs = list(
      list(className = 'dt-left', targets = c(0,1,4)),
      list(className = 'dt-center', targets = c(2,3)),
      list(width = c('200px'), targets = c(0,2)),
      list(width = c('100px'), targets = c(1,3)),
      list(width = c('400px'), targets = c(5))))

  datatable(d, options = opts, escape = FALSE,
    class = 'table-bordered stripe table-condensed',
    rownames = F)

})

observeEvent(input$targethip_rows_selected, {

  w = which(phop$name %in% input$cmpSERV)

  row = input$targethip_rows_selected

  respSEND$resp = phop$signature[w][row]

  newtab <- switch(input$tabs,
   "hiphop" = "signature",
   "signature" = "hiphop")

  updateTabItems(session, "tabs", newtab)

})

#######################################################
############# END SIGNATURE EVENTS ####################
#######################################################

############## RESET COMPOUND #############

observeEvent(input$resetCmp,{

  site = input$site

  updateSelectizeInput(session,'cmpSERV',label = NULL,choices = NULL,
          selected = "")

  if(site == "marjhop"){

  updateSelectizeInput(session,"cmpSERV",

   label = NULL, choices = phop$name,
   selected = "azithromycin_1.75mM",server = T)

  } else if(site == "marjhip"){

  updateSelectizeInput(session,"cmpSERV", label = NULL,
   choices = pmarjhip$name,
   selected = "23May09:doxorubicin-hydrochloride_13uM",server = T)

  } else if(site == "elke"){

  updateSelectizeInput(session,"cmpSERV",
   label = NULL, choices = pelke$name,
   selected = "sertraline hydrochloride",server = T)
  }


  },ignoreInit = F, ignoreNULL = T)

##################################################
################ START MOUSE-OVERS ###############
##################################################

show_click = reactiveVal(NULL)
print_click = reactiveVal(NULL)
output$click_info <- renderUI(show_click())
output$point_info <- DT::renderDataTable({

  df = print_click()

  opts = list(dom = 'Bfrtip', paging = FALSE, target = "cell", searching = FALSE,
    info = FALSE, autowidth = TRUE, scrollX = TRUE, ordering = FALSE,
    columnDefs = list(
      list(width = c('10%'), targets = c(0, 1, 2)),
      list(width = c('70%'), targets = 3)))

   df =  DT::datatable(df, options = opts,rownames = F, escape = F, selection = "single") %>%
   formatStyle(c(1:4),fontWeight = 'bold', fontSize = '12px',target="row")

})

observeEvent(input$plot_click, {

   welke = which(colnames(delke)%in% input$cmpSERV)
   whop = which(colnames(d3) %in% input$cmpSERV)
   wboth = which(colnames(dn3) %in% input$cmpSERV)

  if(length(whop)>0) {x = d3} else if(length(wboth)>0) {x = dn3} else {x = delke}

  req(input$cmpSERV)
  w1 = which(colnames(x) == input$cmpSERV)

  req(length(w1) > 0)

  validate(need(length(w1) == 1 ,message = "please enter a valid compound"))

  hop = geneAnno(mat = x,cmp=colnames(x)[w1],fdat = fdat, xvar = F)

  pclick <- nearPoints(df = hop,  xvar = "gene", yvar = "FD", coordinfo = input$plot_click,threshold = 10)

  hideTooltip <- function(hide){
  session$sendCustomMessage(type = 'hideDraggable', message = list('hide'= hide))
  }

  g = grep("FD",names(pclick))

  if(length(g) > 0) pclick[,g] = format(round(pclick[,g],2),nsmall = 1,scientific = F)

  g = which(names(pclick) %in% c("xvar","gene"))

  if(length(g) > 0) pclick = pclick[,-g]

  if( nrow(pclick) == 0 ) {
  show_click(NULL)

  hideTooltip(TRUE) # Hide tooltip if there's no info to show
  return()

  } else {

  session$sendCustomMessage(type = 'placeDraggable', message = list())
  show_click(tagList(

  {DT::dataTableOutput("point_info", width = "100%")}))

  print_click({pclick})

  }

})

show_eclick = reactiveVal(NULL)
print_eclick = reactiveVal(NULL)
output$eclick_info <- renderUI(show_eclick())
output$point_einfo <- DT::renderDataTable({

  df = print_eclick()

 opts = list(dom = 'Bfrtip', paging = FALSE, target = "cell", searching = FALSE,
    info = FALSE, autowidth = TRUE, scrollX = TRUE, ordering = FALSE,
    columnDefs = list(
      list(width = c('10%'), targets = c(0, 1, 2)),
      list(width = c('70%'), targets = 3)))

  df =  DT::datatable(df, options = opts,rownames = F, escape = F, selection = "single") %>%
    formatStyle(c(1:4),fontWeight = 'bold', fontSize = '12px',target="row")

})

observeEvent(input$plot_eclick, {

  x = de3

  req(input$cmpSERV)

  w1 = which(colnames(x) == input$cmpSERV)

  req(length(w1) > 0)

  validate(need(length(w1) == 1 ,message = "please enter a valid compound"))

  hideTooltip <- function( hide ){
  session$sendCustomMessage(type = 'ehideDraggable', message = list('hide'=hide))
  }

  hop =  geneAnno(mat = x, cmp=colnames(x)[w1],fdat = fdat,xvar=F)

  peclick <- nearPoints(df = hop,  xvar = "gene", yvar = "FD", coordinfo = input$plot_eclick,threshold = 5)

  g = grep("FD",names(peclick))

  if(length(g) > 0) peclick[,g] = format(round(peclick[,g],2),nsmall = 1,scientific = F)

  g = which(names(peclick) %in% c("xvar","gene"))

  if(length(g) > 0) peclick = peclick[,-g]

  if( nrow(peclick) == 0 ) {
    show_eclick(NULL)

    hideTooltip(TRUE) # Hide tooltip if there's no info to show
    return()


 } else {

  session$sendCustomMessage(type = 'eplaceDraggable', message = list())
  show_click(tagList(

  {DT::dataTableOutput("point_info", width = "100%")}))

  print_click({peclick})

  }

})

####################################################################### #######################################################################
#######################  END ALL MOUSEOVVER ###########################
#######################################################################
#######################################################################


####################################################################### #######################################################################
####################### START GENEBYDRUG #############################
#######################################################################
#######################################################################

data_for_genebydrug <- reactive({

  req(input$gene)

  gene = toupper(input$gene)

  xdat = xinput()

  df = pdata()

  w = which(rownames(xdat) == gene)

  validate(need(length(w) == 1 ,message = "please enter a valid gene"))

  mx2 = mymeltdf(xdat,row = gene,df = df)

  print(c("mx2",nrow(mx2)))

  mx2

})

##################################################
################ GENE BY SCREEN PLOT #############
##################################################

output$genebydrug <- renderPlot({

  mx2 = data_for_genebydrug()

  med = median(mx2$fitness_defect)

  tit = toupper(input$gene)

  mx2$fitness_defect = round(mx2$fitness_defect,2)

  mx2$sig = 0

  mx2$shape = 19

  g = which(mx2$fitness_defect >= 15)

  if(length(g) > 0) mx2$sig[g] = 17

  wx = which(mx2$fitness_defect >= 1)

  if(length(wx) > 0) mx2$sig[wx] = 1

  g  = ggplot(mx2,aes(x =  screen,y=fitness_defect,col = factor(sig))) + theme_bw() +
    geom_point(aes(col = factor(sig),shape = factor(shape),size = 15)) +
    scale_shape_manual(values = c(17,19))

  g1 = g + theme(legend.position="none") +
    theme(panel.grid.minor =   element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(axis.ticks = element_blank(), axis.text.x =   element_blank())

  g1 = g1 + labs(y = "fitness defect score") + labs(x = "compound") +
    geom_hline(yintercept=median(mx2$fitness_defect),color = "black",
    linetype = "dashed",size =1) + ggtitle(tit)

  g2 = g1 + geom_text_repel(size = 6,
    data = subset(mx2, sig == TRUE),
    aes(x =  screen,y = fitness_defect, label = screen),
    point.padding = 0.25,
    segment.alpha = 0.2, col = "black")

  g2 = g2 + theme(axis.text=element_text(size=18),
    axis.title=element_text(size=24,face="bold"),
    plot.title = element_text(size = 24,face = "bold"))

    g2})

####################################################################### #######################################################################
####################### START BRUSH PTS ###############################
#######################################################################
#######################################################################

observeEvent(input$brushpts, {

  mx2 = data_for_genebydrug()

  mx2$fitness_defect = round(mx2$fitness_defect,2)

  pts = brushedPoints(mx2, input$brushpts, xvar = "screen", yvar = "fitness_defect")

  output$tabpts = DT::renderDataTable({

  pts

  }, escape = F,rownames = F, selection = "single",server = F)


  choices = pts$screen


},ignoreInit = T,ignoreNULL = F)

##################################################
################ GENEBYSCREEN ##################
##################################################

observeEvent(input$tabpts_rows_selected, {

  row = input$tabpts_rows_selected

  req(input$brushpts)

  mx2 = data_for_genebydrug()

  mx2$fitness_defect = round(mx2$fitness_defect,2)

  pts = brushedPoints(mx2, input$brushpts, xvar = "screen", yvar = "fitness_defect")

  selected = pts$screen[row]

  site = input$site

  req(site)

  updatePrettyRadioButtons(session,"site", label = NULL,
               c("2008 Erikson PLOS Genetics" = "elke",
                 "2021 HIPHOP Marjan" = "marjhiphop",
                 "2020:2024 HOP Marjan" = "marjhop"),
                  selected = site)

  if(selected%in% phop$name) {
    choices = phop$name} else if(selected%in% pmarjhip$name) {
    choices = pmarjhip$name}else if(selected%in% pelke$name) {
    choices = pelke$name}

  updateSelectizeInput(session,"cmpSERV",label = NULL,
                       choices = choices, selected = selected,server = T)

  newtab <- switch(input$tabs,
                   "genebyscreen" = "hiphop",
                   "hiphop" = "genebyscreen")

  updateTabItems(session, "tabs", newtab)

})

####################################################################### #######################################################################
####################### END BRUSH PTS #################################
#######################################################################
#######################################################################

output$geneinfo  <- renderDataTable({

   w = which(fdat$sgd_gene %in% rownames(dint))

  fd = fdat[w,] %>% distinct(sgd_gene,.keep_all = T)

   xdat = xinput()

 w = which(orth$best_human_gene %in% rownames(xdat))


  f = fd[w1,c("sgd_gene","link","descriptor")]

  names(f)[1] = c("GENE")

  f

 fd = orth[w,] %>% distinct(best_human_gene,.keep_all = T)

  w1 = which(fd$best_human_gene %in% toupper(input$gene))

  validate(
    need(length(w1) == 1 ,message =
           "please enter a valid gene"))

  f = fd[w1,c("best_human_gene","genecards_link","descriptor")]

  names(f)[1] = c("GENE")

  f

},escape = F,options=list(paging = F,searching = F,

  info = F,scrollX = T,scrollY=TRUE,scrollCollapse=TRUE,autowidth=TRUE),

  rownames = F)

####################################################################### #######################################################################
######################### END GENEBYDRUG ##############################
#######################################################################
