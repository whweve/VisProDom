#' @title VisProDom
#' @description  plot gene structure and protein domain for given gene across species
#' @author Hongwei Wang <\email{whweve@163.com}>
#' @export
#' @import shiny ggplot2 rintrojs dplyr data.table fuzzyjoin fresh shinyBS
library(shiny)
library(ggplot2)
library(rintrojs)
library(dplyr)
library(data.table)
library(markdown)
library(fresh)
library(shinyBS)
library(plotly)
dreamRs <- create_theme(
  theme = "default",
  #bs_vars_global(
  #  grid_columns = 30
  #),
  bs_vars_navbar(
    default_bg = "#112446",
    default_color = "#FFFFFF",
    default_link_color = "#FFFFFF",
    default_link_active_color = "#FFFFFF",
    default_link_hover_color = "#A4A4A4"
  )
  #bs_vars_nav(link_padding = "5px 25px"),
  
)

ui <-  navbarPage(
  header = tagList(
    use_theme(dreamRs)
  ),
  title = "VisProDom",
  introjsUI(),
  id="navbar",
  tabPanel(
    "Graph",
    sidebarLayout(
      #width=3,
      #box(style='width:400px;overflow-x: scroll;height:400px;overflow-y: scroll;',
      sidebarPanel(
        width = 4,
        style = "overflow-y:auto; max-height: 100vh; position:relative;",
        div(class = "well", style = "height:100%;width:100%;colour:red",
            introBox(
              div(style = "margin-left:30%",
                bsButton("submit1",h4("VisProDom Tour"),style="background-color:blue",icon=icon("circle-play"))
              ),
              data.step = 1,
              data.intro = "Introduction to VisProDom. Update the graph once modifications have made.",
              style = "background-color: red;text-align: middle;margin-left:0px"
            )
        ),
        br(),
        #br(),
        div(class = "well", style = "height:100%;width:100%;",
            introBox(data.step = 2, data.intro = "read in gff file",
                     h4("* Step 1: Open file",style = "color: black;text-align: left;margin-left:6px"),
                     selectInput("datasource","Datasource:",
                                 choices = c("Import"="readgffcheck",
                                             "Load"="loadgffcheck"),
                                 selected = "loadgffcheck"),
                     uiOutput("setgffnumber"),
                     uiOutput("readgfffile"),
                     uiOutput("loadgfffile"),
                     style = "background-color: white;text-align: left;margin-left:20px"
            )
        ),
        br(),
        #br(),
        div(class = "well", style = "height:100%;width:100%;",
            introBox(data.step = 3, data.intro = "Set transcript or domain nanes",
                     h4("* Step 2: Keyword",style = "color: black;text-align: left;margin-left:6px"),
                     selectInput("keywordsource","Keyword:",
                                 choices = c("Keyword"="setkeyword",
                                             "Domain"="setpfam"),
                                 selected = "setpfam"),
                     uiOutput("selectpfam"),
                     uiOutput("selectkeyword"),
                     uiOutput("checkdomainrelation"),
                     uiOutput("submitkeyword"),
                     style = "background-color: white;text-align: left;margin-left:20px"
            )
        ),
        br(),
        #br(),
        div(class = "well", style = "height:100%;width:100%;",
            introBox(data.step = 4, data.intro = "Modify colours of the genomic features",
                     h4("step 3: Colour and size",style = "color: black;text-align: left;margin-left:6px"),
                     uiOutput("setcolourandsize"),
                     style = "background-color: white;text-align: left;margin-left:20px"
            )
        ),
        br(),
        #br(),
        div(class = "well", style = "height:100%;width:100%;",
            introBox(data.step = 5, data.intro = "label the name of transcript",
                     h4("step 4: Transcript name",style = "color: black;text-align: left;margin-left:6px"),
                     uiOutput("showtransname"),
                     style = "background-color: white;text-align: left;margin-left:20px"
            )
        ),
        br(),
        #br(),
        div(class = "well", style = "height:100%;width:100%;",
            introBox(data.step = 6, data.intro = "Label the name of domain",
                     h4("step 5: Domain name",style = "color: black;text-align: left;margin-left:6px"),
                     checkboxInput("displaydomain",label = "Domain",TRUE),
                     uiOutput("showdomainname"),
                     style = "background-color: white;text-align: left;margin-left:20px"
            )
        ),
        br(),
        #br(),
        div(class = "well", style = "height:100%;width:100%;",
            introBox(data.step = 7, data.intro = "Selective print domain name",
                     h4("step 6: Highlight domain",style = "color: black;text-align: left;margin-left:6px"),
                     textAreaInput("domainhighlight","highlight domain name",NULL),
                     
                     style = "background-color: white;text-align: left;margin-left:20px"
            )
        ),
        br(),
        #br(),
        div(class = "well", style = "height:100%;width:100%;",
            introBox(data.step = 8, data.intro = "Export the image with width, heigt and resolution",
                     h4("step 7: Export image",style = "color: black;text-align: left;margin-left:6px"),
                     selectInput("outputtype","export:",
                                 choices = c("Genic feature"="savegenic",
                                             "Transcript feature"="savetrans",
                                             "CDS feature"="savecds",
                                             "Summary file"="savesummary"),
                                 selected = "savegenic"),
                     uiOutput("exportgenic"),
                     uiOutput("exporttrans"),
                     uiOutput("exportcds"),
                     uiOutput("exportsummary"),
                     style = "background-color: white;text-align: left;margin-left:20px"
            )
        )
		),
        mainPanel(
		  style = "overflow-y:auto;overflow-x:auto ",
          #width=9,
          #width = 9
          tabsetPanel(
            #tabPanel(
            #  h4("Genic feature visualizer"),
            #  plotOutput("genicvisualizer",
			##                height = "auto",
            #             dblclick = "plot1_dblclick",
            #             brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE)
            #  )
            #),
            #tabPanel(
            #  h4("Transcript feature visualizer"),
            #  plotOutput("transvisualizer",height = "auto",
            #            dblclick = "plot1_dblclick",
            #             brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE)
            #  )
            #),
            #tabPanel(
            #  h4("CDS visualizer"),
            #  plotOutput("cdsvisualizer",height = "auto",
            #             dblclick = "plot1_dblclick",
            #             brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE)
            #  )
            #),
			
			tabPanel(
              h4("Genic feature visualizer"),
              plotlyOutput("genicvisualizer",
			                height = "auto"
                         )
              ),
            tabPanel(
              h4("Transcript feature visualizer"),
              plotlyOutput("transvisualizer",
			  height = "auto"
              )
            ),
            tabPanel(
              h4("CDS visualizer"),
              plotlyOutput("cdsvisualizer",
			  height = "auto"
              )
            ),
			
            tabPanel(
              h4(span(tagList(icon("table"),"Summary"))),
              dataTableOutput("Summary")
            )
          )
        )
        
      
    )
	),
    tabPanel(
	"About", 
             sidebarLayout(
               sidebarPanel(
                 width = 0
               ),
               mainPanel(includeHTML("www/intro_text.html"),
                         width = 12)
             )
    ),
	navbarMenu(#div(style="margin-top:40%",
	"",
	icon=icon("power-off", class = NULL, lib = "font-awesome"),
                            tabPanel('stop',  value='stop1' ,icon=icon("stop")),
                            tabPanel('refresh',  value='refresh1',icon=icon("refresh"))
  #)
  )
  )