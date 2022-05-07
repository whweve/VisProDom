#' @title VisProDom
#' @description  plot gene structure and protein domain for given gene across species
#' @author Hongwei Wang <\email{whweve@163.com}>
#' @export
#' @import shinydashboard shiny ggplot2 rintrojs dplyr data.table 
library(shiny)
library(shinydashboard)
library(ggplot2)
library(rintrojs)
library(dplyr)
library(data.table)
library(markdown)
ui <- dashboardPage(
  skin = "red",
    dashboardHeader(
    title = h3("VisProDom",style = "color: white;text-align: left;margin-left:6px"),
    titleWidth = 360,
    dropdownMenu(
      type = "notifications", 
      headerText = strong("HELP"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      notificationItem(
        text = "Questions can be sent to the email address whweve@163.com",
        icon = icon("email")
      )
    ),
    tags$li(
      a(
        strong("ABOUT"),
        height = 40,
        href = "https://github.com/whweve/VisProDom/blob/main/README.md",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),
  dashboardSidebar(
    #disable=TRUE,
    width = 360,
    introjsUI(),
	#tags$head(
    tags$style(HTML(".sidebar {
                      height:95vx; overflow-y: auto;background-color:gray
                    }")
      ),
    #)
	conditionalPanel(
    condition = "input.tabvals == 1",
	#includeMarkdown("www/home.md")
    dashboardSidebar(disable = TRUE),
	dashboardSidebar(collapsed = TRUE)
	#updateSidebar()
	),
    conditionalPanel(
	  #dashboardSidebar(disable = TRUE),
      condition = "input.tabvals == 2",
	    div(class = "inlay", style = "background-color: red;width:360",
        introBox(
          actionButton("submit1",h4("Introduction to VisProDom",style="color:red")),
          data.step = 1,
          data.intro = "A quick introduction to VisProDom",
          style = "background-color: red;text-align: left;margin-left:36px"
        )
    ),
    br(),
      sidebarMenu(
        introBox(data.step = 2, data.intro = "read in gff file",
                 menuItem(startExpanded=TRUE,
                          h4("* Step 1: Open file",style = "color: white;text-align: left;margin-left:6px"),
                          selectInput("datasource","Datasource:",
                                      choices = c("Import"="readgffcheck",
                                                  "Load"="loadgffcheck"),
                                      selected = "loadgffcheck"),
                          uiOutput("setgffnumber"),
                          uiOutput("readgfffile"),
                          uiOutput("loadgfffile")
                 ),
                 style = "background-color: gray;text-align: left;margin-left:20px"
        ),
        introBox(data.step = 3, data.intro = "Set transcript or domain nanes",
                 menuItem(startExpanded=TRUE,
                          h4("* Step 2: Keyword",style = "color: white;text-align: left;margin-left:6px"),
                          selectInput("keywordsource","Keyword:",
                                      choices = c("Keyword"="setkeyword",
                                                  "Domain"="setpfam"),
                                      selected = "setpfam"),
                          uiOutput("selectpfam"),
                          uiOutput("selectkeyword"),
                          uiOutput("checkdomainrelation"),
                          uiOutput("submitkeyword")
                 ),
                 style = "background-color: gray;text-align: left;margin-left:20px"
        ),
        introBox(data.step = 4, data.intro = "Modify colours of the genomic features",
                 menuItem(startExpanded=TRUE,
                          h4("step 3: Colour and size",style = "color: white;text-align: left;margin-left:6px"),
                          uiOutput("setcolourandsize")
                 ),
                 style = "background-color: gray;text-align: left;margin-left:20px"
        ),
        introBox(data.step = 5, data.intro = "label the name of transcript",
                 menuItem(startExpanded=TRUE,
                          h4("step 4: Transcript name",style = "color: white;text-align: left;margin-left:6px"),
                          uiOutput("showtransname")
                 ),
                 style = "background-color: gray;text-align: left;margin-left:20px"
        ),
        introBox(data.step = 6, data.intro = "Label the name of domain",
                 menuItem(startExpanded=TRUE,
                          h4("step 5: Domain name",style = "color: white;text-align: left;margin-left:6px"),
                          checkboxInput("displaydomain",label = "Domain",TRUE),
                          uiOutput("showdomainname")
                 ),
                 style = "background-color: gray;text-align: left;margin-left:20px"
        ),
        introBox(data.step = 7, data.intro = "Selective print domain name",
                 menuItem(startExpanded=TRUE,
                          h4("step 6: Highlight domain",style = "color: white;text-align: left;margin-left:6px"),
                          textAreaInput("domainhighlight","highlight domain name",NULL)
                 ),
                 style = "background-color: gray;text-align: left;margin-left:20px"
        ),
        introBox(data.step = 8, data.intro = "Export the image with width, heigt and resolution",
                 menuItem(startExpanded=TRUE,
                          h4("step 7: Export image",style = "color: white;text-align: left;margin-left:6px"),
                          selectInput("outputtype","export:",
                                      choices = c("Genic feature"="savegenic",
                                                  "Transcript feature"="savetrans",
                                                  "CDS feature"="savecds",
                                                  "Summary file"="savesummary"),
                                      selected = "savegenic"),
                          uiOutput("exportgenic"),
                          uiOutput("exporttrans"),
                          uiOutput("exportcds"),
                          uiOutput("exportsummary")
                 ),
                 style = "background-color: gray;text-align: left;margin-left:20px"
        )
      )
    ),
    conditionalPanel(
      condition = "input.tabvals == 3",
      sidebarMenu(
        #introBox(data.step = 10, data.intro = "Import Gff file",
        menuItem(
		  #icon = icon("table"),
          startExpanded=TRUE,
          h4("Open Gff file",style = "color: white;background-color: gray;text-align: left;margin-left:6px"),
          fileInput("gffreadin","gff file",
                    accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
		  style = "background-color: gray;text-align: left;margin-left:20px"
        ),
        #),
        #introBox(data.step = 12, data.intro = "Import Gff file",
        menuItem(
          startExpanded=TRUE,
          h4("Genome Region",style = "color: white;background-color: gray;text-align: left;margin-left:6px"),
          uiOutput("chrom1region"),
		  style = "background-color: gray;text-align: left;margin-left:20px"
        ),
        style = "background-color: gray;text-align: left;margin-left:20px",
        #),
        #introBox(data.step = 13, data.intro = "Colour and size",
        menuItem(
          startExpanded=TRUE,
          h4("Colour and size",style = "color: white;background-color: gray;text-align: left;margin-left:6px"),
          uiOutput("chrom1colsize"),
		  style = "background-color: gray;text-align: left;margin-left:20px"
        ),
        style = "background-color: gray;text-align: left;margin-left:20px",
        #),
        #introBox(data.step = 14, data.intro = "Export",
        menuItem(
          startExpanded=TRUE,
          h4("Save",style = "color: white;background-color: gray;text-align: left;margin-left:6px"),
          uiOutput("exportgffvisualizer"),
		  style = "background-color: gray;text-align: left;margin-left:20px"
        ),
        style = "background-color: gray;text-align: left;margin-left:20px"
        #)
      )
    )
  ),
  dashboardBody(
      ### changing theme
    #shinyDashboardThemes(
    #  theme = "blue_gradient"
    #),
	tags$head(
      tags$style(HTML(".sidebar {
                      height: 95vh;overflow-y: auto;background-color:gray
                    }")
      )
    ),
    tabBox(
      id='tabvals',
      width=NULL,
      #introBox(data.step = 9, data.intro = "The output image, which can be zoomed-in or -out",
               tabPanel(value=2,
                        h4("Genic feature visualizer"),
                        plotOutput("genicvisualizer",height = "auto",
                                   dblclick = "plot1_dblclick",
                                   brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE)
								   )
               ),
               tabPanel(value=2,
                        h4("Transcript feature visualizer"),
                        plotOutput("transvisualizer",height = "auto",
                                   dblclick = "plot1_dblclick",
                                   brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE)
								   )
               ),
               tabPanel(value=2,
                        h4("CDS visualizer"),
                        plotOutput("cdsvisualizer",height = "auto",
                                   dblclick = "plot1_dblclick",
                                   brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE)
								   )
               ),
               tabPanel(value=2,
                        h4(span(tagList(icon("table"),"Summary"))),
                        dataTableOutput("Summary")
               ),
			   tabPanel(value=1,
                        h4(span(tagList(icon("home"),"Home"))),
						#tags$iframe(path = 'www/home.md', # put testdoc.html to /www
                        #            width = '100%', 
						#			height = '800px', 
						#			frameborder = 0, 
						#			scrolling = 'auto'),
	                    includeMarkdown(path="www/home.md")
			           #tags$iframe(height="820px")
                 ),
               tabPanel(value=4,
                        h4(span(tagList(icon("table"),"Summary"))),
                        dataTableOutput("Summary1")
               )
      )
    )
  )

