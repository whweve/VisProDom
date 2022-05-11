#' @title VisProDom
#' @description  plot gene structure and protein domain for given gene across genomes
#' @author Hongwei Wang <\email{whweve@163.com}>
#' @export
#' @import shiny ggplot2 rintrojs dplyr data.table fuzzyjoin fresh shinyBS plotly
library(shiny)
library(ggplot2)
library(rintrojs)
library(dplyr)
library(data.table)
library(markdown)
library(fresh)
library(shinyBS)
library(plotly)
#options(shiny.maxRequestSize=1000*1024^2,shiny.usecairo=TRUE,res=300)
options(shiny.usecairo=TRUE,res=300)
server <- function(input, output,session) {
  
  #observeEvent(input.tabvals == 1, {
  #      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  #    })
  #observe(print(input$navbar))
  observe({
      if(input$navbar == "stop1") {
        stopApp()
      }
    })
    
  observe({
      if(input$navbar == "refresh1") {
        #stopApp()
		#VisProDom::runVPDapp()
		#stopApp()
		#restartApp()
		session$reload()
      }
    })
	
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("www/intro_text.html"),
      easyClose = TRUE,
      footer = tagList(
        #style="text-align: center",
		div(style = "margin-right:33.1%;colour:green",
        actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle"),class = "danger")
		)
      )
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  # show intro tour
  observeEvent(input$intro,
               introjs(session, options = list("nextLabel" = "Continue",
                                               "prevLabel" = "Previous",
                                               "doneLabel" = "Alright. Let's go"))
  )
  
  observeEvent(input$submit1, {
  div(class="well","background-color:green",
    updateButton(
      session, 
      inputId = "submit1", 
      label = "Update VisProDom", 
      icon = icon("circle-check"), 
      style = "background-color:red")
	  )
  })
  
  output$setgffnumber <- renderUI({
    if(input$datasource=="readgffcheck") {
      list(numericInput("gffnumber","inputted gff number",1))
    } else {
      return(NULL)
    }
  })
  gfffilename <- reactive({
    if(input$datasource=="readgffcheck" & !is.null(input$gffnumber)) {
      paste0(sample(LETTERS,input$gffnumber + 1,replace = TRUE),collapse = "")
    } else {
      return(NULL)
    }
  })
  output$readgfffile <- renderUI ({
    if(input$datasource=="readgffcheck" & !is.null(input$gffnumber)) {
      aa <- list()
      for(i in seq(1,input$gffnumber)) {
        aa <- c(aa,list(
          fileInput(paste0(gfffilename(),i),paste0("gff file",i),
                    accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
        ))
      }
      aa
    } else {
      return(NULL)
    }
  })
  preassignedgfffile <- reactive({
    print(list.files())
  }
  )
  output$loadgfffile <- renderUI({
    if(input$datasource=="loadgffcheck") {
      selectInput("spevariable", "Genome:",
                  c(
                    "Aquilegia_coerulea_v3.1"="Aquilegia_coerulea_v3.1",
                    "Ananas_comosus_v3"="Ananas_comosus_v3",
                    "Arabidopsis_halleri_v1.1"="Arabidopsis_halleri_v1.1",
                    "Amaranthus_hypochondriacus_v1.0"="Amaranthus_hypochondriacus_v1.0",
                    "Amaranthus_hypochondriacus_v2.1"="Amaranthus_hypochondriacus_v2.1",
                    "Arabidopsis_lyrata_v2.1"="Arabidopsis_lyrata_v2.1",
                    "Anacardium_occidentale_v0.9"="Anacardium_occidentale_v0.9",
                    "Asparagus_officinalis_V1.1"="Asparagus_officinalis_V1.1",
                    "Arabidopsis_thaliana_Araport11.447"="Arabidopsis_thaliana_Araport11.447",
                    "Amborella_trichopoda_v1.0"="Amborella_trichopoda_v1.0",
                    "Botryococcus_braunii_v2.1"="Botryococcus_braunii_v2.1",
                    "Brachypodium_distachyon_v3.1"="Brachypodium_distachyon_v3.1",
                    "Brachypodium_distachyon_Bd21_3_v1.1"="Brachypodium_distachyon_Bd21_3_v1.1",
                    "Brachypodium_hybridum_v1.1"="Brachypodium_hybridum_v1.1",
                    "Brassica_oleracea_capitata_v1.0"="Brassica_oleracea_capitata_v1.0",
                    "Brassica_rapa_FPsc_v1.3"="Brassica_rapa_FPsc_v1.3",
                    "Brachypodium_stacei_v0.1"="Brachypodium_stacei_v0.1",
                    "Boechera_stricta_v1.2"="Boechera_stricta_v1.2",
                    "Brachypodium_sylvaticum_v1.1"="Brachypodium_sylvaticum_v1.1",
                    "Cicer_arietinum_v1.0"="Cicer_arietinum_v1.0",
                    "Citrus_clementina_v1.0"="Citrus_clementina_v1.0",
                    "Capsella_grandiflora_v1.1"="Capsella_grandiflora_v1.1",
                    "Chenopodium_quinoa_v1.0"="Chenopodium_quinoa_v1.0",
                    "Chlamydomonas_reinhardtii_v5.5"="Chlamydomonas_reinhardtii_v5.5",
                    "Capsella_rubella_v1.0"="Capsella_rubella_v1.0",
                    "Cucumis_sativus_v1"="Cucumis_sativus_v1",
                    "Chromochloris_zofingiensis_v5.2.3"="Chromochloris_zofingiensis_v5.2.3",
                    "Daucus_carota_v1.0"="Daucus_carota_v1.0",
                    "Dunaliella_salina_v1.0"="Dunaliella_salina_v1.0",
                    "Eutrema_salsugineum_v1.0"="Eutrema_salsugineum_v1.0",
                    "Fragaria_vesca_v1.1"="Fragaria_vesca_v1.1",
                    "Gossypium_hirsutum_v1.1"="Gossypium_hirsutum_v1.1",
                    "Glycine_max_Wm82.a2.v1"="Glycine_max_Wm82.a2.v1",
                    "Gossypium_raimondii_v2.1"="Gossypium_raimondii_v2.1",
                    "Helianthus_annuus_r1.2"="Helianthus_annuus_r1.2",
                    "Hordeum_vulgare_r1"="Hordeum_vulgare_r1",
                    "Kalanchoe_fedtschenkoi_v1.1"="Kalanchoe_fedtschenkoi_v1.1",
                    "Kalanchoe_laxiflora_v1.1"="Kalanchoe_laxiflora_v1.1",
                    "Lactuca_sativa_v5"="Lactuca_sativa_v5",
                    "Linum_usitatissimum_v1.0"="Linum_usitatissimum_v1.0",
                    "Musa_acuminata_v1"="Musa_acuminata_v1",
                    "Malus_domestica_v1.0"="Malus_domestica_v1.0",
                    "Manihot_esculenta_v6.1"="Manihot_esculenta_v6.1",
                    "Mimulus_guttatus_v2.0"="Mimulus_guttatus_v2.0",
                    "Marchantia_polymorpha_v3.1"="Marchantia_polymorpha_v3.1",
                    "Micromonas_pusilla_CCMP1545_v3.0"="Micromonas_pusilla_CCMP1545_v3.0",
                    "Miscanthus_sinensis_v7.1"="Miscanthus_sinensis_v7.1",
                    "Micromonas_sp._RCC299_v3.0"="Micromonas_sp._RCC299_v3.0",
                    "Medicago_truncatula_Mt4.0v1"="Medicago_truncatula_Mt4.0v1",
                    "Ostreococcus_lucimarinus_v2.0"="Ostreococcus_lucimarinus_v2.0",
                    "Oryza_sativa_v7.0"="Oryza_sativa_v7.0",
                    "Oryza_sativa_Kitaake_v3.1"="Oryza_sativa_Kitaake_v3.1",
                    "Oropetium_thomaeum_v1.0"="Oropetium_thomaeum_v1.0",
                    "Populus_deltoides_WV94_v2.1"="Populus_deltoides_WV94_v2.1",
                    "Panicum_hallii_v3.1"="Panicum_hallii_v3.1",
                    "Prunus_persica_v2.1"="Prunus_persica_v2.1",
                    "Populus_trichocarpa_v3.0"="Populus_trichocarpa_v3.0",
                    "Populus_trichocarpa_v3.1"="Populus_trichocarpa_v3.1",
                    "Porphyra_umbilicalis_v1.5"="Porphyra_umbilicalis_v1.5",
                    "Panicum_virgatum_v1.1"="Panicum_virgatum_v1.1",
                    "Phaseolus_vulgaris_v2.1"="Phaseolus_vulgaris_v2.1",
                    "Ricinus_communis_v0.1"="Ricinus_communis_v0.1",
                    "Sorghum_bicolor_v3.1"="Sorghum_bicolor_v3.1",
                    "Sorghum_bicolor_v3.2"="Sorghum_bicolor_v3.2",
                    "Sorghum_bicolor_Rio_v2.1"="Sorghum_bicolor_Rio_v2.1",
                    "Sphagnum_fallax_v0.5"="Sphagnum_fallax_v0.5",
                    "Setaria_italica_v2.2"="Setaria_italica_v2.2",
                    "Solanum_lycopersicum_ITAG2.4"="Solanum_lycopersicum_ITAG2.4",
                    "Selaginella_moellendorffii_v1.0"="Selaginella_moellendorffii_v1.0",
                    "Spirodela_polyrhiza_v2"="Spirodela_polyrhiza_v2",
                    "Salix_purpurea_v1.0"="Salix_purpurea_v1.0",
                    "Solanum_tuberosum_v4.03"="Solanum_tuberosum_v4.03",
                    "Setaria_viridis_v1.1"="Setaria_viridis_v1.1",
                    "Setaria_viridis_v2.1"="Setaria_viridis_v2.1",
                    "Triticum_aestivum_v2.2"="Triticum_aestivum_v2.2",
                    "Theobroma_cacao_v1.1"="Theobroma_cacao_v1.1",
                    "Trifolium_pratense_v2"="Trifolium_pratense_v2",
                    "Volvox_carteri_v2.1"="Volvox_carteri_v2.1",
                    "Vitis_vinifera_Genoscope.12X"="Vitis_vinifera_Genoscope.12X",
                    "Zostera_marina_v2.2"="Zostera_marina_v2.2",
                    "Zea_mays_B73_V3"="Zea_mays_B73_V3",
                    "Zea_mays_PH207_v1.1"="Zea_mays_PH207_v1.1"
                  ),
                  selected=c("Zea_mays_B73_V3","Arabidopsis_thaliana_Araport11.447"),
                  multiple = TRUE)
    } else {
      list(NULL)
    }
  })
  csvfile <- reactive({
    if(input$datasource=="loadgffcheck") {
      if(length(input$spevariable)) {
        dt <- NULL
        for(i in seq(1,length(input$spevariable))) {
          #load(paste0(input$spevariable[i],".rda"))
          dt <- rbind(dt,get(input$spevariable[i]))
        }
        dt
      }
    } else {
      if(input$datasource=="readgffcheck") {
        if(is.null(input[[paste0(gfffilename(),1)]])) {
          return(NULL)
        } else {
          dtall <- NULL
          gffreadnumber <- NULL
          for (i in seq(1,input$gffnumber)) {
            tmp <- length(input[[paste0(gfffilename(),i)]]$datapath)
            gffreadnumber <- c(gffreadnumber,tmp)
          }
          if (sum(gffreadnumber) == input$gffnumber) {
            for(i in seq(1,input$gffnumber)) {
              if(!is.null(input[[paste0(gfffilename(),i)]])) {
                dt <- data.table::fread(input[[paste0(gfffilename(),i)]]$datapath)
                dt$V4 <- as.numeric(dt$V4)
                dt$V5 <- as.numeric(dt$V5)
                dtall <- rbind(dtall,dt)
                dtall
              }
            }
            dtall
          }
        }
      }
    }
  })
  output$selectpfam <- renderUI({
    if(input$keywordsource=="setpfam") {
      list(textAreaInput("keypfam","the domain name","pfam08774"))
    }
  })
  output$selectkeyword <- renderUI({
    if(input$keywordsource=="setkeyword") {
      list(textAreaInput("keyword","the transcript name",NULL))
    }
  })
  output$checkdomainrelation <- renderUI({
    if(input$keywordsource=="setpfam") {
      if(!is.null(input$keypfam)) {
        #if(grepl("[\r\n]",input$keypfam)) {
        domainnumber1 <- gsub("\\|{2,}","|",
                              sub("\\|{0,}$|^\\|{0,}","",gsub("[\r\n]","|",input$keypfam))) %>%
          strsplit("|") %>% unlist
        domainnumber2 <- ((length(domainnumber1[domainnumber1 != "\\|"])+1)/2)
        #}
        if(!is.null(domainnumber2) & domainnumber2 > 1) {
          aa <- list({
            selectInput("domainrelation","Domain relation:",
                        choices = c("any, any background domain"="or",
                                    "all, any background domain"="and",
                                    "all, no background domain"="only"),
                        selected = "or")
          })
          return(aa)
        }
        if (!is.null(domainnumber2) & domainnumber2 == 1) {
          aa <- list({
            selectInput("domainrelation","Domain relation:",
                        choices = c("any, any background domain"="or",
                                    "all, any background domain"="and",
                                    "all, no background domain"="only"),
                        selected = "or")
          })
          return(aa)
        }
      }
    } else {
      NULL
    }
  })
  #output$checkdomainrelation <- renderUI({
  #  if(all(input$keywordsource=="setpfam",nchar(input$keypfam) > 0,!is.null(domainnumber()),domainnumber() > 1)) {
  #    selectInput("domainrelation","relation:",
  #                choices = c("and"="and",
  #                            "or"="or"),
  #                selected = "or")
  #    NULL
  #  }
  #})
  #output$submitkeyword <- renderUI({
  #  list(actionButton("submit1","submit", class = "btn-success"))
  #})
  output$setcolourandsize <- renderUI({
    if(is.null(csvfile())) {
      list(textInput("cdscol", "The colour of CDS", NULL),
           textInput("utr3col", "The colour of 3'UTR", NULL),
           textInput("utr5col", "The colour of 5'UTR", NULL),
           numericInput("cdssize", "The size of cds", NULL),
           numericInput("utrsize", "The size of utr", NULL),
           numericInput("intronsize", "The size of intron", NULL)
      )
    } else {
      list(textInput("cdscol", "The colour of CDS", "black"),
           textInput("utr3col", "The colour of 3'UTR", "gray"),
           textInput("utr5col", "The colour of 5'UTR", "gray"),
           numericInput("cdssize", "The size of cds", 3.363861-0.007426*maxsymbollength()),
           numericInput("utrsize", "The size of utr", 3.363861-0.007426*maxsymbollength()),
           numericInput("intronsize", "The size of intron", 0.5)
      )
    }
  })
  output$showtransname <- renderUI({
    if(is.null(csvfile())) {
      list(
        checkboxInput("showtransnames",label = "Set names to show or not",NULL),
        numericInput("textsize", "The size of transcript name", NULL),
        textInput("textcolour", "The colour of text", NULL)
      )
    } else {
      list(
        checkboxInput("showtransnames",label = "Set names to show or not",TRUE),
        numericInput("textsize", "The size of transcript name", 5.2-0.009901*maxsymbollength()),
        textInput("textcolour", "The colour of text", "black")
      )
    }
  })
  output$showdomainname <- renderUI({
    if(is.null(csvfile())) {
      list(
        checkboxInput("showdomainnames",label = "Set names to show or not",FALSE),
        numericInput("domaintextsize", "The size of domain name", NULL),
        textInput("domaintextcolour", "The colour of text", NULL),
        numericInput("domainsize", "The size of domain", NULL),
        selectInput("domaincolourplatte","Domain colour platte:",
                    choices = c("Accent" = "Accent",
                                "Dark2" = "Dark2",
                                "Paired" = "Paired",
                                "Pastel1" = "Pastel1",
                                "Pastel2" = "Pastel2",
                                "Set1" = "Set1",
                                "Set2" = "Set2",
                                "Set3" = "Set3",
                                "None" = "None"),
                    selected = NULL)
      )
    } else {
      list(
        checkboxInput("showdomainnames",label = "Set names to show or not",FALSE),
        numericInput("domaintextsize", "The size of domain name", 3),
        textInput("domaintextcolour", "The colour of text", "black"),
        numericInput("domainsize", "The size of domain", 3.363861-0.007426*maxsymbollength()),
        selectInput("domaincolourplatte","Domain colour platte:",
                    choices = c("Accent" = "Accent",
                                "Dark2" = "Dark2",
                                "Paired" = "Paired",
                                "Pastel1" = "Pastel1",
                                "Pastel2" = "Pastel2",
                                "Set1" = "Set1",
                                "Set2" = "Set2",
                                "Set3" = "Set3",
                                "None" = "None"),
                    selected = "None")
      )
    }
  })
  output_image_data <- eventReactive(input$submit1,{
    if(is.null(input$keyword) & is.null(input$keypfam)) {return(NULL)}
    if(input$keywordsource=="setkeyword") {
      if(nchar(input$keyword) > 0) {
        keywordlist <- gsub("\\|{2,}","|",
                            sub("\\|{0,}$|^\\|{0,}","",gsub("[\r\n]","|",input$keyword)))
        gff <- csvfile()[grep(keywordlist,csvfile()$V9),]
        gff <- gff[!is.na(gff$V9),]
        if(!is.null(gff) & dim(gff)[1] >= 1)  {
          trans <- gff[gff$V3 %in% c("exon", "CDS", "five_prime_UTR", "three_prime_UTR"), ]
          transtmp <- trans %>%  dplyr::select(V9) %>% dplyr::filter(!duplicated(V9))
          if(dim(transtmp)[1]>1) {
            cleankeywordlist <- unlist(strsplit(keywordlist,"\\|"))[unlist(strsplit(keywordlist,"\\|")) != "|"]
            transtmp <- transtmp[match(cleankeywordlist,transtmp$V9),]
          }
          transtmp$pos <- seq(dim(transtmp)[1],1)
          trans <- merge(trans,transtmp,by="V9")
          return(trans)
        }
      }
    }
    if(input$keywordsource=="setpfam") {
      if(nchar(input$keypfam) > 0) {
        if(any(is.null(input$domainrelation),input$domainrelation == "or")) {
          keypfamlist <- gsub("\\|{2,}","|",
                              sub("\\|{0,}$|^\\|{0,}","",gsub("[\r\n]","|",input$keypfam)))
          gff <- csvfile()[grep(keypfamlist, csvfile()$domain),]
          pfamgene <- unique(gff$V9)
          gff <- csvfile()[csvfile()$V9 %in% pfamgene,]
          gff <- gff[!is.na(gff$V9),]
          if(!is.null(gff) & dim(gff)[1] >= 1)  {
            trans <- gff[gff$V3 %in% c("exon", "CDS", "five_prime_UTR", "three_prime_UTR"), ]
            transtmp <- trans %>%  dplyr::select(V9) %>% dplyr::filter(!duplicated(V9))
            transtmp$pos <- seq(dim(transtmp)[1],1)
            trans <- merge(trans,transtmp,by="V9")
          }
          return(trans)
        }
        if(input$domainrelation == "and") {
          keypfamlist <- gsub("\\|{2,}","|",
                              sub("\\|{0,}$|^\\|{0,}","",gsub("[\r\n]","|",input$keypfam)))
          gff <- csvfile()[grep(keypfamlist, csvfile()$domain),]
          pfamgene <- unique(gff$V9)
          gff <- csvfile()[csvfile()$V9 %in% pfamgene,]
          gff <- gff[!is.na(gff$V9),]
          if(!is.null(gff) & dim(gff)[1] >= 1)  {
            trans <- gff[gff$V3 %in% c("exon", "CDS", "five_prime_UTR", "three_prime_UTR"), ]
            transtmp <- trans %>%  dplyr::select(V9) %>% dplyr::filter(!duplicated(V9))
            transtmp$pos <- seq(dim(transtmp)[1],1)
            trans <- merge(trans,transtmp,by="V9")
            pfamnumber <- length(unlist(strsplit(keypfamlist,"\\|")))
            trans <- trans %>%
              dplyr::ungroup() %>%
              dplyr::group_by(V9) %>%
              dplyr::filter(sum(grepl(keypfamlist,unique(domain))) == pfamnumber) %>%
              dplyr::arrange(V9)
            if(!is.null(trans) & dim(trans)[1] >= 1) {
              #trans$pos <- NULL
              trans$pos[1] = 1
              if(dim(trans)[1] > 1) {
                for (i in seq(2,dim(trans)[1])) {
                  if(trans$V9[i] == trans$V9[i-1]) {
                    trans$pos[i] = trans$pos[i-1]
                  } else {
                    trans$pos[i] = trans$pos[i-1]+1
                  }
                }
              }
            }
          }
          return(trans)
        }
        if(input$domainrelation == "only") {
          keypfamlist <- gsub("\\|{2,}","|",
                              sub("\\|{0,}$|^\\|{0,}","",gsub("[\r\n]","|",input$keypfam)))
          gff <- csvfile()[grep(keypfamlist, csvfile()$domain),]
          pfamgene <- unique(gff$V9)
          gff <- csvfile()[csvfile()$V9 %in% pfamgene,]
          gff <- gff[!is.na(gff$V9),]
          if(!is.null(gff) & dim(gff)[1] >= 1)  {
            trans <- gff[gff$V3 %in% c("exon", "CDS", "five_prime_UTR", "three_prime_UTR"), ]
            transtmp <- trans %>%  dplyr::select(V9) %>% dplyr::filter(!duplicated(V9))
            transtmp$pos <- seq(dim(transtmp)[1],1)
            trans <- merge(trans,transtmp,by="V9")
            pfamnumber <- length(unlist(strsplit(keypfamlist,"\\|"))[nchar(unlist(strsplit(keypfamlist,"\\|"))) >0])
            trans <- trans %>%
              dplyr::ungroup() %>%
              dplyr::group_by(V9) %>%
              #dplyr::filter(sum(grepl(keypfamlist,unique(domain))) == pfamnumber) %>%
              #dplyr::filter(sum(unlist(strsplit(keypfamlist,"\\|")) %in% unique(trans$domain)) == pfamnumber) %>%
              dplyr::arrange(V9)
            trans <- trans[!is.na(trans$V9),] %>% ungroup()
            V9hold <- NULL
            for (i in unique(trans$V9)) {
              tmp <- trans[trans$V9 == i,]
              tmpdomain <- unique(tmp$domain[!is.na(tmp$domain)])
              keypfamlist <- unlist(strsplit(keypfamlist,"\\|"))[nchar(unlist(strsplit(keypfamlist,"\\|"))) >0]
              if((sum(keypfamlist %in% tmpdomain) == pfamnumber) & (length(tmpdomain) == pfamnumber)) {
                V9hold <- c(V9hold,i)
              }
            }
            trans <- trans[trans$V9 %in% V9hold,]
            if(!is.null(trans) & dim(trans)[1] >= 1) {
              #trans$pos <- NULL
              trans$pos[1] = 1
              if(dim(trans)[1] > 1) {
                for (i in seq(2,dim(trans)[1])) {
                  if(trans$V9[i] == trans$V9[i-1]) {
                    trans$pos[i] = trans$pos[i-1]
                  } else {
                    trans$pos[i] = trans$pos[i-1]+1
                  }
                }
              }
            }
          }
          return(trans)
        }
      }
    }
  })
  selectiveprintdomain <- reactive({
    if(is.null(input$domainhighlight)) {
      return(NULL)
    }
    if(!is.null(input$domainhighlight)) {
      return(gsub("\\|{2,}","|",
                  sub("\\|{0,}$|^\\|{0,}","",gsub("[\r\n]","|",input$domainhighlight))))
    }
  })
  maxsymbollength <- eventReactive(!is.null(output_image_data()) & dim(output_image_data())[1] >= 1,{
    trans <- output_image_data()
    if(!is.null(trans) ) { 
      if(dim(trans)[1] >= 1) {
        maxsymbollength <- max(trans$pos)
        return(maxsymbollength)
      }
    }
  })
  genicmaximum <- eventReactive(!is.null(output_image_data()) & dim(output_image_data())[1] >= 1,{
    trans <- output_image_data()
    if(!is.null(trans) & dim(trans)[1] >= 1) {
      maximum <- (floor(max(abs(c(trans$V4,trans$V5)))/1000+1))*1000
      return(maximum)
    }
  })
  output$exportgenic <- renderUI({
    if(input$outputtype == "savegenic" & !is.null(output_image_data())) {
      list(selectInput("figuretype","Figure type:",
                       choices = c("pdf"="pdf",
                                   "png"="png",
                                   "tiff"="tiff",
                                   "jpeg"="jpeg",
                                   "eps"="eps",
                                   "ps"="ps",
                                   "tex"="tex",
                                   "bmp"="bmp",
                                   "svg"="svg"),
                       selected = "png"),
           numericInput("genicimagewidth", "The width of image (inch)", round(13*genicmaximum()/12000,2)),
           numericInput("genicimageheight", "The height of image (inch)", round(3*maxsymbollength()/11,2)),
           numericInput("genicresolution", "The resolution of image (dpi)", 300),
           downloadButton("savegenic", "Save genic feature"))
    } else {
      list(NULL)
    }
  })
  output$exporttrans <- renderUI({
    if(input$outputtype == "savetrans" & !is.null(output_image_data()))  {
      list(selectInput("figuretype","Figure type:",
                       choices = c("pdf"="pdf",
                                   "png"="png",
                                   "tiff"="tiff",
                                   "jpeg"="jpeg",
                                   "eps"="eps",
                                   "ps"="ps",
                                   "tex"="tex",
                                   "bmp"="bmp",
                                   "svg"="svg"),
                       selected = "png"),
           numericInput("transimagewidth", "The width of image (inch)", round(13*genicmaximum()/12000,2)),
           numericInput("transimageheight", "The height of image (inch)", round(3*maxsymbollength()/11,2)),
           numericInput("transresolution", "The resolution of image (dpi)", 300),
           downloadButton("savetrans", "Save transcript feature"))
    }  else {
      list(NULL)
    }
  })
  output$exportcds <- renderUI({
    if(input$outputtype == "savecds" & !is.null(output_image_data())) {
      list(selectInput("figuretype","Figure type:",
                       choices = c("pdf"="pdf",
                                   "png"="png",
                                   "tiff"="tiff",
                                   "jpeg"="jpeg",
                                   "eps"="eps",
                                   "ps"="ps",
                                   "tex"="tex",
                                   "bmp"="bmp",
                                   "svg"="svg"),
                       selected = "png"),
           numericInput("cdsimagewidth", "The width of image (inch)", round(13*genicmaximum()/12000,2)),
           numericInput("cdsimageheight", "The height of image (inch)",round(3*maxsymbollength()/11,2)),
           numericInput("cdsresolution", "The resolution of image (dpi)", 300),
           downloadButton("savecds", "Save CDS feature"))
    } else {
      list(NULL)
    }
  })
  output$exportsummary <- renderUI({
    if(input$outputtype == "savesummary" & !is.null(output_image_data())) {
      downloadButton("savesummary", "Save Summary")
    } else {
      list(NULL)
    }
  })
  genicvisualizer <- reactive({
    trans <- output_image_data()
    if(all(!is.null(trans),dim(trans)[1]>=1)) {
      intronlineV4 <- aggregate(V4 ~ V9,  data = trans,min)[,2]
      intronlineV5 <- aggregate(V5 ~ V9,  data = trans,max)[,2]
      intronlinepos <- aggregate(pos ~ V9,data = trans,min)[,2]
      intronline <- data.frame(V4 = intronlineV4,
                               V5=intronlineV5,
                               pos=intronlinepos)
      exon <-  trans[trans$V3 == "exon", ]
      cds <-  trans[trans$V3 == "CDS", ]
      five_prime <- trans[trans$V3 == "five_prime_UTR", ]
      three_prime <- trans[trans$V3 == "three_prime_UTR", ]
      maxsymbollength <- max(nchar(as.character(trans$V9)))
      showtranstext <- if(isTRUE(input$showtransnames)) {
        list(geom_text(data=trans[!duplicated(trans$V9),],aes(x=-max(pretty(seq(1,maximum)))*0.2,y=pos,label=V9),
                       size=input$textsize,colour=input$textcolour))
      } else {
        list(NULL)
      }
      trans <- trans  %>%
        dplyr::group_by(V9,domain) %>%
        dplyr::mutate(VVVV4_domain=min(VVVV4),
                      VVVV5_domain=max(VVVV5))
      tmp <- trans %>%
        dplyr::filter(!is.na(domain),!duplicated(V9,domain)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(V9) %>%
        dplyr::arrange(V9,VVVV4_domain) %>%
        dplyr::mutate(domainnum = dplyr::row_number()) %>%
        dplyr::select(V9,domain,domainnum)
      trans <- dplyr::left_join(trans,tmp,by=c("V9","domain"))
	  
      #convert trans to trans2 for domain line usage
	  trans2 <- as.data.frame(trans)
      if(dim(trans2)[1] >= 2) {
        tmp <- NULL
        for(j in unique(trans2$V9)) {
          tmp1 <- trans2[trans2$V9 == j,]
		  tmp1 <- tmp1[grepl("\\w+",tmp1$domain),]
          tmp1 <- tmp1[order(tmp1$domain,tmp1$VVV4),]
          if(dim(tmp1)[1] >= 2) {
            for(i in 2:dim(tmp1)[1]) {
              #if(!is.na(tmp1$VVVV4[i]) & !is.na(tmp1$VVVV5[i-1])) {
                if((as.numeric(tmp1$VVV4[i]) - as.numeric(tmp1$VVV5[i-1])) <= 20) {
                  tmp1$VVV4[i] = tmp1$VVV5[i-1]
                  tmp <- rbind(tmp,tmp1)
                } 
          } 
          } else {
                  tmp <- rbind(tmp,tmp1)
              #}
            }
        }
      }
      trans2 <- rbind(trans2[!grepl("\\w+",trans2$domain),],tmp)
      
      if(isTRUE(input$displaydomain)) {
        domainbackground1 <- list(
          geom_segment(data = trans[!is.na(trans$domain),],
                       aes(x = VVVV4_domain, xend = VVVV5_domain, y = pos,
                           yend = pos,colour=domain),size=input$domainsize,linetype="dashed")
        )
        domainbackground2 <- list(
          geom_segment(data = trans[!is.na(trans$domain),],
                       aes(x = VVVV4_domain, xend = VVVV5_domain, y = pos, yend = pos),colour = "white",
                       size=0.7*input$domainsize)
        )
        domainline <- list(geom_segment(data = trans2[grepl("\\w+",trans2$domain),],
                                        aes(x = VVVV4, xend = VVVV5, y = pos, yend = pos,
                                            colour = domain),size=input$domainsize))
      } else {
        domainbackground1 <- NULL
        domainbackground2 <- NULL
        domainline <- NULL
      }
      if(isTRUE(input$showdomainnames)) {
        if(is.null(selectiveprintdomain())) {
          showdomaintext <- list(geom_text(data=trans[!is.na(trans$domain),],
                                           aes(x=(VVVV4_domain+VVVV5_domain)/2,y=pos,label=domain),
                                           size=input$domaintextsize,colour=input$domaintextcolour))
        }
        if(!is.null(selectiveprintdomain())) {
          showdomaintext <- list(geom_text(data=trans[grep(selectiveprintdomain(),trans$domain),],
                                           aes(x=(VVVV4_domain+VVVV5_domain)/2,y=pos,label=domain),
                                           size=input$domaintextsize,colour=input$domaintextcolour))
        }
      } else {
        showdomaintext <- list(NULL)
      }
      maximum <- (floor(max(c(max(trans$V4),max(trans$V5)))/1000+1))*1000
      setdomaincolourplatte <- ifelse(input$domaincolourplatte == "None",
                                      list(NULL),
                                      list(scale_colour_brewer(palette = input$domaincolourplatte)
                                      ))
      xaxismax <- max(pretty(seq(1,maximum)))
      axaisnonemax <- pretty(seq(1,maximum))[1:(length(pretty(seq(1,maximum)))-1)]
      xaxislabel = c(axaisnonemax,paste0(xaxismax, " (bp)"))
      output_image <- ggplot() + 
        domainbackground1+
        domainbackground2+
        geom_segment(data = intronline, aes(x = V4, xend = V5, y = pos, yend = pos),
                     size = input$intronsize, colour = "black") +
        geom_segment(data = five_prime, aes(x = V4, xend = V5, y = pos, yend = pos),
                     size = input$utrsize, colour = input$utr5col) +
        geom_segment(data = three_prime, aes(x = V4, xend = V5, y = pos, yend = pos),
                     size = input$utrsize, colour = input$utr3col) +
        geom_segment(data = cds, aes(x = V4, xend = V5, y = pos, yend = pos),
                     colour=input$cdscol,size = input$cdssize) +
        domainline+
        setdomaincolourplatte+
        #scale_colour_brewer(palette = input$domaincolourplatte)+
        showtranstext+
        showdomaintext+
        geom_segment(aes(x=pretty(seq(1,maximum)),
                         xend=pretty(seq(1,maximum)),
                         y=rep(0,length(pretty(seq(1,maximum)))),
                         yend=rep(0-max(trans$pos)/75,length(pretty(seq(1,maximum))))))+
        #geom_text(aes(x=pretty(seq(1,maximum)),y=0-max(trans$pos)/20,label=pretty(seq(1,maximum))))+
        theme_bw() +
        scale_y_continuous(limits = c(-2-max(trans$pos)/10,max(trans$pos)+2))+
        scale_x_continuous(limits = c(-max(pretty(seq(1,maximum)))*0.5,max(pretty(seq(1,maximum)))*1.1),
                           breaks = pretty(seq(1,maximum)))+
        geom_segment(aes(x=pretty(seq(1,maximum)),
                         xend=pretty(seq(1,maximum)),
                         y=rep(0,length(pretty(seq(1,maximum)))),
                         yend=rep(0-max(trans$pos)/75,length(pretty(seq(1,maximum))))))+
        geom_segment(aes(x=min(pretty(seq(1,maximum))),xend=max(pretty(seq(1,maximum))),y=0,yend=0))+
        #geom_text(aes(x=pretty(seq(1,maximum)),y=0-max(trans$pos)/20,label=pretty(seq(1,maximum))),size=input$textsize)+
        geom_text(aes(x=pretty(seq(1,maximum)),y=0-max(trans$pos)/20,label=xaxislabel),size=input$textsize)+
        #geom_text(aes(x=mean(pretty(seq(1,maximum))),y=-0.2-max(trans$pos)/40),label="bp",size=input$textsize)+
        theme(#legend.position = "none",
          panel.border = element_blank(),
          panel.grid = element_blank(),
          #text = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          #axis.ticks.x = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          #axis.line.x = element_line(colour="black"),
          #axis.ticks = element_blank(),
          #axis.ticks.length.x = unit(0.25, "cm"),
          panel.background = element_rect(fill = "transparent",colour = "NA"),
          plot.background = element_rect(fill = "transparent", colour = "NA"),
          axis.text.x=element_blank(),
          legend.text = element_text(size=input$textsize*2.8),
          legend.title = element_text(size=input$textsize*2.8))
      return(output_image)
    } else {
      print("no symbol observed")
    }
    
  })
  transvisualizer <- reactive({
    trans <- output_image_data()
    if(all(!is.null(trans),dim(trans)[1]>=1)) {
      cds <-  trans[trans$V3 == "CDS", ] %>%
        dplyr::group_by(V9) %>%
        dplyr::mutate(VV4=min(VV4),VV5=max(VV5))
      five_prime <- trans[trans$V3 == "five_prime_UTR", ]
      three_prime <- trans[trans$V3 == "three_prime_UTR", ]
      maxsymbollength <- max(nchar(as.character(trans$V9)))
      #showtranstext <- if(isTRUE(input$showtransnames)) {
      #  list(geom_text(data=trans[!duplicated(trans$V9),],aes(x=-450*maxsymbollength/16,y=pos,label=V9),
      #                 size=input$textsize,colour=input$textcolour))
      #} else {
      #  list(NULL)
      #}
      showtranstext <- if(isTRUE(input$showtransnames)) {
        list(geom_text(data=trans[!duplicated(trans$V9),],aes(x=-max(pretty(seq(1,maximum)))*0.2,y=pos,label=V9),
                       size=input$textsize,colour=input$textcolour))
      } else {
        list(NULL)
      }
      maximum <- (floor(max(c(max(trans$VV4),max(trans$VV5)))/1000+1))*1000
      trans <- trans  %>%
        dplyr::group_by(V9,domain) %>%
        dplyr::mutate(VVV4_domain=min(VVV4),
                      VVV5_domain=max(VVV5))
      tmp <- trans %>%
        dplyr::filter(!is.na(domain),!duplicated(V9,domain)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(V9) %>%
        dplyr::arrange(V9,VVV4_domain) %>%
        dplyr::select(V9,domain)
      trans <- dplyr::left_join(trans,tmp,by=c("V9","domain"))
      
      #
	  trans2 <- trans
	  trans2 <- as.data.frame(trans)
	  trans2 <- trans2[!is.na(trans2$domain),]
	  trans2 <- trans2[order(trans2$V9,trans2$domain,trans2$VVV4),]
	  for(i in 2:dim(trans2)[1]) {
              #if(!is.na(tmp1$VVVV4[i]) & !is.na(tmp1$VVVV5[i-1])) {
                if((as.numeric(trans2$VVV4[i]) - as.numeric(trans2$VVV5[i-1])) <= 20 & 
				   (trans2$domain[i] == trans2$domain[i-1]) & 
				   (trans2$V9[i] == trans2$V9[i-1]) ) {
                  trans2$VVV4[i] = trans2$VVV4[i-1]
                } 
          }
	  
      if(isTRUE(input$displaydomain)) {
        domainbackground1 <- list(
          geom_segment(data = trans[!is.na(trans$domain),],
                       aes(x = VVV4_domain, xend = VVV5_domain, y = pos,
                           yend = pos,colour=domain),size=input$domainsize,linetype="dashed")
        )
        domainbackground2 <- list(
          geom_segment(data = trans[!is.na(trans$domain),],
                       aes(x = VVV4_domain, xend = VVV5_domain, y = pos, yend = pos),colour = "white",
                       size=0.7*input$domainsize)
        )
        domainline <- list(geom_segment(data = trans2[!is.na(trans2$domain),],
                                        aes(x = VVV4, xend = VVV5, y = pos, yend = pos,
                                            colour = domain,fill=domain),size=input$domainsize))
      } else {
        domainbackground1 <- NULL
        domainbackground2 <- NULL
        domainline <- NULL
      }
      if(isTRUE(input$showdomainnames)) {
        if(is.null(selectiveprintdomain())) {
          showdomaintext <- list(geom_text(data=trans[!is.na(trans$domain),],
                                           aes(x=(VVV4_domain+VVV5_domain)/2,y=pos,label=domain),
                                           size=input$domaintextsize,colour=input$domaintextcolour))
        }
        if(!is.null(selectiveprintdomain())) {
          showdomaintext <- list(geom_text(data=trans[grep(selectiveprintdomain(),trans$domain),],
                                           aes(x=(VVV4_domain+VVV5_domain)/2,y=pos,label=domain),
                                           size=input$domaintextsize,colour=input$domaintextcolour))
        }
      } else {
        showdomaintext <- list(NULL)
      }
      setdomaincolourplatte <- ifelse(input$domaincolourplatte == "None",
                                      list(NULL),
                                      list(scale_colour_brewer(palette = input$domaincolourplatte)
                                      ))
      xaxismax <- max(pretty(seq(1,maximum)))
      axaisnonemax <- pretty(seq(1,maximum))[1:(length(pretty(seq(1,maximum)))-1)]
      xaxislabel = c(axaisnonemax,paste0(xaxismax, " (bp)"))
      output_image <- ggplot() + 
        #domainbackground1+
        #domainbackground2+
        geom_segment(data = five_prime, aes(x = VV4, xend = VV5, y = pos, yend = pos),
                     size = input$utrsize, colour = input$utr5col) + # plot three
        geom_segment(data = three_prime, aes(x = VV4, xend = VV5, y = pos, yend = pos),
                     size = input$utrsize, colour = input$utr3col) + # plot exon
        geom_segment(data = cds, aes(x = VV4, xend = VV5, y = pos, yend = pos),
                     size = input$cdssize, colour = input$cdscol) +
        #geom_text(aes(x=pretty(seq(1,maximum)),y=0-max(trans$pos)/20,label=pretty(seq(1,maximum))))+
        domainline +
        setdomaincolourplatte+
        #scale_colour_brewer(palette = input$domaincolourplatte)+
        showtranstext+
        showdomaintext+
        theme_bw() +
        scale_y_continuous(limits = c(-2-max(trans$pos)/10,max(trans$pos)+2))+
        scale_x_continuous(limits = c(-max(pretty(seq(1,maximum)))*0.5,max(pretty(seq(1,maximum)))*1.1),
                           breaks = pretty(seq(1,maximum)))+
        #geom_segment(aes(x=-1800*maxsymbollength/16,xend=max(pretty(seq(1,maximum)))+1000,y=-Inf,yend=-Inf))+
        geom_segment(aes(x=pretty(seq(1,maximum)),
                         xend=pretty(seq(1,maximum)),
                         y=rep(0,length(pretty(seq(1,maximum)))),
                         yend=rep(0-max(trans$pos)/75,length(pretty(seq(1,maximum))))))+
        geom_segment(aes(x=min(pretty(seq(1,maximum))),xend=max(pretty(seq(1,maximum))),y=0,yend=0))+
        #geom_text(aes(x=pretty(seq(1,maximum)),y=0-max(trans$pos)/20,label=pretty(seq(1,maximum))),size=input$textsize)+
        geom_text(aes(x=pretty(seq(1,maximum)),y=0-max(trans$pos)/20,label=xaxislabel),size=input$textsize)+
        #geom_text(aes(x=mean(pretty(seq(1,maximum))),y=-0.2-max(trans$pos)/40),label="bp",size=input$textsize)+
        theme(#legend.position = "none",
          panel.border = element_blank(),
          panel.grid = element_blank(),
          #text = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          #axis.ticks.x = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          #axis.line.x = element_line(colour="black"),
          #axis.ticks = element_blank(),
          #axis.ticks.length.x = unit(0.25, "cm"),
          panel.background = element_rect(fill = "transparent",colour = "NA"),
          plot.background = element_rect(fill = "transparent", colour = "NA"),
          axis.text.x=element_blank(),
          legend.text = element_text(size=input$textsize*2.8),
          legend.title = element_text(size=input$textsize*2.8))
      return(output_image)
    } else {
      print("no symbol observed")
    }
  })
  cdsvisualizer <- reactive({
    if(all(!is.null(output_image_data()),dim(output_image_data())[1]>=1)) {
      trans <- output_image_data() %>%
        dplyr::filter(V3 == "CDS") %>%
        dplyr::group_by(V9,domain) %>%
        dplyr::mutate(VVVV4_domain=min(VVVV4),
                      VVVV5_domain=max(VVVV5),
                      row_number = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(V9) %>%
        dplyr::mutate(start=min(V4)) %>%
        dplyr::mutate(V4 = V4 - start+1,
                      V5 = V5 - start+1,
                      VV4 = VV4 - start+1,
                      VV5 = VV5 - start+1,
                      VVV4 = VVV4 - start+1,
                      VVV5 = VVV5 - start+1,
                      VVVV4 = VVVV4 - start+1,
                      VVVV5 = VVVV5 - start+1,
                      VVVV4_domain = VVVV4_domain - start+1,
                      VVVV5_domain = VVVV5_domain - start+1)
      trans <- trans  %>%
        dplyr::group_by(V9,domain) %>%
        dplyr::mutate(VVV4_domain=min(VVV4),
                      VVV5_domain=max(VVV5))
      tmp <- trans %>%
        dplyr::filter(!is.na(domain),!duplicated(V9,domain)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(V9) %>%
        dplyr::arrange(V9,VVV4_domain) %>%
        dplyr::mutate(domainnum = dplyr::row_number()) %>%
        dplyr::select(V9,domain,domainnum)
      trans <- dplyr::left_join(trans,tmp,by=c("V9","domain"))
      
      trans2 <- trans
	  trans2 <- as.data.frame(trans)
	  trans2 <- trans2[!is.na(trans2$domain),]
	  trans2 <- trans2[order(trans2$V9,trans2$domain,trans2$VVV4),]
	  for(i in 2:dim(trans2)[1]) {
              #if(!is.na(tmp1$VVVV4[i]) & !is.na(tmp1$VVVV5[i-1])) {
                if((as.numeric(trans2$VVV4[i]) - as.numeric(trans2$VVV5[i-1])) <= 20 & 
				   (trans2$domain[i] == trans2$domain[i-1]) & 
				   (trans2$V9[i] == trans2$V9[i-1]) ) {
                  trans2$VVV4[i] = trans2$VVV4[i-1]
                } 
          }
	  
      maxsymbollength <- max(nchar(as.character(trans$V9)))
      #showtranstext <- if(isTRUE(input$showtransnames)) {
      #  list(geom_text(data=trans[!duplicated(trans$V9),],
      #                 aes(x=-450*maxsymbollength/16,y=pos,label=V9),
      #                 size=input$textsize,colour=input$textcolour))
      #} else {
      #  list(NULL)
      #}
      showtranstext <- if(isTRUE(input$showtransnames)) {
        list(geom_text(data=trans[!duplicated(trans$V9),],aes(x=-max(pretty(seq(1,maximum)))*0.2,y=pos,label=V9),
                       size=input$textsize,colour=input$textcolour))
      } else {
        list(NULL)
      }
      maximum <- (floor(max(c(max(trans$VV4),max(trans$VV5)))/1000+1))*1000
      if(isTRUE(input$displaydomain)) {
        domainbackground1 <- list(
          geom_segment(data = trans[!is.na(trans$domain),],
                       aes(x = VVV4_domain, xend = VVV5_domain, y = pos, yend = pos),
                       colour="white",size=input$domainsize)
        )
        domainbackground2 <- list(
          geom_segment(data = trans[!is.na(trans$domain),],
                       aes(x = VVV4_domain, xend = VVV5_domain, y = pos, yend = pos),
                       colour="white",size=0.7*input$domainsize)
        )
        domainline <- list(geom_segment(data = trans2[!is.na(trans2$domain),],
                                        aes(x = VVV4, xend = VVV5, y = pos, yend = pos,
                                            colour = domain,fill=domain),size=input$domainsize))
      } else {
        domainbackground1 <- NULL
        domainbackground2 <- NULL
        domainline <- NULL
      }
      if(isTRUE(input$showdomainnames)) {
        if(is.null(selectiveprintdomain())) {
          showdomaintext <- list(geom_text(data=trans[!is.na(trans$domain),],
                                           aes(x=(VVV4_domain+VVV5_domain)/2,y=pos,label=domain),
                                           size=input$domaintextsize,colour=input$domaintextcolour))
        }
        if(!is.null(selectiveprintdomain())) {
          showdomaintext <- list(geom_text(data=trans[grep(selectiveprintdomain(),trans$domain),],
                                           aes(x=(VVV4_domain+VVV5_domain)/2,y=pos,label=domain),
                                           size=input$domaintextsize,colour=input$domaintextcolour))
        }
      } else {
        showdomaintext <- list(NULL)
      }
      trans <- trans %>%
        dplyr::ungroup() %>%
        dplyr::group_by(V9) %>%
        dplyr::mutate(cds_start = min(VV4),
                      cds_end = max(VV5))
      setdomaincolourplatte <- ifelse(input$domaincolourplatte == "None",
                                      list(NULL),
                                      list(scale_colour_brewer(palette = input$domaincolourplatte)
                                      ))
      xaxismax <- max(pretty(seq(1,maximum)))
      axaisnonemax <- pretty(seq(1,maximum))[1:(length(pretty(seq(1,maximum)))-1)]
      xaxislabel = c(axaisnonemax,paste0(xaxismax, " (bp)"))
      output_image <- ggplot() + 
        #domainbackground1+
        #domainbackground2+
        geom_segment(data = trans, aes(x = cds_start, xend = cds_end, y = pos, yend = pos),
                     size = input$cdssize, colour = input$cdscol) +
        domainline+
        setdomaincolourplatte+
        #scale_colour_brewer(palette = input$domaincolourplatte)+
        showtranstext+
        showdomaintext+
        theme_bw() +
        scale_y_continuous(limits = c(-2-max(trans$pos)/10,max(trans$pos)+2))+
        scale_x_continuous(limits = c(-max(pretty(seq(1,maximum)))*0.5,max(pretty(seq(1,maximum)))*1.1),
                           breaks = pretty(seq(1,maximum)))+
        #geom_segment(aes(x=-1800*maxsymbollength/16,xend=max(pretty(seq(1,maximum)))+1000,y=-Inf,yend=-Inf))+
        geom_segment(aes(x=pretty(seq(1,maximum)),
                         xend=pretty(seq(1,maximum)),
                         y=rep(0,length(pretty(seq(1,maximum)))),
                         yend=rep(0-max(trans$pos)/75,length(pretty(seq(1,maximum))))))+
        geom_segment(aes(x=min(pretty(seq(1,maximum))),xend=max(pretty(seq(1,maximum))),y=0,yend=0))+
        #geom_text(aes(x=pretty(seq(1,maximum)),y=0-max(trans$pos)/20,label=pretty(seq(1,maximum))),size=input$textsize)+
        geom_text(aes(x=pretty(seq(1,maximum)),y=0-max(trans$pos)/20,label=xaxislabel),size=input$textsize)+
        #geom_text(aes(x=mean(pretty(seq(1,maximum))),y=-0.2-max(trans$pos)/40),label="bp",size=input$textsize)+
        theme(#legend.position = "none",
          panel.border = element_blank(),
          panel.grid = element_blank(),
          #text = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          #axis.ticks.x = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          #axis.line.x = element_line(colour="black"),
          #axis.ticks = element_blank(),
          #axis.ticks.length.x = unit(0.25, "cm"),
          panel.background = element_rect(fill = "transparent",colour = "NA"),
          plot.background = element_rect(fill = "transparent", colour = "NA"),
          axis.text.x=element_blank(),
          legend.text = element_text(size=input$textsize*2.8),
          legend.title = element_text(size=input$textsize*2.8))
      return(output_image)
    } else {
      print("no symbol observed")
    }
  })
  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  summarydata <- eventReactive(!is.null(output_image_data()) & dim(output_image_data())[1] >= 1,{
    trans1 <- output_image_data() %>%
      dplyr::group_by(V9) %>%
      dplyr::filter(!duplicated(paste0(V9,V4))) %>%
      dplyr::mutate(cds.number = sum(V3 == "CDS"),
                    intron.number = sum(V3 == "CDS")-1,
                    utr5.number = sum(V3 == "three_prime_UTR"),
                    utr3.number = sum(V3 == "five_prime_UTR"),
                    cds.length = sum((V5-V4)[V3 == "CDS"])
      ) %>%
      dplyr::filter(!duplicated(V9)) %>%
      dplyr::select(V9,cds.number,intron.number,utr5.number,utr3.number,cds.length)
    names(trans1)[names(trans1)=="V9"] <- "transcript"
    trans2 <- output_image_data() %>%
      dplyr::group_by(V9) %>%
      dplyr::mutate(domain = paste0(domain[!is.na(domain)],collapse=",")) %>%
      dplyr::filter(!duplicated(V9)) %>%
      dplyr::select(V9,domain)
    names(trans2)[names(trans2)=="V9"] <- "transcript"
    trans <- merge(trans1,trans2,by="transcript")
  })
  
  cdata <- session$clientData
  output$genicvisualizer <- renderPlotly({
    ggplotly(genicvisualizer(), height = session$clientData$output_genicvisualizer_width)
  })
  output$transvisualizer <- renderPlotly({
    ggplotly(transvisualizer(), height = session$clientData$output_transvisualizer_width)
  })
  output$cdsvisualizer <- renderPlotly({
    ggplotly(cdsvisualizer(), height = session$clientData$output_cdsvisualizer_width)
  })
  output$Summary =  renderDataTable(summarydata())
  devicename <- reactive({
    xx=paste0("save.",input$figuretype)
    xx
  })
  output$savegenic <- downloadHandler(
    filename = function() {
      paste0("save.",input$figuretype)
    },
    content = function(file) {
      ggsave(genicvisualizer(), filename = file,width = input$genicimagewidth,
             height=input$genicimageheight,dpi=input$genicresolution,
             limitsize = FALSE)
    })
  output$savetrans <- downloadHandler(
    filename = function() {
      paste0("save.",input$figuretype)
    },
    content = function(file) {
      ggsave(transvisualizer(), filename = file,width = input$transimagewidth,
             height=input$transimageheight,dpi=input$transresolution,
             limitsize = FALSE)
    })
  output$savecds <- downloadHandler(
    filename = function() {
      paste0("save.",input$figuretype)
    },
    content = function(file) {
      ggsave(cdsvisualizer(), filename = file,width = input$cdsimagewidth,
             height=input$cdsimageheight,dpi=input$cdsresolution,
             limitsize = FALSE)
    })
  output$savesummary <- downloadHandler(
    filename = function() {
      paste0("save.csv")
    },
    content = function(file) {
      write.csv(summarydata(),file=file,row.names = FALSE)
    })
  #observeEvent(input$intro,
  #             introjs(session))
  #
}
