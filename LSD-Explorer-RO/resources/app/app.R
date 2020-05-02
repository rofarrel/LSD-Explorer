# library(tidyverse)
library(data.table)
library(plyr)
library(dplyr)
# library(gridExtra)
# library(reshape2)
# library(scales)
# library(splitstackshape) #needed for "cslplit" function
library(stringr)
# library(readr)
library(ggplot2)
library(shiny)
# library(readxl)
# library(shinyEventLogger)
# library(shinyBS)
# library(photon)
# library(Rcrawler)
# library(xml2)
# library(devtools)
# library(shinyFiles)
# library(plotly)
# # library(JLutils)
# library(lubridate)
# library(DT)
# library(formattable)
# library(visreg)
# library(cowplot)
if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }
# library(graph)
# install_github("larmarange/JLutils", lib = )
# htmlwidgets,webshot,webdriver,doParallel,cellranger,withr,hms,tidyselect,vctrs,data.table,plyr,plotly,scales,splitstackshape,readr,shiny,readxl,Rcrawler,xml2,shinyFiles,lubridate,formattable,DT,visreg,htmltools,rlang,httpuv,later,promises,retry,websocket,fastmap,crayon,digest,xtable,jsonlite,mime,pillar,lifecycle

# Define UI for application that draws a histogram

"%||%" <- function(x, y){
    if (is.null(x)) {
        y
    } else {
        x
    }
}

gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    grDevices::hcl(h = hues,
                   l = 65,
                   c = 100)[1:n]
}


inv_cumsum <<- function(x) {
    sum(x) - cumsum(x) + x
}

StatFillLabels <<- ggplot2::ggproto(
    "StatFillLabels",
    ggplot2::StatCount,
    compute_panel = function(self, data, scales, ...) {
        if (ggplot2:::empty(data)) {
            return(data.frame())
        }
        groups <- split(data, data$group)
        stats <- lapply(groups, function(group) {
            self$compute_group(data = group, scales = scales, ...)
        })
        stats <- mapply(function(new, old) {
            if (ggplot2:::empty(new)) {
                return(data.frame())
            }
            unique <- ggplot2:::uniquecols(old)
            missing <- !(names(unique) %in% names(new))
            cbind(new, unique[rep(1, nrow(new)), missing, drop = FALSE])
        }, stats, groups, SIMPLIFY = FALSE)
        data <- do.call(plyr::rbind.fill, stats)
        plyr::ddply(
            data,
            "x",
            plyr::mutate,
            prop = count / sum(count),
            cumprop = inv_cumsum(count) / sum(count),
            ylabel = (inv_cumsum(count) - count / 2) / sum(count),
            na.rm = TRUE
        )
    },
    default_aes = ggplot2::aes(y = ..ylabel.., label = paste(round(
        100 * ..prop.., digits = 1
    ), "%", sep = ""))
)



stat_fill_labels <<-
    function(mapping = NULL,
             data = NULL,
             geom = "text",
             position = "identity",
             width = NULL,
             na.rm = FALSE,
             show.legend = NA,
             inherit.aes = TRUE,
             ...) {
        ggplot2::layer(
            stat = StatFillLabels,
            data = data,
            mapping = mapping,
            geom = geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
        )
    }


StatStackLabels <<- ggplot2::ggproto(
    "StatStackLabels",
    ggplot2::StatCount,
    compute_panel = function(self, data, scales, ...) {
        if (ggplot2:::empty(data)) {
            return(data.frame())
        }
        groups <- split(data, data$group)
        stats <- lapply(groups, function(group) {
            self$compute_group(data = group, scales = scales, ...)
        })
        stats <- mapply(function(new, old) {
            if (ggplot2:::empty(new)) {
                return(data.frame())
            }
            unique <- ggplot2:::uniquecols(old)
            missing <- !(names(unique) %in% names(new))
            cbind(new, unique[rep(1, nrow(new)), missing, drop = FALSE])
        }, stats, groups, SIMPLIFY = FALSE)
        data <- do.call(plyr::rbind.fill, stats)
        plyr::ddply(
            data,
            "x",
            plyr::mutate,
            cumcount = inv_cumsum(count),
            ylabel = inv_cumsum(count) - count / 2,
            na.rm = TRUE
        )
    },
    default_aes = ggplot2::aes(y = ..ylabel.., label = ..count..)
)

stat_stack_labels <<-
    function(mapping = NULL,
             data = NULL,
             geom = "text",
             position = "identity",
             width = NULL,
             na.rm = FALSE,
             show.legend = NA,
             inherit.aes = TRUE,
             ...) {
        ggplot2::layer(
            stat = StatStackLabels,
            data = data,
            mapping = mapping,
            geom = geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
        )
    }


ui <- navbarPage(
    "Navbar",
    tabPanel("Data Management",
             sidebarLayout(
                 sidebarPanel(
                     radioButtons(
                         inputId = "dataselector",
                         label = "Data-View: Choose Which Data to Work With",
                         choices = c(
                             "Imported Previous DataFrame" = "a",
                             "Downloaded New Data" = "b",
                             "Merged Old Data and New Data" = "c"
                         ),
                         selected = "a"
                     ),
                     tags$br(),
                     h4("Load Saved Data"),
                     fileInput(
                         inputId = "dataload",
                         label = "Browse for File",
                         multiple = FALSE,
                         buttonLabel = "Browse..",
                         placeholder = "No files selected"
                     ),
                     actionButton(
                         inputId = "loadButton",
                         label = "Import DataFrame",
                         icon = NULL,
                         width = NULL
                     ),
                     tags$br(),
                     tags$br(),
                     h4("Gather Data from LSD"),
                     radioButtons(
                         inputId = "cycleSelect",
                         label = "Cycle",
                         choices = c(
                             "17-18" = 15,
                             "18-19" = 16,
                             "19-20" = 17,
                             "20-21" = 18
                         ),
                         selected = 17,
                         inline = TRUE
                         
                     ),
                     selectInput(
                         inputId = "tierSelect",
                         label = "Select Group of Schools",
                         choices = c(
                             "T14" = 14,
                             "T20" = 20,
                             "Custom Selection" = 225
                         ),
                         selected = "T14",
                         multiple = FALSE
                     ),
                     uiOutput("sch_in"),
                     actionButton(
                         inputId = "scrapeButton",
                         label = "Gather From LSD...",
                         icon = NULL,
                         width = NULL
                     ),
                     h4("Update Old Data with New Data?"),
                     actionButton(
                         inputId = "mergeButton",
                         label = "Update",
                         icon = NULL,
                         width = NULL
                     ),
                     h4("Save Currently Selected Data-View"),
                     downloadButton(
                         outputId = "saveMerged",
                         label = "Save",
                         icon = NULL,
                         width = NULL
                     ),
                     tableOutput("listtable"),
                     width = 3
                 ),
                 mainPanel(
                     uiOutput("tt1"),
                     DT::dataTableOutput("dtable"),
                     uiOutput("tt2"),
                     DT::dataTableOutput("rtable")
                 )
             )),
    tabPanel("View Data",
             sidebarLayout(
                 sidebarPanel(
                     sliderInput(
                         inputId = "lsat_in",
                         label = "LSAT Range",
                         min = 120,
                         max = 180,
                         value = c(160, 170),
                         step = 1
                     ),
                     
                     sliderInput(
                         inputId = "gpa_in",
                         label = "GPA Range",
                         min = 2.0,
                         max = 4.33,
                         value = c(3.5, 4.2),
                         step = 0.01
                     ),
                     radioButtons(
                         inputId = "bar_type",
                         label = "Histogram Bar Type",
                         choices = c("Fill", "Stack"),
                         selected = "Fill"
                     ),
                     uiOutput("side1"),
                     uiOutput("side3"),
                     radioButtons(
                         inputId = "complete_yn",
                         label = "Use Complete Dates?",
                         choices = c("Yes", "No"),
                         selected = "No",
                         inline = TRUE
                     ),
                     
                     dateRangeInput(
                         inputId = "complete_d",
                         label = "Selected Complete date range in YYYY-MM-DD Format",
                         start = "2018-09-01",
                         end = "2020-01-01",
                         separator = "to"
                     ),
                     radioButtons(
                         inputId = "resultbin",
                         label = "Use Simple Result Binning?",
                         choices = c("Yes", "No"),
                         selected = "Yes",
                         inline = TRUE
                     ),
                     uiOutput("side2"),
                     width = 3
                     
                     
                 ),
                 mainPanel(
                     plotly::plotlyOutput("hist"),
                     plotly::plotlyOutput("waves"),
                     DT::dataTableOutput("temptable")
                 )
             )),
    tabPanel("Scholarships",
             sidebarLayout(
                 sidebarPanel(
                     uiOutput("sch1"),
                     uiOutput("sch2"),
                     h5(
                         "The Legends are interactive. Click a money amount to remove it or double-click to isolate it."
                     ),
                     numericInput(
                         inputId = "money_val1",
                         label = "Scholarship Minimum Amount",
                         value = 0,
                         min = 0,
                         max = NA,
                         step = NA,
                         width = NULL
                     ),
                     
                     numericInput(
                         inputId = "money_val2",
                         label = "Scholarship Maximum Amount",
                         value = 500000,
                         min = 0,
                         max = NA,
                         step = NA,
                         width = NULL
                     ),
                     width = 3
                 ),
                 mainPanel(plotly::plotlyOutput("schol1"),
                           plotly::plotlyOutput("schol2"),)
             )),
    tabPanel("Predictor",
             sidebarLayout(
                 sidebarPanel(
                     numericInput(
                         inputId = "lsat_in2",
                         label = "LSAT",
                         value = 160,
                         min = 140,
                         max = 180,
                         step = 1,
                         width = NULL
                     ),
                     
                     numericInput(
                         inputId = "gpa_in2",
                         label = "GPA (2 decimals)",
                         value = 3.72,
                         min = 2,
                         max = 4.33,
                         step = 0.01,
                         width = NULL
                     ),
                     
                     radioButtons(
                         inputId = "urm_in",
                         label = "URM",
                         choices = c("Non-URM" = 0, "URM" = 1),
                         selected = 0,
                         inline = TRUE,
                         width = NULL
                     ),
                     radioButtons(
                         inputId = "sent_yn2",
                         label = "Use 'Sent' Month?",
                         choices = c("No" = 0, "Yes" = 1),
                         selected = 0,
                         inline = TRUE,
                         width = NULL
                     ),
                     
                     checkboxGroupInput(
                         inputId = "month_in",
                         label = "Sent Month",
                         choices = c(
                             "September" = 9,
                             "October" = 10,
                             "November" = 11,
                             "December" = 12,
                             "January" = 1,
                             "February" = 2,
                             "March" = 3
                         ),
                         selected = c(
                             "September" = 9,
                             "October" = 10,
                             "November" = 11,
                             "December" = 12,
                             "January" = 1,
                             "February" = 2,
                             "March" = 3
                         ),
                         inline = FALSE
                         
                     ),
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     p("Prediction Table Below..."),
                     width = 3
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     uiOutput("preds"),
                     plotly::plotlyOutput("predplot1"),
                     plotly::plotlyOutput("predplot2"),
                     DT::dataTableOutput("rtable2")
                     
                 )
             ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #####
    # xxchange <- reactive({
    #     paste(input$tierSelect , input$cycleSelect)
    # })
    
    # init.count <- reactiveVal()
    # init.count(1)
    # 
    # initdata <- readr::read_csv(file = "data-2020-04-30-T20-Cycle17.csv", col_names = TRUE)
    # 
    # observeEvent(input$loadButton,{
    # newval <- init.count()+1
    # init.count(newval)
    # })
    # observeEvent(input$scrapeButton,{
    # newval <- init.count()+1
    # init.count(newval)
    # })
    schools2 <- reactive({
        b1 <-
            c(
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id=",
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="             ,
                "https://www.lawschooldata.org/school/applicants?cycle_id="
            )
        b3 <-
            c(
                "&school=Arizona+State+University",
                "&school=Baylor+University",
                "&school=Boston+College",
                "&school=Boston+University",
                "&school=Brigham+Young+University",
                "&school=College+of+William+and+Mary",
                "&school=Columbia+University",
                "&school=Cornell+University",
                "&school=Duke+University",
                "&school=Emory+University",
                "&school=Florida+State+University",
                "&school=Fordham+University",
                "&school=George+Mason+University"
                ,
                "&school=George+Washington+University",
                "&school=Georgetown+University",
                "&school=Harvard+University",
                "&school=Indiana+University+Bloomington"
                ,
                "&school=New+York+University",
                "&school=Northwestern+University",
                "&school=Ohio+State+University",
                "&school=Pepperdine+University"
                ,
                "&school=Stanford+University",
                "&school=Temple+University",
                "&school=University+of+Alabama",
                "&school=University+of+Arizona",
                "&school=University+of+California+Berkeley"
                ,
                "&school=University+of+California+Davis",
                "&school=University+of+California+Irvine",
                "&school=University+of+California+Los+Angeles",
                "&school=University+of+Chicago"
                ,
                "&school=University+of+Colorado+Boulder",
                "&school=University+of+Florida",
                "&school=University+of+Georgia",
                "&school=University+of+Illinois+Urbana-Champaign"
                ,
                "&school=University+of+Iowa",
                "&school=University+of+Minnesota",
                "&school=University+of+North+Carolina+Chapel+Hill",
                "&school=University+of+Notre+Dame"
                ,
                "&school=University+of+Pennsylvania",
                "&school=University+of+Southern+California",
                "&school=University+of+Texas+Austin",
                "&school=University+of+Utah"
                ,
                "&school=University+of+Virginia",
                "&school=University+of+Washington",
                "&school=University+of+Wisconsin+Madison",
                "&school=Vanderbilt+University",
                "&school=Wake+Forest+University"
                ,
                "&school=Washington+University+in+St+Louis",
                "&school=Washington+and+Lee+University",
                "&school=Yale+University"
            )
        b2 <-
            c(
                "Arizona State University"                 ,
                "Baylor University"                        ,
                "Boston College"
                ,
                "Boston University"                        ,
                "Brigham Young University"                 ,
                "College of William and Mary"
                ,
                "Columbia University"                      ,
                "Cornell University"                       ,
                "Duke University"
                ,
                "Emory University"                         ,
                "Florida State University"                 ,
                "Fordham University"
                ,
                "George Mason University"                  ,
                "George Washington University"             ,
                "Georgetown University"
                ,
                "Harvard University"                       ,
                "Indiana University Bloomington"           ,
                "New York University"
                ,
                "Northwestern University"                  ,
                "Ohio State University"                    ,
                "Pepperdine University"
                ,
                "Stanford University"                      ,
                "Temple University"                        ,
                "University of Alabama"
                ,
                "University of Arizona"                    ,
                "University of California Berkeley"        ,
                "University of California Davis"
                ,
                "University of California Irvine"          ,
                "University of California Los Angeles"     ,
                "University of Chicago"
                ,
                "University of Colorado Boulder"           ,
                "University of Florida"                    ,
                "University of Georgia"
                ,
                "University of Illinois Urbana-Champaign"  ,
                "University of Iowa"                       ,
                "University of Minnesota"
                ,
                "University of North Carolina Chapel Hill" ,
                "University of Notre Dame"                 ,
                "University of Pennsylvania"
                ,
                "University of Southern California"        ,
                "University of Texas Austin"               ,
                "University of Utah"
                ,
                "University of Virginia"                   ,
                "University of Washington"                 ,
                "University of Wisconsin Madison"
                ,
                "Vanderbilt University"                    ,
                "Wake Forest University"                   ,
                "Washington University in St Louis"
                ,
                "Washington and Lee University"            ,
                "Yale University"
            )
        ranks <-
            c(
                27,
                48,
                27,
                23,
                39,
                39,
                5,
                13,
                10,
                26,
                48,
                39,
                45,
                22,
                14,
                3,
                34,
                6,
                10,
                34,
                51,
                2,
                48,
                25,
                39,
                10,
                31,
                23,
                15,
                4,
                45,
                31,
                27,
                39,
                27,
                20,
                34,
                21,
                7,
                17,
                16,
                47,
                8,
                44,
                34,
                18,
                31,
                18,
                34,
                1
            )
        temp <- as.data.frame(cbind(b1, b3, b2, ranks))
        temp$full <-
            paste(temp$b1, input$cycleSelect, temp$b3, sep = "")
        schoolurls <- temp[, c("full", "b2", "ranks")]
        colnames(schoolurls) <- c("X1", "X2", "rank")
        schoolurls$rank <- as.integer(as.character(schoolurls$rank))
        schoolurls$X1 <- as.character(schoolurls$X1)
        schoolurls$X2 <- as.character(schoolurls$X2)
        rm(temp)
        rm(b1)
        rm(b2)
        rm(b3)
        rm(ranks)
        return(schoolurls)
    })
    
    # rlist3 <- c("Rejected", "Rejected, Deferred", "Rejected, Withdrawn")
    #
    # alist2 <- c("Accepted", "Accepted, Attending", "Accepted, Deferred", "WL, Accepted","WL, Accepted, Withdrawn","WL, Accepted, Attending", "Acceptd, Deferred, Attending", "Accepted, Deferred, Withdrawn", "Accepted, Withrdawn")
    #
    # wlist2 <- c("Waitlisted", "Waitlisted, Deferred", "Waitlisted, Withdrawn", "WL, Rejected", "WL, Rejected, Withdrawn",
    #             "WL, Withdrawn")
    
    
    schools <-
        eventReactive(c(input$tierSelect, input$cycleSelect, input$allSelect),
                      {
                          schoolurls <- schools2()
                          schoolurls$X1 <- as.character(schoolurls$X1)
                          schoolurls$X2 <- as.character(schoolurls$X2)
                          if (input$tierSelect == 225) {
                              namecheck <- input$allSelect
                              schoolurls <- schoolurls[schoolurls$X2 %in% namecheck,]
                          } else{
                              rankcheck <- 1:input$tierSelect
                              schoolurls <-
                                  schoolurls[schoolurls$rank %in% rankcheck,]
                          }
                          schoolurls$X1 <- as.character(schoolurls$X1)
                          schoolurls$X2 <- as.character(schoolurls$X2)
                          rm(rankcheck)
                          rm(namecheck)
                          return(schoolurls)
                      })
    #####
    global <- reactiveValues(datapath = getwd())
    
    dir <- reactive(input$dir)
    
    '%!in%' <- function(x, y) {
        !('%in%'(x, y))
    }
    observeEvent(input$scrapeButton, {
        updateRadioButtons(session, "dataselector",
                           selected = "b")
    })
    # shinyDirChoose(
    #     input,
    #     'dir',
    #     roots = c(home = '~'),
    #     filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    # )
    # observeEvent(
    #     ignoreNULL = TRUE,
    #     eventExpr = {
    #         input$dir
    #     },
    #     handlerExpr = {
    #         req(is.list(input$dir))
    #         home <- normalizePath("~")
    #         global$datapath <-
    #             file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    #     }
    # )
    output$saveMerged <- downloadHandler(
        filename = function() {
            paste(
                'data-',
                Sys.Date(),
                "-T",
                input$tierSelect,
                "-Cycle",
                input$cycleSelect,
                '.csv',
                sep = ''
            )
        },
        content = function(con) {
            dfsave <- query_result()
            readr::write_csv(dfsave[order(dfsave$II, na.last = TRUE),], con , col_names = TRUE)
        }
    )
    
    output$tt1 <- renderUI(if (!is.null(recentD())) {
        h4(strong("Most Recent Decisions in the Data"), align = "center")
    })
    output$tt2 <- renderUI(if (!is.null(colortable())) {
        h4(strong("All Data - Default sorted by Decision Date"),
           align = "center")
    })
    
    dfScrape <- eventReactive(input$scrapeButton, {
        br <- Rcrawler::run_browser()
        mylist <- list()
        schoolurls <- schools()
        schoolurls <-
            data.frame(lapply(schoolurls, as.character), stringsAsFactors = FALSE)
        withProgress(message = 'Please Wait:',
                     detail = 'Get a coffee. Gathering data may take some time...',
                     value = 0,
                     {
                         for (i in seq_along(schoolurls$X1)) {
                             url <- schoolurls$X1[i]
                             
                             page1 <-
                                 Rcrawler::LinkExtractor(url = url, Browser = br,)
                             page1 <-
                                 page1[["Info"]][["Source_page"]]
                             page1 <- gsub('"', " ", page1)
                             page1 <-
                                 stringr::str_split(page1, "<a href= /users/creep/")
                             page1 <- unlist(page1)
                             page1 <- cbind(page1)
                             
                             xy <- data.frame(page1)
                             xy <- xy[!grepl("DOCTYPE", xy$page1), ]
                             xy <- data.frame(xy)
                             xy$xy <- as.character(xy$xy)
                             xy <- stringr::str_split(xy$xy, "',")
                             xy <- unlist(xy)
                             xy <- cbind(xy)
                             xy <- data.frame(xy)
                             xy$xy <- as.character(xy$xy)
                             xy$xy <- gsub('"', "", xy$xy)
                             xy$xy <- gsub("'", "", xy$xy)
                             xy$xy <- trimws(xy$xy)
                             test <- c("[", "]", "<td>")
                             xy <- xy[xy$xy %!in% test, ]
                             xy <- data.frame(xy)
                             xy$xy <- as.character(xy$xy)
                             df3 <- grep("order", xy$xy)
                             cut <- as.numeric(df3[1])
                             cut <- cut - 1
                             df.c <- xy[1:cut, ]
                             df3 <- grep("</a>", df.c)
                             riley2 <-
                                 cbind(
                                     df.c[df3],
                                     df.c[df3 + 1],
                                     df.c[df3 + 2],
                                     df.c[df3 + 3],
                                     df.c[df3 + 4],
                                     df.c[df3 + 5],
                                     df.c[df3 + 6],
                                     df.c[df3 + 7],
                                     df.c[df3 + 8],
                                     df.c[df3 + 9],
                                     df.c[df3 + 10],
                                     df.c[df3 + 11],
                                     df.c[df3 + 12]
                                 )
                             riley2 <- data.frame(riley2)
                             riley2$school <-
                                 paste(schoolurls$X2[i])
                             if (as.numeric(input$cycleSelect) == 15) {
                                 riley2$cycle <- paste("17-18")
                             }
                             if (as.numeric(input$cycleSelect) == 16) {
                                 riley2$cycle <- paste("18-19")
                             }
                             if (as.numeric(input$cycleSelect) == 17) {
                                 riley2$cycle <- paste("19-20")
                             }
                             if (as.numeric(input$cycleSelect) == 18) {
                                 riley2$cycle <- paste("20-21")
                             }
                             mylist[[i]] <- riley2
                             # write_csv(riley2, paste(schoolurls$X2[match(i, schoolurls$X1)], ".csv", sep = "") , col_names = TRUE)
                             if (i %% 2 == 0) {
                                 gc()
                                 
                             }
                             # tval <- as.numeric(input$tierSelect)
                             tval <- length(schoolurls$X1)
                             incProgress(amount = 1 / tval)
                         }
                         try(Rcrawler::stop_browser(br), silent = TRUE)
                         try(Rcrawler::stop_browser(br), silent = TRUE)
                         try(Rcrawler::stop_browser(br), silent = TRUE)
                         remove(br)
                         dat_csv <- do.call(rbind, mylist)
                         rm(mylist)
                         dat_csv <- as.data.frame(dat_csv)
                         dat_csv <-
                             data.frame(lapply(dat_csv, as.character), stringsAsFactors = FALSE)
                         dat_csv$X1 <- gsub(">.*", "", dat_csv$X1)
                         dat_csv$X1 <- trimws(dat_csv$X1)
                         dat_csv$X5 <-
                             gsub("\\+|<|>|=", " ", dat_csv$X5)
                         dat_csv$X5 <- substring(dat_csv$X5, 51)
                         dat_csv$X5 <-
                             ifelse(dat_csv$X5 == "", 0, 1)
                         dat_csv$X5 <- trimws(dat_csv$X5)
                         dat_csv$X7 <- gsub("\\$|,", "", dat_csv$X7)
                         dat_csv$X13 <-
                             gsub("\\]|,|\\[", "", dat_csv$X13)
                         dat_csv$X13 <- trimws(dat_csv$X13)
                         cnames <-
                             c(
                                 "User",
                                 "Result",
                                 "GPA",
                                 "LSAT",
                                 "URM",
                                 "WE",
                                 "Money",
                                 "Sent",
                                 "Received",
                                 "Complete",
                                 "UR",
                                 "II",
                                 "Decision",
                                 "School",
                                 "Cycle"
                             )
                         colnames(dat_csv) <- cnames
                         dat_csv <-
                             dat_csv[, c(1, 14, 15, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)]
                         dat_csv$GPA <-
                             round(as.numeric(dat_csv$GPA), 2)
                         dat_csv$LSAT <- as.integer(dat_csv$LSAT)
                         dat_csv$Money <- as.integer(dat_csv$Money)
                         dat_csv$URM <- as.factor(dat_csv$URM)
                         dat_csv$Sent <-
                             as.Date(dat_csv$Sent, "%Y/%m/%d")
                         dat_csv$Received <-
                             as.Date(dat_csv$Received, "%Y/%m/%d")
                         dat_csv$Complete <-
                             as.Date(dat_csv$Complete, "%Y/%m/%d")
                         dat_csv$UR <-
                             as.Date(dat_csv$UR, "%Y/%m/%d")
                         dat_csv$II <-
                             as.Date(dat_csv$II, "%Y/%m/%d")
                         dat_csv$Decision <-
                             as.Date(dat_csv$Decision, "%Y/%m/%d")
                         dat_csv$Result <- as.factor(dat_csv$Result)
                         dat_csv$WE <- as.factor(dat_csv$WE)
                         dat_csv$Cycle <- as.factor(dat_csv$Cycle)
                         return(dat_csv)
                     })
    })
    query_result <- reactive({
        if (input$dataselector == "a") {
            if (!is.null(dfLoad()) == TRUE) {
                return(dfLoad())
            }
        }
        if (input$dataselector == "b") {
            if (!is.null(dfScrape()) == TRUE) {
                return(dfScrape())
            }
        }
        if (input$dataselector == "c") {
            if (!is.null(dfMerge()) == TRUE) {
                return(dfMerge())
            }
        }
    })
    # shinyDirChoose(
    #     input,
    #     'dir',
    #     roots = c(home = '~'),
    #     filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    # )
    dfLoad <- eventReactive(input$loadButton, {
        # append(chat,"Loading")
        oldpath <-
            data.frame(lapply(input$dataload, as.character),
                       stringsAsFactors = FALSE)
        dat_csv <-
            readr::read_csv(file = oldpath[1, 4], col_names = TRUE)
        dat_csv$GPA <- round(as.numeric(dat_csv$GPA), 2)
        dat_csv$LSAT <- as.integer(dat_csv$LSAT)
        dat_csv$Money <- as.integer(dat_csv$Money)
        dat_csv$URM <- as.factor(dat_csv$URM)
        dat_csv$Sent <- as.Date(dat_csv$Sent, "%Y/%m/%d")
        dat_csv$Received <- as.Date(dat_csv$Received, "%Y/%m/%d")
        dat_csv$Complete <- as.Date(dat_csv$Complete, "%Y/%m/%d")
        dat_csv$UR <- as.Date(dat_csv$UR, "%Y/%m/%d")
        dat_csv$II <- as.Date(dat_csv$II, "%Y/%m/%d")
        dat_csv$Decision <- as.Date(dat_csv$Decision, "%Y/%m/%d")
        dat_csv$Result <- as.factor(dat_csv$Result)
        dat_csv$WE <- as.factor(dat_csv$WE)
        dat_csv$Cycle <- as.factor(dat_csv$Cycle)
        return(dat_csv)
    })
    observeEvent(input$loadButton, {
        updateRadioButtons(session, "dataselector",
                           selected = "a")
    })
    listOutput <- reactive({
        return(plyr::ddply(
            unique(query_result()[, c("School", "Cycle")]),
            plyr::.(School),
            plyr::summarize,
            Cycle = toString(Cycle)
        ))
    })
    
    colortable2 <- reactive({
        cols <- c(gg_color_hue(3)[1:2], "khaki")
        
        cr <- gg_color_hue(3)[1]
        ca <- gg_color_hue(3)[2]
        cw <- "khaki"
        
        cols2 <- c(rep(cr, 3), rep(ca, 9), rep(cw, 6))
        
        alist <-
            c(
                "Accepted",
                "Accepted, Attending",
                "Accepted, Deferred",
                "WL, Accepted",
                "WL, Accepted, Withdrawn",
                "WL, Accepted, Attending",
                "Acceptd, Deferred, Attending",
                "Accepted, Deferred, Withdrawn",
                "Accepted, Withdrawn"
            )
        rlist <-
            c("Rejected",
              "Rejected, Deferred",
              "Rejected, Withdrawn")
        wlist <-
            c(
                "Waitlisted",
                "Waitlisted, Deferred",
                "Waitlisted, Withdrawn",
                "WL, Rejected",
                "WL, Rejected, Withdrawn",
                "WL, Withdrawn"
            )
        
        return(
            DT::datatable(
                dfPlot(),
                options = list(order = list(14, 'desc')),
                rownames = FALSE
            ) %>% DT::formatStyle(
                "Result",
                target = "row",
                backgroundColor = DT::styleEqual(c(rlist, alist, wlist), cols2)
            ) %>% DT::formatStyle(columns = c(1:ncol(
                query_result()
            )), fontSize = '85%')
        )
    })
    
    output$temptable <-
        DT::renderDataTable(
            colortable2(),
            options = list(
                lengthMenu = c(25, 50, 100),
                pageLength = 25,
                autoWidth = FALSE,
                columnDefs = list(list(
                    width = '150px', targets = c(1, 3)
                )),
                order = list(1, 'asc')
            )
        )
    output$listtable <- renderTable(listOutput())
    
    colortable <- reactive({
        # cols <- c(gg_color_hue(3)[1:2],"khaki")
        
        cr <- gg_color_hue(3)[1]
        ca <- gg_color_hue(3)[2]
        cw <- "khaki"
        # cw <- gg_color_hue(3)[3]
        
        cols2 <- c(rep(cr, 3), rep(ca, 9), rep(cw, 6))
        
        alist <-
            c(
                "Accepted",
                "Accepted, Attending",
                "Accepted, Deferred",
                "WL, Accepted",
                "WL, Accepted, Withdrawn",
                "WL, Accepted, Attending",
                "Acceptd, Deferred, Attending",
                "Accepted, Deferred, Withdrawn",
                "Accepted, Withdrawn"
            )
        rlist <-
            c("Rejected",
              "Rejected, Deferred",
              "Rejected, Withdrawn")
        wlist <-
            c(
                "Waitlisted",
                "Waitlisted, Deferred",
                "Waitlisted, Withdrawn",
                "WL, Rejected",
                "WL, Rejected, Withdrawn",
                "WL, Withdrawn"
            )
        
        return(
            DT::datatable(
                query_result(),
                options = list(order = list(14, 'desc')),
                rownames = FALSE
            ) %>% DT::formatStyle(
                columns = c(1:ncol(query_result())),
                fontSize = '85%',
                background = "beige"
            ) %>% DT::formatStyle(
                "Result",
                target = "cell",
                backgroundColor = DT::styleEqual(c(rlist, alist, wlist), cols2)
            )
        )
    })
    output$rtable <-
        DT::renderDataTable(
            colortable(),
            options = list(
                lengthMenu = c(25, 50, 100),
                pageLength = 25,
                autoWidth = TRUE,
                columnDefs = list(list(
                    width = '200px', targets = c(1, 3)
                ))
            )
        )
    
    output$schSelect <- renderUI({
        s.choices <- listOutput()[, 1]
        selectInput(
            inputId = "pickschool",
            label = "Schools",
            choices = s.choices,
            selected = NULL
        )
    })
    
    recentD <- reactive({
        cols <- c(gg_color_hue(3)[1:2], "khaki")
        
        cr <- gg_color_hue(3)[1]
        ca <- gg_color_hue(3)[2]
        cw <- "khaki"
        # cw <- gg_color_hue(3)[3]
        
        cols2 <- c(rep(cr, 3), rep(ca, 9), rep(cw, 6))
        
        alist <-
            c(
                "Accepted",
                "Accepted, Attending",
                "Accepted, Deferred",
                "WL, Accepted",
                "WL, Accepted, Withdrawn",
                "WL, Accepted, Attending",
                "Acceptd, Deferred, Attending",
                "Accepted, Deferred, Withdrawn",
                "Accepted, Withdrawn"
            )
        rlist <-
            c("Rejected",
              "Rejected, Deferred",
              "Rejected, Withdrawn")
        wlist <-
            c(
                "Waitlisted",
                "Waitlisted, Deferred",
                "Waitlisted, Withdrawn",
                "WL, Rejected",
                "WL, Rejected, Withdrawn",
                "WL, Withdrawn"
            )
        temp <- query_result()
        temp <- temp[, c("School", "Result", "Decision")]
        temp <- temp[!is.na(temp$Decision), ]
        temp <- plyr::count(temp)
        temp <- data.frame(temp)
        temp$Decision <- as.Date(temp$Decision, "%Y/%m/%d")
        temp$Result <- as.factor(temp$Result)
        temp$freq <- as.numeric(temp$freq)
        # r.choices1[order(r.choices1$rank, na.last = TRUE),]
        
        return(
            DT::datatable(
                temp,
                options = list(order = list(2, 'desc')),
                rownames = FALSE
            ) %>% DT::formatStyle(
                "Result",
                target = "row",
                backgroundColor = DT::styleEqual(c(rlist, alist, wlist), cols2)
            ) %>% DT::formatStyle(columns = c(1:ncol(temp)), fontSize = '85%')
        )
        
    })
    
    output$dtable <-
        DT::renderDataTable(
            recentD(),
            options = list(
                lengthMenu = c(25, 50, 100),
                pageLength = 25,
                autoWidth = TRUE,
                columnDefs = list(list(
                    width = '200px', targets = c(1, 3)
                ))
            )
        )
    
    output$sch1 <- renderUI({
        s.choices <- listOutput()[, 1]
        selectInput(
            inputId = "school1",
            label = "Pick a School",
            choices = s.choices,
            selected = s.choices[1]
        )
    })
    output$sch2 <- renderUI({
        s.choices <- listOutput()[, 1]
        selectInput(
            inputId = "school2",
            label = "Pick a 2nd School",
            choices = s.choices,
            selected = s.choices[2]
        )
    })
    output$side1 <- renderUI({
        if (!is.null(query_result())) {
            s.choices <- listOutput()[, 1]
            checkboxGroupInput(
                inputId = "school_in",
                label = "Schools",
                choices = s.choices,
                selected = s.choices[1:2],
                inline = FALSE
            )
        }
    })
    output$side3 <- renderUI({
        if (!is.null(query_result())) {
            temp <- query_result()
            c.choices <- unique(temp$Cycle)
            checkboxGroupInput(
                inputId = "cycleid",
                label = "Cycles",
                choices = c.choices,
                selected = c.choices,
                inline = TRUE
            )
        }
    })
    output$preds <- renderUI({
        if (!is.null(query_result())) {
            temp <- listOutput()[, 1]
            selectInput(
                inputId = "predschool",
                label = "Choose a School",
                choices = temp,
                selected = temp[1],
                multiple = FALSE
            )
        }
    })
    output$side2 <- renderUI({
        if (!is.null(query_result()) && input$resultbin == "No") {
            r.choices1 <-
                plyr::ddply(unique(query_result()[, c("Result", "Cycle")]),
                            plyr::.(Result),
                            plyr::summarize,
                            Cycle = toString(Cycle))
            r.choices <- r.choices1[, 1]
            rm(r.choices1)
            
            checkboxGroupInput(
                inputId = "result_in",
                label = "Results",
                choices = r.choices,
                selected = c("Accepted", "Rejected", "Waitlisted"),
                inline = TRUE
            )
        }
    })
    
    output$sch_in <- renderUI({
        if (input$tierSelect == 225) {
            r.choices1 <- schools2()
            r.choices1$rank <- as.numeric(r.choices1$rank)
            r.choices1 <-
                r.choices1[order(r.choices1$rank, na.last = TRUE), ]
            r.choices1 <- r.choices1[, 2]
            
            selectInput(
                inputId = "allSelect",
                label = "Select Group of Schools",
                choices = r.choices1,
                selected = NULL,
                multiple = TRUE
            )
        }
    })
    
    
    
    rbin <- reactive({
        paste(input$resultbin)
    })
    
    dfPlot <- reactive({
        df.temp <- query_result()
        if (input$complete_yn == "Yes") {
            df.temp <-
                df.temp[df.temp$Complete >= input$complete_d[1] &
                            df.temp$Complete <= input$complete_d[2], ]
            df.temp <- df.temp[is.na(df.temp$User) == FALSE, ]
        }
        df.temp <- df.temp[df.temp$School %in% input$school_in, ]
        df.temp <- df.temp[is.na(df.temp$User) == FALSE, ]
        df.temp <- df.temp[is.na(df.temp$LSAT) == FALSE, ]
        df.temp <- df.temp[df.temp$Cycle %in% input$cycleid, ]
        df.temp <- df.temp[is.na(df.temp$User) == FALSE, ]
        df.temp <- df.temp[is.na(df.temp$LSAT) == FALSE, ]
        
        alist <-
            c(
                "Accepted",
                "Accepted, Attending",
                "Accepted, Deferred",
                "Accepted, Deferred, Attending",
                "Accepted, Deferred, Withdrawn",
                "Accepted, Withdrawn"
            )
        rlist <-
            c("Rejected",
              "Rejected, Deferred",
              "Rejected, Withdrawn")
        wlist <-
            c(
                "Waitlisted",
                "Waitlisted, Deferred",
                "Waitlisted, Withdrawn",
                "WL, Accepted",
                "WL, Accepted, Attending",
                "WL, Accepted, Withdrawn",
                "WL, Rejected",
                "WL, Rejected, Withdrawn",
                "WL, Withdrawn"
            )
        rlist2 <- c("Accepted", "Rejected", "Waitlisted")
        df.temp$Result <- as.character(df.temp$Result)
        if (input$resultbin == "Yes") {
            df.temp$Result[df.temp$Result %in% alist] <- "Accepted"
            df.temp$Result[df.temp$Result %in% rlist] <- "Rejected"
            df.temp$Result[df.temp$Result %in% wlist] <-
                "Waitlisted"
            df.temp <- df.temp[df.temp$Result %in% rlist2, ]
        } else{
            df.temp <- df.temp[df.temp$Result %in% input$result_in, ]
        }
        
        return(df.temp)
    })
    schoollist <- reactive({
        input$school_in
    })
    #
    # inv_cumsum <- function(x) {
    #     sum(x) - cumsum(x) + x
    # }
    #
    # StatFillLabels <- ggproto(
    #     "StatFillLabels",
    #     StatCount,
    #     compute_panel = function(self, data, scales, ...) {
    #         if (ggplot2:::empty(data)) {
    #             return(data.frame())
    #         }
    #         groups <- split(data, data$group)
    #         stats <- lapply(groups, function(group) {
    #             self$compute_group(data = group, scales = scales, ...)
    #         })
    #         stats <- mapply(function(new, old) {
    #             if (ggplot2:::empty(new)) {
    #                 return(data.frame())
    #             }
    #             unique <- ggplot2:::uniquecols(old)
    #             missing <- !(names(unique) %in% names(new))
    #             cbind(new, unique[rep(1, nrow(new)), missing, drop = FALSE])
    #         }, stats, groups, SIMPLIFY = FALSE)
    #         data <- do.call(plyr::rbind.fill, stats)
    #         plyr::ddply(
    #             data, "x", plyr::mutate,
    #             prop = count / sum(count),
    #             cumprop = inv_cumsum(count) / sum(count),
    #             ylabel = (inv_cumsum(count) - count / 2) / sum(count),
    #             na.rm = TRUE
    #         )
    #     },
    #     default_aes = aes(y = ..ylabel.., label = paste0(round(100 * ..prop.., digits = 1), "%"))
    # )
    #
    # stat_fill_labels <- function(mapping = NULL, data = NULL, geom = "text",
    #                              position = "identity", width = NULL, na.rm = FALSE, show.legend = NA,
    #                              inherit.aes = TRUE, ...) {
    #     layer(
    #         stat = StatFillLabels, data = data, mapping = mapping, geom = geom,
    #         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    #         params = list(na.rm = na.rm, ...)
    #     )
    # }
    #
    # StatStackLabels <- ggproto(
    #     "StatStackLabels",
    #     StatCount,
    #     compute_panel = function(self, data, scales, ...) {
    #         if (ggplot2:::empty(data)) {
    #             return(data.frame())
    #         }
    #         groups <- split(data, data$group)
    #         stats <- lapply(groups, function(group) {
    #             self$compute_group(data = group, scales = scales, ...)
    #         })
    #         stats <- mapply(function(new, old) {
    #             if (ggplot2:::empty(new)) {
    #                 return(data.frame())
    #             }
    #             unique <- ggplot2:::uniquecols(old)
    #             missing <- !(names(unique) %in% names(new))
    #             cbind(new, unique[rep(1, nrow(new)), missing, drop = FALSE])
    #         }, stats, groups, SIMPLIFY = FALSE)
    #         data <- do.call(plyr::rbind.fill, stats)
    #         plyr::ddply(
    #             data, "x", plyr::mutate,
    #             cumcount = inv_cumsum(count),
    #             ylabel = inv_cumsum(count) - count / 2,
    #             na.rm = TRUE
    #         )
    #     },
    #     default_aes = aes(y = ..ylabel.., label = ..count..)
    # )
    #
    # stat_stack_labels <- function(mapping = NULL, data = NULL, geom = "text",
    #                               position = "identity", width = NULL, na.rm = FALSE, show.legend = NA,
    #                               inherit.aes = TRUE, ...) {
    #     layer(
    #         stat = StatStackLabels, data = data, mapping = mapping, geom = geom,
    #         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    #         params = list(na.rm = na.rm, ...)
    #     )
    # }
    plot3 <- reactive({
        df.temp2 <- dfPlot()
        df.temp2 <-
            df.temp2[df.temp2$LSAT >= input$lsat_in[1] &
                         df.temp2$LSAT <= input$lsat_in[2], ]
        df.temp2 <- df.temp2[is.na(df.temp2$User) == FALSE, ]
        df.temp2 <-
            df.temp2[df.temp2$GPA >= input$gpa_in[1] &
                         df.temp2$GPA <= input$gpa_in[2], ]
        df.temp2 <- df.temp2[is.na(df.temp2$User) == FALSE, ]
        
        df.temp2$LSAT <- as.factor(df.temp2$LSAT)
        df.temp2$Result <- as.factor(df.temp2$Result)
        cols <- c(gg_color_hue(3)[1:2], "khaki")
        
        cr <- gg_color_hue(3)[1]
        ca <- gg_color_hue(3)[2]
        cw <- "khaki"
        # cw <- gg_color_hue(3)[3]
        
        cols2 <- c(cr, ca, cw)
        if (input$resultbin == "Yes") {
            p1 <- ggplot2::ggplot(df.temp2, ggplot2::aes(x = LSAT,
                                                         fill = Result)) +
                ggplot2::geom_bar(colour = "black") +
                ggplot2::facet_grid(. ~ School, scales = "free_y") +
                ggplot2::scale_x_discrete(drop = FALSE) +
                stat_stack_labels(color = "black") +
                ggplot2::scale_fill_manual(values = c(
                    "Accepted" = ca,
                    "Rejected" = cr,
                    "Waitlisted" = cw
                ))
            
            p2 <- ggplot2::ggplot(df.temp2, ggplot2::aes(x = LSAT,
                                                         fill = Result)) +
                ggplot2::geom_bar(position = "fill") +
                ggplot2::facet_grid(. ~ School) +
                ggplot2::scale_x_discrete(drop = FALSE) +
                stat_fill_labels() +
                ggplot2::scale_fill_manual(values = c(
                    "Accepted" = ca,
                    "Rejected" = cr,
                    "Waitlisted" = cw
                ))
            
        } else{
            p1 <- ggplot2::ggplot(df.temp2, ggplot2::aes(
                x = LSAT,
                fill = Result,
                color = Result
            )) +
                ggplot2::geom_bar(colour = "black") +
                ggplot2::facet_grid(. ~ School, scales = "free_y") +
                ggplot2::scale_x_discrete(drop = FALSE) +
                stat_stack_labels(color = "black")
            
            p2 <- ggplot2::ggplot(df.temp2, ggplot2::aes(x = LSAT,
                                                         fill = Result)) +
                ggplot2::geom_bar(position = "fill") +
                ggplot2::facet_grid(. ~ School) +
                ggplot2::scale_x_discrete(drop = FALSE) +
                stat_fill_labels()
        }
        
        if (input$bar_type == "Stack") {
            return(plotly::ggplotly(p1, tooltip = c("x", "y", "n", "fill")))
        }
        if (input$bar_type == "Fill") {
            return(plotly::ggplotly(p2))
        }
        # p3 <- p2
        # ggplotly(p3)
    })
    output$hist <- plotly::renderPlotly({
        plot3()
    })
    output$waves <- plotly::renderPlotly({
        df1 <- dfPlot()
        
        df1$Decision <- as.Date(df1$Decision)
        df1 <- df1[df1$Decision >= as.Date("2017-1-1"), ]
        df1 <- df1[is.na(df1$User) == FALSE, ]
        df1$Result <- as.factor(df1$Result)
        
        cols <- c(gg_color_hue(3)[1:2], "khaki")
        
        cr <- gg_color_hue(3)[1]
        ca <- gg_color_hue(3)[2]
        cw <- "khaki"
        # cw <- gg_color_hue(3)[3]
        
        cols2 <- c(cr, ca, cw)
        if (input$resultbin == "Yes") {
            p1 <- ggplot2::ggplot(data = df1, ggplot2::aes(
                x = Decision,
                y = Result,
                label = as.Date(Decision)
            )) +
                ggplot2::geom_count(ggplot2::aes(fill = Result),
                                    color = "black",
                                    alpha = 0.7) +
                ggplot2::facet_grid(School ~ .) +
                ggplot2::scale_x_date(
                    "Time",
                    limits = c(as.Date(min(
                        df1$Decision
                    )),
                    as.Date(max(
                        df1$Decision
                    ))),
                    date_breaks = "1 week",
                    date_labels = "%m-%d"
                ) +
                ggplot2::scale_fill_manual(values = c(
                    "Accepted" = ca,
                    "Rejected" = cr,
                    "Waitlisted" = cw
                ))
        } else{
            p1 <- ggplot2::ggplot(data = df1, ggplot2::aes(
                x = Decision,
                y = Result,
                label = as.Date(Decision)
            )) +
                ggplot2::geom_count(ggplot2::aes(fill = Result),
                                    color = "black",
                                    alpha = 0.7) +
                ggplot2::facet_grid(School ~ .) +
                ggplot2::scale_x_date(
                    "Time",
                    limits = c(as.Date(min(
                        df1$Decision
                    )),
                    as.Date(max(
                        df1$Decision
                    ))),
                    date_breaks = "1 week",
                    date_labels = "%m-%d"
                )
        }
        return(plotly::ggplotly(p1, tooltip = c("n", "label")))
        # ggtitle("February 2020 T13 Waves")
    })
    
    output$predplot1 <- plotly::renderPlotly({
        df_t <- query_result()
        school_name <- input$predschool
        df_t$Result[df_t$Result %in% c('Accepted, Withdrawn')] <-
            "Accepted"
        df_t$Result[df_t$Result %in% c('Accepted, Attending')] <-
            "Accepted"
        df_t$Result[df_t$Result %in% c('WL, Accepted, Attending')] <-
            "Waitlisted"
        df_t$Result[df_t$Result %in% c('WL, Accepted, Withdrawn')] <-
            "Waitlisted"
        df_t$Result[df_t$Result %in% c('Waitlisted, Withdrawn')] <-
            "Waitlisted"
        df_t$Result[df_t$Result %in% c('WL, Withdrawn')] <-
            "Waitlisted"
        df_t$Result[df_t$Result %in% c('Rejected, Withdrawn')] <-
            "Rejected"
        
        df_t <-
            subset(df_t, School == school_name) #changed from df to alldata because I commented out urm filter
        df_t <- subset(df_t, URM == input$urm_in)
        if (input$sent_yn2 == 1) {
            df_t <- subset(df_t, lubridate::month(Sent) %in% input$month_in)
        }
        df_t$pr <- ifelse(df_t$Result == "Accepted", 1, 0)
        df_t <- subset(df_t, is.na(df_t$GPA) == FALSE)
        df_t <- subset(df_t, is.na(df_t$pr) == FALSE)
        model <-
            stats::glm(data = df_t,
                       pr ~ GPA + LSAT,
                       family = stats::binomial(link = logit))
        
        v1 <- visreg::visreg(
            model,
            "LSAT",
            scale = "response",
            rug = 2,
            xlab = "LSAT Score",
            ylab = "Pr(Accepted)",
            type = "conditional",
            by = "GPA",
            breaks = c(input$gpa_in2-0.05,input$gpa_in2,input$gpa_in2+0.05),
            gg = TRUE)+
            ggplot2::scale_x_continuous(limits = c(150,180),breaks = c(150:180))
        
        return(plotly::ggplotly(v1+ggplot2::ggtitle(paste(school_name," - ","Pr(Accepted) by LSAT by GPA",sep = ""))))
    })
    
    output$predplot2 <- plotly::renderPlotly({
        df_t <- query_result()
        school_name <- input$predschool
        df_t$Result[df_t$Result %in% c('Accepted, Withdrawn')] <-
            "Accepted"
        df_t$Result[df_t$Result %in% c('Accepted, Attending')] <-
            "Accepted"
        df_t$Result[df_t$Result %in% c('WL, Accepted, Attending')] <-
            "Waitlisted"
        df_t$Result[df_t$Result %in% c('WL, Accepted, Withdrawn')] <-
            "Waitlisted"
        df_t$Result[df_t$Result %in% c('Waitlisted, Withdrawn')] <-
            "Waitlisted"
        df_t$Result[df_t$Result %in% c('WL, Withdrawn')] <-
            "Waitlisted"
        df_t$Result[df_t$Result %in% c('Rejected, Withdrawn')] <-
            "Rejected"
        
        df_t <-
            subset(df_t, School == school_name) #changed from df to alldata because I commented out urm filter
        df_t <- subset(df_t, URM == input$urm_in)
        if (input$sent_yn2 == 1) {
            df_t <- subset(df_t, lubridate::month(Sent) %in% input$month_in)
        }
        df_t$pr <- ifelse(df_t$Result == "Accepted", 1, 0)
        df_t <- subset(df_t, is.na(df_t$GPA) == FALSE)
        df_t <- subset(df_t, is.na(df_t$pr) == FALSE)
        model <-
            stats::glm(data = df_t,
                       pr ~ GPA + LSAT,
                       family = stats::binomial(link = logit))
        
        v1 <- visreg::visreg(
            model,
            "GPA",
            scale = "response",
            rug = 2,
            xlab = "GPA",
            ylab = "Pr(Accepted)",
            type = "conditional",
            by = "LSAT",
            breaks = c(input$lsat_in2-1,input$lsat_in2,input$lsat_in2+1),
            gg = TRUE)+
            ggplot2::scale_x_continuous(limits = c(2.5,4.33),breaks = c(seq(2.5,4.3,0.1)))
        
        return(plotly::ggplotly(v1+ggplot2::ggtitle(paste(school_name," - ","Pr(Accepted) by GPA by LSAT",sep = ""))))
    })
    
    
    
    query_result2 <-  reactive({
        df5 <- query_result()
        xy <-
            vector("list", 0) # create an empty list into which values are to be filled
        school_names <- listOutput()[, 1]
        for (i in school_names) {
            df_t <-
                subset(df5, School == i) #changed from df to alldata because I commented out urm filter
            df5$Result[df5$Result %in% c('Accepted, Withdrawn')] <-
                "Accepted"
            df5$Result[df5$Result %in% c('Accepted, Attending')] <-
                "Accepted"
            df5$Result[df5$Result %in% c('WL, Accepted, Attending')] <-
                "Waitlisted"
            df5$Result[df5$Result %in% c('WL, Accepted, Withdrawn')] <-
                "Waitlisted"
            df5$Result[df5$Result %in% c('Waitlisted, Withdrawn')] <-
                "Waitlisted"
            df5$Result[df5$Result %in% c('WL, Withdrawn')] <-
                "Waitlisted"
            df5$Result[df5$Result %in% c('Rejected, Withdrawn')] <-
                "Rejected"
            df_t <- subset(df_t, URM == input$urm_in)
            if (input$sent_yn2 == 1) {
                df_t <- subset(df_t, lubridate::month(Sent) %in% input$month_in)
            }
            df_t$pr <- ifelse(df_t$Result == "Accepted", 1, 0)
            df_t <- subset(df_t, is.na(df_t$GPA) == FALSE)
            df_t <- subset(df_t, is.na(df_t$pr) == FALSE)
            model <-
                stats::glm(data = df_t,
                           pr ~ GPA + LSAT,
                           family = stats::binomial(link = logit))
            g.seq <- data.frame(matrix(ncol = 2, nrow = 1))
            colnames(g.seq) <- c("GPA", "LSAT")
            g.seq$GPA <- input$gpa_in2
            g.seq$LSAT <- input$lsat_in2
            m.df <- g.seq
            m.df$pr <-
                stats::predict(model, newdata = m.df, type = "response")
            m.df$School <- i
            m.df <-
                data.frame(lapply(m.df, as.character), stringsAsFactors = FALSE)
            ilink <- stats::family(model)$linkinv
            pd <-
                cbind(g.seq,
                      stats::predict(model, g.seq, type = "link", se.fit = TRUE)[1:2])
            pd <-
                transform(
                    pd,
                    Fitted = ilink(fit),
                    Upper = ilink(fit + (2 * se.fit)),
                    Lower = ilink(fit - (2 * se.fit))
                )
            pd <- pd[, -c(2:4)]
            pd <-
                data.frame(lapply(pd, as.character), stringsAsFactors = FALSE)
            df_w <-
                subset(df5, School == i) #changed from df to alldata because I commented out urm filter
            df_w <- subset(df_w, URM == input$urm_in)
            if (input$sent_yn2 == 1) {
                df_w <- subset(df_w, lubridate::month(Sent) %in% input$month_in)
            }
            df_w$wpr <- ifelse(df_w$Result == "Waitlisted", 1, 0)
            df_w <- subset(df_w, is.na(df_w$GPA) == FALSE)
            df_w <- subset(df_w, is.na(df_w$wpr) == FALSE)
            wmodel <-
                stats::glm(data = df_w,
                           wpr ~ GPA + LSAT,
                           family = stats::binomial(link = logit))
            w.df <- g.seq
            w.df$wpr <-
                stats::predict(wmodel, newdata = w.df, type = "response")
            w.df <-
                data.frame(lapply(w.df, as.character), stringsAsFactors = FALSE)
            ilink <- stats::family(wmodel)$linkinv
            wpd <-
                cbind(g.seq,
                      stats::predict(wmodel, g.seq, type = "link", se.fit = TRUE)[1:2])
            wpd <-
                transform(
                    wpd,
                    Fitted = ilink(fit),
                    WUpper = ilink(fit + (2 * se.fit)),
                    WLower = ilink(fit - (2 * se.fit))
                )
            wpd <- wpd[, -c(2:4)]
            wpd <-
                data.frame(lapply(wpd, as.character), stringsAsFactors = FALSE)
            m.df <- merge(m.df, w.df, by = "GPA")
            pd <- merge(pd, wpd, by = "GPA")
            xy[[i]] <- merge(m.df, pd)
        }
        xy <- do.call(rbind, xy)
        dffr <- xy
        dffr$LSAT.x <- NULL
        dffr$LSAT.y <- NULL
        dffr$Fitted.x <- NULL
        dffr$Fitted.y <- NULL
        dffr$GPA <- NULL
        dffr$pr <- round(as.numeric(dffr$pr), 4) * 100
        dffr$wpr <- round(as.numeric(dffr$wpr), 4) * 100
        dffr$Upper <- round(as.numeric(dffr$Upper), 4) * 100
        dffr$WUpper <- round(as.numeric(dffr$WUpper), 4) * 100
        dffr$Lower <- round(as.numeric(dffr$Lower), 4) * 100
        dffr$WLower <- round(as.numeric(dffr$WLower), 4) * 100
        dffr$conf <-
            paste(dffr$Lower, "% to ", dffr$Upper, "%", sep = "")
        dffr$wconf <-
            paste(dffr$WLower, "% to ", dffr$WUpper, "%", sep = "")
        ranklist <- schools2()
        dffr$rank <- ranklist$rank[match(dffr$School, ranklist$X2)]
        dffr$rank <- as.integer(dffr$rank)
        dffr$School <- NULL
        dffr$Lower <- NULL
        dffr$Upper <- NULL
        dffr$WLower <- NULL
        dffr$WUpper <- NULL
        dffr <- dffr[, c(5, 1, 3, 2, 4)]
        CN <-
            c(
                "Rank",
                "Pr. Accepted (%)",
                "A 95% Conf. Intvl.",
                "Pr. WL (%)",
                "WL 95% Conf. Intvl."
            )
        colnames(dffr) <- CN
        
        
        
        
        
        
        dffr <-
            DT::datatable(
                dffr,
                rownames = TRUE,
                options = list(
                    lengthMenu = c(50, 100),
                    pageLength = 50,
                    order = list(1, 'asc')
                )
            ) %>% DT::formatStyle(
                "Pr. Accepted (%)",
                background = DT::styleColorBar(range(c(0, 100)), 'limegreen'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center'
            ) %>% DT::formatStyle(
                "Pr. WL (%)",
                background = DT::styleColorBar(range(c(0, 100)), 'khaki'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center'
            )
        dffr
        
        # return(datatable(dfPlot(),options = list(order=list(14,'desc')),rownames = FALSE)%>% formatStyle(
        #     "Result",
        #     target = "row",
        #     backgroundColor = styleEqual(c(rlist,alist,wlist),cols2))%>% formatStyle(columns = c(1:ncol(query_result())),fontSize = '85%'))
        #
    })
    
    
    output$rtable2 <-
        DT::renderDataTable(
            query_result2(),
            options = list(
                lengthMenu = c(50, 100),
                pageLength = 50,
                autoWidth = TRUE,
                columnDefs = list(list(
                    width = '200px', targets = c(1, 3)
                )),
                order = list(1, 'asc')
            )
        )
    
    dfMerge <- eventReactive(input$mergeButton, {
        df.temp1 <- dfScrape()
        
        if (is.null(dfLoad())) {
            downloaddata <- df.temp1
            rm(df.temp1)
            return(downloaddata)
            # break()
        }
        
        olddata <- dfLoad()
        
        olddata$check <-
            ifelse(is.na(match(
                paste(olddata$User, olddata$School, olddata$Cycle, sep = ""),
                paste(df.temp1$User, df.temp1$School, df.temp1$Cycle, sep = "")
            )), "No", "Yes")
        
        olddata <- olddata[olddata$check == "No", ]
        olddata$check <- NULL
        return(rbind(olddata, df.temp1))
    })
    
    observeEvent(input$mergeButton, {
        updateRadioButtons(session, "dataselector",
                           selected = "c")
    })
    
    
    # posn.j <- position_jitter(width = 0.1,height = 0.01)
    # posn.j <- position_jitter(width = 0.2,height = 0.05)
    #
    # png(file="JS5.png", width=2200, height=1300, res=160)
    # ggplot(df10, aes(x = lsat, y = gpa, color = factor(result)))+
    #     geom_point(size = 5, alpha = 0.3, position = posn.j)+
    #     scale_color_manual(values = c(mycols[1],mycols[2],mycols[3],mycols[4]))+
    #     scale_x_continuous(limits = c(160,180), breaks = c(162:180))+
    #     scale_y_continuous(limits = c(3.55,4.21), breaks = b1)+
    #     theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "darkgrey"))
    # dev.off()
    output$schol1 <- plotly::renderPlotly({
        df1 <- query_result()
        posn.j <- ggplot2::position_jitter(width = 0.2, height = 0.05)
        df1 <- df1[df1$School %in% input$school1, ]
        df1 <- df1[df1$Money != 0, ]
        df1 <- df1[is.na(df1$Money) == FALSE, ]
        df1 <- df1[df1$Money >= as.numeric(input$money_val1), ]
        df1 <- df1[df1$Money <= as.numeric(input$money_val2), ]
        df1 <- df1[is.na(df1$Money) == FALSE, ]
        df1 <- df1[is.na(df1$GPA) == FALSE, ]
        gmin <- min(df1$GPA)
        gmax <- max(df1$GPA)
        b1 <- seq(gmin, gmax, 0.05)
        df1$Money <- as.factor(df1$Money)
        df1$LSAT <- as.factor(df1$LSAT)
        p1 <- ggplot2::ggplot(df1, ggplot2::aes(
            x = LSAT,
            y = GPA,
            label = User
        )) +
            ggplot2::geom_point(
                ggplot2::aes(fill = Money),
                color = "black",
                size = 2.4,
                alpha = 0.7,
                position = posn.j
            ) +
            ggplot2::scale_y_continuous("GPA",
                                        limits = c(gmin, gmax),
                                        breaks = b1) +
            ggplot2::scale_x_discrete("LSAT") +
            ggplot2::geom_text(ggplot2::aes(label = User),
                               color = "white",
                               nudge_y = -100) +
            ggplot2::ggtitle(paste(df1$School))
        return(plotly::ggplotly(p1))
    })
    
    output$schol2 <- plotly::renderPlotly({
        df1 <- query_result()
        posn.j <- ggplot2::position_jitter(width = 0.2, height = 0.05)
        df1 <- df1[df1$School %in% input$school2, ]
        df1 <- df1[df1$Money != 0, ]
        df1 <- df1[is.na(df1$Money) == FALSE, ]
        df1 <- df1[df1$Money >= as.numeric(input$money_val1), ]
        df1 <- df1[df1$Money <= as.numeric(input$money_val2), ]
        df1 <- df1[is.na(df1$Money) == FALSE, ]
        df1 <- df1[is.na(df1$GPA) == FALSE, ]
        gmin <- min(df1$GPA)
        gmax <- max(df1$GPA)
        b1 <- seq(gmin, gmax, 0.05)
        df1$Money <- as.factor(df1$Money)
        df1$LSAT <- as.factor(df1$LSAT)
        p1 <- ggplot2::ggplot(df1, ggplot2::aes(
            x = LSAT,
            y = GPA,
            label = User
        )) +
            ggplot2::geom_point(
                ggplot2::aes(fill = Money),
                color = "grey2",
                size = 2.4,
                alpha = 0.7,
                position = posn.j
            ) +
            ggplot2::scale_y_continuous("GPA",
                                        limits = c(gmin, gmax),
                                        breaks = b1) +
            ggplot2::scale_x_discrete("LSAT") +
            ggplot2::geom_text(ggplot2::aes(label = User),
                               color = "white",
                               nudge_y = -100) +
            ggplot2::ggtitle(paste(df1$School))
        return(plotly::ggplotly(p1, tooltip = "all"))
    })
    
    # output$waves <- renderPlot({
    #     df1 <- dfPlot()
    #
    #     p1 <- ggplot(df.temp2, aes(
    #         x = factor(LSAT),
    #         y = GPA
    #     )) +
    #         geom_bar(colour = "black") +
    #         facet_grid(. ~ School) +
    #         scale_x_discrete(drop = FALSE)
    #
    #     ggplot(data = df1, aes(
    #         x = as.Date(Decision),
    #         y = factor(Result),
    #         color = Result
    #     )) +
    #         geom_count() +
    #         facet_grid(School ~ .) +
    #         scale_x_date(
    #             "Time",
    #             limits = c(
    #                 as.Date(input$date_range[1]),
    #                 as.Date(input$date_range[2])
    #             ),
    #             date_breaks = "1 week",
    #             date_labels = "%a%d"
    #         )
    # })
    
    
    
    
}
# Run the application
shinyApp(ui = ui, server = server)
