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
if (is.null(suppressMessages(webshot:::find_phantom()))) {
    webshot::install_phantomjs()
}
# library(graph)
# install_github("larmarange/JLutils", lib = )
# gginnards,rtweet,selectr,debugme,showimage,curl,webutils,knitr,ps,processx,callr,iterators,generics,farver,foreach,tidyr,httr,htmlwidgets,webshot,webdriver,doParallel,cellranger,withr,hms,tidyselect,vctrs,data.table,plyr,plotly,scales,splitstackshape,readr,shiny,readxl,Rcrawler,xml2,shinyFiles,lubridate,formattable,DT,visreg,htmltools,rlang,httpuv,later,promises,retry,websocket,fastmap,crayon,digest,xtable,jsonlite,mime,pillar,lifecycle,shinyjs,shinythemes




"%||%" <- function(x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}

'%!in%' <- function(x, y) {
    !('%in%'(x, y))
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

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(
        HTML(
            ".shiny-notification {
             position:fixed;
             top: calc(45%);
             left: calc(43%);
             width: 320px;
             }
             "
        )
    )),
    navbarPage(
        theme = shinythemes::shinytheme("flatly"),
        "LSD Explorer",
        tabPanel("Data Management",
                 sidebarLayout(
                     sidebarPanel(
                         h4(strong("Data-View:")),
                         p(
                             "Here you may choose between different sets of data. This selection will be used for the other pages."
                         ),
                         radioButtons(
                             inputId = "dataselector",
                             label = "Select Data",
                             choices = c(
                                 "Imported Previous Dataset" = "a",
                                 "Downloaded New Data" = "b",
                                 "Merged Old Data and New Data" = "c"
                             ),
                             selected = "a"
                         ),
                         h4("_________________"),
                         h4(strong("Get Data from LSD:")),
                         p(
                             "Here you can download live data from LawSchoolData.org. Select the Cycle and and Schools you wish to download"
                         ),
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
                             label = "Get Data...",
                             icon = NULL,
                             width = NULL
                         )
                         ,
                         h4("_________________"),
                         h4(strong("Import Data:")),
                         p(
                             "Here you may load a previously saved dataset. Select the file then click Import"
                         ),
                         fileInput(
                             inputId = "dataload",
                             label = "Browse for File",
                             multiple = FALSE,
                             buttonLabel = "Browse..",
                             placeholder = "No files selected"
                         ),
                         actionButton(
                             inputId = "loadButton",
                             label = "Import Dataset",
                             icon = NULL,
                             width = NULL
                         )
                         ,
                         h4("_________________"),
                         h4(strong("Merge:")),
                         p(
                             "Here you may combine an imported old data set with any new data you have downloaded"
                         ),
                         actionButton(
                             inputId = "mergeButton",
                             label = "Merge",
                             icon = NULL,
                             width = NULL
                         ),
                         p("Save Currently Selected Data-View"),
                         downloadButton(
                             outputId = "saveMerged",
                             label = "Save",
                             icon = NULL,
                             width = NULL
                         ),
                         width = 3
                         
                     ),
                     mainPanel(
                         uiOutput("introtext"),
                         uiOutput("introtext2"),
                         uiOutput("frontpage")
                     )
                 )),
        tabPanel("View Data",
                 sidebarLayout(
                     sidebarPanel(
                         p(
                             "Specify an LSAT and GPA range to adjust the data visible in the histogram."
                         ),
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
                             inputId = "plots_urm",
                             label = "URM Filter",
                             choices = c(
                                 "Don't filter" = 2,
                                 "Show only non-URM" = 0,
                                 "Show only URM" = 1
                             ),
                             selected = 2,
                             inline = TRUE
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
                         p(
                             em(
                                 "You may filter the data for the time-series plot by application 'Complete' dates to view when individuals with certain complete dates received their decisions"
                             )
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
                         tabsetPanel(
                         tabPanel(
                             "Overview",
                             plotly::plotlyOutput("hist"),
                             plotly::plotlyOutput("waves"),
                             DT::dataTableOutput("temptable")
                         ),
                         tabPanel(
                             "User Stats",
                             plotly::plotlyOutput("user_graph"),
                             DT::dataTableOutput("temptable3")
                         )))
                 ,fluid = TRUE)),
        tabPanel("Scholarships",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("sch1"),
                         uiOutput("sch2"),
                         h5(
                             "The Legends are interactive. Click a money amount to remove it or double-click to isolate it."
                         ),
                         radioButtons(
                             inputId = "schol_urm",
                             label = "Isolate URM observations?",
                             choices = c("Yes" = 1, "No" = 0),
                             selected = 0,
                             inline = TRUE
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
                     mainPanel(
                         plotly::plotlyOutput("schol1"),
                         plotly::plotlyOutput("schol2"),
                     )
                 )),
        tabPanel("Predictor",
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons(
                             inputId = "predcurve1",
                             label = "Show or Hide GPA curve",
                             choices = c("Show" = 1, "Hide" = 0),
                             selected = 1,
                             inline = TRUE,
                             width = NULL
                         ),
                         radioButtons(
                             inputId = "predcurve2",
                             label = "Show or Hide LSAT curve",
                             choices = c("Show" = 1, "Hide" = 0),
                             selected = 0,
                             inline = TRUE,
                             width = NULL
                         ),
                         tags$br(),
                         h4("Enter your stats:"),
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
                         uiOutput("months"),
                         width = 3
                     ),
                     mainPanel(
                         uiOutput("preds"),
                         uiOutput("preds_points"),
                         uiOutput("notice"),
                         uiOutput("predplot11"),
                         uiOutput("predplot22"),
                         DT::dataTableOutput("rtable2")
                         
                     )
                 )),
        tabPanel("Twitter",
                 sidebarLayout(
                     sidebarPanel(
                         textInput(
                             inputId = "twitUN",
                             label = "Enter the person's Twitter handle without the '@' symbol",
                             placeholder = "Your Favorite Dean!"
                         ),
                         numericInput(
                             inputId = "num_tweets",
                             label = "How many tweets to download?",
                             min = 0,
                             max = 500,
                             value = 0
                         ),
                         radioButtons(
                             inputId = "rtButton",
                             label = "Include Retweets?",
                             choices = c("Yes" = 1, "No" = 0),
                             selected = 0,
                             inline = TRUE
                         ),
                         actionButton(inputId = "tweetButton",
                                      label = "Get Tweets"),
                         uiOutput("twitwarn"),
                         p(
                             em(
                                 "You are limited to query a maximum of 100,000 per day by the Twitter API."
                             )
                         ),
                         p(
                             em(
                                 "Each time you change a parameter, it initializes a new query and these add to your 100,000 limit."
                             )
                         ),
                         width = 2
                     ),
                     mainPanel(tabsetPanel(
                         tabPanel("Table", DT::dataTableOutput("tweet_table")),
                         tabPanel("Time Series", plotly::plotlyOutput("tweet_time_series"))
                     ))
                 ))
    )
)


# ui <- shinymanager::secure_app(
#     head_auth = tags$script(inactivity),
#     ui,
#     theme = shinythemes::shinytheme("flatly")
# )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # result_auth <-
    #     shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials))
    # 
    # output$res_auth <- renderPrint({
    #     reactiveValuesToList(result_auth)
    # })
    ##
    # init.count <- reactiveVal()
    # init.count(1)
    # initdata <-
    #     readr::read_csv(file = "LSDEdata-2020-05-04.csv", col_names = TRUE)
    # observeEvent(input$loadButton, {
    #     newval <- init.count() + 1
    #     init.count(newval)
    # })
    # observeEvent(input$scrapeButton, {
    #     newval <- init.count() + 1
    #     init.count(newval)
    # })
    ##
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
                "&school=University+of+Michigan+Ann+Arbor",
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
                "University of Michigan Ann Arbor"         ,
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
                10,
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
        schoolurls$rank <-
            as.integer(as.character(schoolurls$rank))
        schoolurls$X1 <- as.character(schoolurls$X1)
        schoolurls$X2 <- as.character(schoolurls$X2)
        rm(temp)
        rm(b1)
        rm(b2)
        rm(b3)
        rm(ranks)
        return(schoolurls)
    })
    
    val <- reactiveVal(1)
    
    observeEvent(input$loadButton, {
        val(2)
    }, once = TRUE)
    
    observeEvent(input$scrapeButton, {
        val(2)
    }, once = TRUE)
    
    output$introtext <- renderUI(if (val() == 1) {
        h4(
            "To get started, select schools to download their data or import a previously saved dataset."
        )
    })
    output$introtext2 <- renderUI(if (val() == 1) {
        p(em("Preloaded data last updated May 4, 2020"))
    })
    
    output$frontpage <- renderUI(if (!is.null(query_result())) {
        conditionalPanel(condition = "'val' != 1",
                         tabsetPanel(
                             tabPanel(
                                 "Data Table",
                                 uiOutput("tt2"),
                                 DT::dataTableOutput("rtable")
                             ),
                             tabPanel(
                                 "Recent Decisions",
                                 uiOutput("tt1"),
                                 DT::dataTableOutput("dtable")
                             ),
                             tabPanel("Schools and Cycles", tableOutput("listtable"))
                         ))
    })
    schools <-
        eventReactive(c(input$tierSelect, input$cycleSelect, input$allSelect),
                      {
                          schoolurls <- schools2()
                          schoolurls$X1 <-
                              as.character(schoolurls$X1)
                          schoolurls$X2 <-
                              as.character(schoolurls$X2)
                          if (input$tierSelect == 225) {
                              namecheck <- input$allSelect
                              schoolurls <-
                                  schoolurls[schoolurls$X2 %in% namecheck, ]
                          } else{
                              rankcheck <- 1:input$tierSelect
                              schoolurls <-
                                  schoolurls[schoolurls$rank %in% rankcheck, ]
                          }
                          schoolurls$X1 <-
                              as.character(schoolurls$X1)
                          schoolurls$X2 <-
                              as.character(schoolurls$X2)
                          rm(rankcheck)
                          rm(namecheck)
                          return(schoolurls)
                      })
    
    observeEvent(input$scrapeButton, {
        updateRadioButtons(session, "dataselector",
                           selected = "b")
    })
    
    output$saveMerged <- downloadHandler(
        filename = function() {
            paste('LSDEdata-',
                  Sys.Date(),
                  '.csv',
                  sep = '')
        },
        content = function(con) {
            dfsave <- query_result()
            readr::write_csv(dfsave[order(dfsave$II, na.last = TRUE), ], con , col_names = TRUE)
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
                     value = 0,
                     {
                         shinyjs::disable("scrapeButton")
                         for (i in seq_along(schoolurls$X1)) {
                             url <- schoolurls$X1[i]
                             
                             page1 <-
                                 Rcrawler::LinkExtractor(url = url, Browser = br, )
                             page1 <-
                                 page1[["Info"]][["Source_page"]]
                             page1 <- gsub('"', " ", page1)
                             page1 <-
                                 stringr::str_split(page1, "<a href= /users/creep/")
                             page1 <- unlist(page1)
                             page1 <- cbind(page1)
                             
                             xy <- data.frame(page1)
                             xy <-
                                 xy[!grepl("DOCTYPE", xy$page1),]
                             xy <- data.frame(xy)
                             xy$xy <- as.character(xy$xy)
                             xy <-
                                 stringr::str_split(xy$xy, "',")
                             xy <- unlist(xy)
                             xy <- cbind(xy)
                             xy <- data.frame(xy)
                             xy$xy <- as.character(xy$xy)
                             xy$xy <- gsub('"', "", xy$xy)
                             xy$xy <- gsub("'", "", xy$xy)
                             xy$xy <- trimws(xy$xy)
                             test <- c("[", "]", "<td>")
                             xy <- xy[xy$xy %!in% test,]
                             xy <- data.frame(xy)
                             xy$xy <- as.character(xy$xy)
                             df3 <- grep("order", xy$xy)
                             cut <- as.numeric(df3[1])
                             cut <- cut - 1
                             df.c <- xy[1:cut,]
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
                             if (i %% 2 == 0) {
                                 gc()
                                 
                             }
                             tval <- length(schoolurls$X1)
                             incProgress(
                                 amount = 1 / tval,
                                 detail = paste(
                                     'Get a coffee. Gathering data may take some time...',
                                     "Completed:",
                                     i,
                                     "/",
                                     tval
                                 )
                             )
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
                         dat_csv$X1 <-
                             gsub(">.*", "", dat_csv$X1)
                         dat_csv$X1 <- trimws(dat_csv$X1)
                         dat_csv$X5 <-
                             gsub("\\+|<|>|=", " ", dat_csv$X5)
                         dat_csv$X5 <- substring(dat_csv$X5, 51)
                         dat_csv$X5 <-
                             ifelse(dat_csv$X5 == "", 0, 1)
                         dat_csv$X5 <- trimws(dat_csv$X5)
                         dat_csv$X7 <-
                             gsub("\\$|,", "", dat_csv$X7)
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
                         dat_csv$LSAT <-
                             as.integer(dat_csv$LSAT)
                         dat_csv$Money <-
                             as.integer(dat_csv$Money)
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
                         dat_csv$Result <-
                             as.factor(dat_csv$Result)
                         dat_csv$WE <- as.factor(dat_csv$WE)
                         dat_csv$Cycle <-
                             as.factor(dat_csv$Cycle)
                         shinyjs::enable("scrapeButton")
                         return(dat_csv)
                     })
    })
    query_result <- reactive({
        # if (init.count() == 1) {
        #     return(initdata)
        # }
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
    
    dfLoad <- eventReactive(input$loadButton, {
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
        dat_csv$Received <-
            as.Date(dat_csv$Received, "%Y/%m/%d")
        dat_csv$Complete <-
            as.Date(dat_csv$Complete, "%Y/%m/%d")
        dat_csv$UR <- as.Date(dat_csv$UR, "%Y/%m/%d")
        dat_csv$II <- as.Date(dat_csv$II, "%Y/%m/%d")
        dat_csv$Decision <-
            as.Date(dat_csv$Decision, "%Y/%m/%d")
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
    
    output$temptable3 <-
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
        temp <- temp[!is.na(temp$Decision),]
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
        if (!is.null(query_result()) &
            input$predcurve1 == 1 | input$predcurve2 == 1) {
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
    output$preds_points <- renderUI({
        if (!is.null(query_result()) &
            input$predcurve1 == 1 | input$predcurve2 == 1) {
            radioButtons(
                inputId = "show_points",
                label = "Show or Hide Plot Points",
                choices = c("Show" = 1, "Hide" = 0),
                selected = 0,
                inline =  TRUE
            )
        }
    })
    output$notice <- renderUI({
        if (!is.null(query_result()) &
            input$predcurve1 == 1 | input$predcurve2 == 1) {
            p(
                em(
                    "These graphs are for illustrative purposes. They plot the binary logistic regression used to predict your chance of admission. The center graph for each
                 set is your GPA or LSAT input. You may follow the regression curve to see how you chance of admission changes with a static LSAT or GPA paired with a variable
                 GPA or LSAT. This may be achieved more explicitly by adjusting your GPA or LSAT inputs and observing how your chances change in the table below. The points
                 shown in the plots are also illustrative, merely providing you with the observations from which the regression was built."
                )
            )
        }
    })
    output$side2 <- renderUI({
        if (!is.null(query_result()) && input$resultbin == "No") {
            r.choices1 <-
                plyr::ddply(
                    unique(query_result()[, c("Result", "Cycle")]),
                    plyr::.(Result),
                    plyr::summarize,
                    Cycle = toString(Cycle)
                )
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
    
    output$months <- renderUI({
        if (!is.null(query_result()) && input$sent_yn2 == 1) {
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
                
            )
        }
    })
    
    output$sch_in <- renderUI({
        if (input$tierSelect == 225) {
            r.choices1 <- schools2()
            r.choices1$rank <- as.numeric(r.choices1$rank)
            r.choices1 <-
                r.choices1[order(r.choices1$rank, na.last = TRUE),]
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
                            df.temp$Complete <= input$complete_d[2],]
            df.temp <- df.temp[is.na(df.temp$User) == FALSE,]
        }
        if (input$plots_urm == 1 | input$plots_urm == 0) {
            df.temp <- df.temp[df.temp$URM == input$plots_urm, ]
        }
        df.temp <- df.temp[df.temp$School %in% input$school_in,]
        df.temp <- df.temp[is.na(df.temp$User) == FALSE,]
        df.temp <- df.temp[is.na(df.temp$LSAT) == FALSE,]
        df.temp <- df.temp[df.temp$Cycle %in% input$cycleid,]
        df.temp <- df.temp[is.na(df.temp$User) == FALSE,]
        df.temp <- df.temp[is.na(df.temp$LSAT) == FALSE,]
        
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
            df.temp$Result[df.temp$Result %in% rlist] <-
                "Rejected"
            df.temp$Result[df.temp$Result %in% wlist] <-
                "Waitlisted"
            df.temp <- df.temp[df.temp$Result %in% rlist2,]
        } else{
            df.temp <- df.temp[df.temp$Result %in% input$result_in,]
        }
        
        return(df.temp)
    })
    schoollist <- reactive({
        input$school_in
    })
    
    plot4 <- reactive({
        df.temp2 <- dfPlot()
        df.temp2 <- df.temp2[is.na(df.temp2$User) == FALSE,]
        df.temp2 <- df.temp2[is.na(df.temp2$GPA) == FALSE,]
        df.temp2 <- df.temp2[is.na(df.temp2$LSAT) == FALSE,]
        
        df.temp2$Result <- as.factor(df.temp2$Result)
        cols <- c(gg_color_hue(3)[1:2], "khaki")
        
        cr <- gg_color_hue(3)[1]
        ca <- gg_color_hue(3)[2]
        cw <- "khaki"
        cols2 <- c(cr, ca, cw)
        
        
        # p1 <- ggplot2::ggplot(df.temp2, ggplot2::aes(x = LSAT,y = GPA,
        #                                              fill = Result)) +
        #     ggplot2::geom_bar(colour = "black") +
        # ggplot2::facet_grid(. ~ School, scales = "free_y") +
        #     ggplot2::scale_x_discrete(drop = FALSE) +
        #     stat_stack_labels(color = "black", size = 3.5) +
        
        
        gmin <- min(df.temp2$GPA, na.rm = TRUE)
        gmax <- max(df.temp2$GPA, na.rm = TRUE)
        b1 <- seq(gmin, gmax, 0.05)
        df.temp2$Money <- as.numeric(df.temp2$Money)
        df.temp2$LSAT <- as.factor(df.temp2$LSAT)
        posn.j <-
            ggplot2::position_jitter(width = 0.2, height = 0.05)
        if (input$resultbin == "Yes") {
            p1 <- ggplot2::ggplot(df.temp2, ggplot2::aes(x = LSAT,
                                                         y = GPA)) +
                ggplot2::geom_point(
                    ggplot2::aes(
                        fill = Result,
                        text = purrr::map(
                            paste(
                                '</br>User:',
                                User,
                                '</br>LSAT:',
                                LSAT,
                                '</br>GPA:',
                                GPA,
                                '</br>Money:',
                                Money,
                                '</br>URM:',
                                URM
                            ),
                            HTML
                        )
                    ),
                    color = "black",
                    size = 2.1,
                    alpha = 0.6
                ) +
                ggplot2::scale_y_continuous("GPA",
                                            limits = c(gmin, gmax),
                                            breaks = b1) +
                ggplot2::scale_x_discrete("LSAT") +
                ggplot2::scale_fill_manual(values = c(
                    "Accepted" = ca,
                    "Rejected" = cr,
                    "Waitlisted" = cw
                )) +
                ggplot2::facet_grid(. ~ School) +
                ggplot2::theme_bw()
            return(plotly::ggplotly(p1, tooltip = 'text'))
        } else{
            p1 <- ggplot2::ggplot(df.temp2, ggplot2::aes(x = LSAT,
                                                         y = GPA)) +
                ggplot2::geom_point(
                    ggplot2::aes(
                        fill = Result,
                        text = purrr::map(
                            paste(
                                '</br>User:',
                                User,
                                '</br>LSAT:',
                                LSAT,
                                '</br>GPA:',
                                GPA,
                                '</br>Money:',
                                Money,
                                '</br>URM:',
                                URM
                            ),
                            HTML
                        )
                    ),
                    color = "black",
                    size = 2.1,
                    alpha = 0.6
                ) +
                ggplot2::scale_y_continuous("GPA",
                                            limits = c(gmin, gmax),
                                            breaks = b1) +
                ggplot2::scale_x_discrete("LSAT") +
                ggplot2::facet_grid(. ~ School) +
                ggplot2::theme_bw()
        }
    })
    
    output$user_graph <- plotly::renderPlotly({
        plot4()
    })
    
    plot3 <- reactive({
        df.temp2 <- dfPlot()
        df.temp2 <-
            df.temp2[df.temp2$LSAT >= input$lsat_in[1] &
                         df.temp2$LSAT <= input$lsat_in[2],]
        df.temp2 <- df.temp2[is.na(df.temp2$User) == FALSE,]
        df.temp2 <-
            df.temp2[df.temp2$GPA >= input$gpa_in[1] &
                         df.temp2$GPA <= input$gpa_in[2],]
        df.temp2 <- df.temp2[is.na(df.temp2$User) == FALSE,]
        
        df.temp2$LSAT <- as.factor(df.temp2$LSAT)
        df.temp2$Result <- as.factor(df.temp2$Result)
        cols <- c(gg_color_hue(3)[1:2], "khaki")
        
        cr <- gg_color_hue(3)[1]
        ca <- gg_color_hue(3)[2]
        cw <- "khaki"
        
        cols2 <- c(cr, ca, cw)
        if (input$resultbin == "Yes") {
            validate(need(length(df.temp2$School) > 0, "Please wait"))
            
            p1 <- ggplot2::ggplot(df.temp2, ggplot2::aes(x = LSAT,
                                                         fill = Result)) +
                ggplot2::geom_bar(colour = "black") +
                ggplot2::facet_grid(. ~ School, scales = "free_y") +
                ggplot2::scale_x_discrete(drop = FALSE) +
                stat_stack_labels(color = "black", size = 3.5) +
                ggplot2::scale_fill_manual(values = c(
                    "Accepted" = ca,
                    "Rejected" = cr,
                    "Waitlisted" = cw
                ))
            
            p2 <-
                ggplot2::ggplot(df.temp2, ggplot2::aes(x = LSAT,
                                                       fill = Result)) +
                ggplot2::geom_bar(position = "fill") +
                ggplot2::facet_grid(. ~ School) +
                ggplot2::scale_x_discrete(drop = FALSE) +
                scale_y_continuous("Proportion", labels = scales::percent) +
                stat_fill_labels(size = 3.5) +
                ggplot2::scale_fill_manual(values = c(
                    "Accepted" = ca,
                    "Rejected" = cr,
                    "Waitlisted" = cw
                ))
            
        } else{
            p1 <- ggplot2::ggplot(df.temp2,
                                  ggplot2::aes(
                                      x = LSAT,
                                      fill = Result,
                                      color = Result
                                  )) +
                ggplot2::geom_bar(colour = "black") +
                ggplot2::facet_grid(. ~ School, scales = "free_y") +
                ggplot2::scale_x_discrete(drop = FALSE) +
                stat_stack_labels(color = "black", size = 3.5)
            
            p2 <-
                ggplot2::ggplot(df.temp2, ggplot2::aes(x = LSAT,
                                                       fill = Result)) +
                ggplot2::geom_bar(position = "fill") +
                ggplot2::facet_grid(. ~ School) +
                ggplot2::scale_x_discrete(drop = FALSE) +
                scale_y_continuous("Proportion", labels = scales::percent) +
                stat_fill_labels(size = 3.5)
        }
        
        if (input$bar_type == "Stack") {
            return(plotly::ggplotly(p1, tooltip = c("x", "y", "n", "fill")))
        }
        if (input$bar_type == "Fill") {
            return(plotly::ggplotly(p2))
        }
        
    })
    output$hist <- plotly::renderPlotly({
        plot3()
    })
    output$waves <- plotly::renderPlotly({
        df1 <- dfPlot()
        
        df1$Decision <- as.Date(df1$Decision)
        df1 <- df1[df1$Decision >= as.Date("2017-1-1"),]
        df1 <- df1[is.na(df1$User) == FALSE,]
        df1$Result <- as.factor(df1$Result)
        
        cols <- c(gg_color_hue(3)[1:2], "khaki")
        
        cr <- gg_color_hue(3)[1]
        ca <- gg_color_hue(3)[2]
        cw <- "khaki"
        
        cols2 <- c(cr, ca, cw)
        if (input$resultbin == "Yes") {
            validate(need(length(df1$School) > 0, "Please wait"))
            
            p1 <- ggplot2::ggplot(data = df1,
                                  ggplot2::aes(
                                      x = Decision,
                                      y = Result,
                                      label = as.Date(Decision)
                                  )) +
                ggplot2::geom_count(ggplot2::aes(fill = Result),
                                    color = "black",
                                    alpha = 0.7) +
                ggplot2::facet_grid(df1$School ~ .) +
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
            p1 <- ggplot2::ggplot(data = df1,
                                  ggplot2::aes(
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
        df_t <- subset(df_t, is.na(df_t$LSAT) == FALSE)
        df_t <- subset(df_t, is.na(df_t$pr) == FALSE)
        model <-
            stats::glm(data = df_t,
                       pr ~ GPA + LSAT,
                       family = stats::binomial(link = logit))
        nd2 <-
            data.frame(LSAT = input$lsat_in2,
                       GPA = seq(
                           from = 2,
                           to = 4.33,
                           by = 0.01
                       ))
        nd2 <-
            rbind(nd2, data.frame(
                LSAT = input$lsat_in2 - 1,
                GPA = seq(
                    from = 2,
                    to = 4.33,
                    by = 0.01
                )
            ))
        nd2 <-
            rbind(nd2, data.frame(
                LSAT = input$lsat_in2 + 1,
                GPA = seq(
                    from = 2,
                    to = 4.33,
                    by = 0.01
                )
            ))
        nd2$prob <- predict(model, newdata = nd2, type = "response")
        v2 <- visreg::visreg(
            model,
            "GPA",
            scale = "response",
            rug = 2,
            by = "LSAT",
            breaks = c(input$lsat_in2 - 1, input$lsat_in2, input$lsat_in2 +
                           1),
            plot = FALSE
        )
        
        nd3 <- data.frame(LSAT = input$lsat_in2-1, GPA = seq(from = max(df_t$GPA)-0.2, to = 4.33, by = 0.01))
        nd3 <- rbind(nd3,data.frame(LSAT = input$lsat_in2, GPA = seq(from = max(df_t$GPA)-0.2, to = 4.33, by = 0.01)))
        nd3 <- rbind(nd3,data.frame(LSAT = input$lsat_in2+1, GPA = seq(from = max(df_t$GPA)-0.2, to = 4.33, by = 0.01)))
        
        ilink <- stats::family(model)$linkinv
        nd3 <-
            cbind(nd3,
                  stats::predict(model, nd3, type = "link", se.fit = TRUE)[1:2])
        nd3 <-
            transform(
                nd3,
                Fitted = ilink(fit),
                Upper = ilink(fit + (2 * se.fit)),
                Lower = ilink(fit - (2 * se.fit))
            )
        
        if (input$show_points == 1) {
            v1 <- visreg::visreg(
                model,
                "GPA",
                scale = "response",
                rug = 2,
                xlab = "GPA",
                ylab = "Pr(Accepted)",
                type = "conditional",
                by = "LSAT",
                breaks = c(
                    input$lsat_in2 - 1,
                    input$lsat_in2,
                    input$lsat_in2 + 1
                ),
                gg = TRUE,
                line.par=list(col="royalblue1")
            ) +
                ggplot2::scale_x_continuous(limits = c(2.5, 4.33),
                                            breaks = c(seq(2.5, 4.3, 0.1))) +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::geom_point(
                    data = v2$res,
                    ggplot2::aes(
                        x = GPA,
                        y = visregRes,
                        fill = factor(visregPos),
                        text = purrr::map(
                            paste(
                                '</br>Probability:',
                                paste(round(visregRes, 3) * 100, "%"),
                                '</br>LSAT:',
                                LSAT,
                                '</br>GPA:',
                                GPA,
                                '</br>Result:',
                                ifelse(
                                    visregPos == TRUE,
                                    paste("Accepted"),
                                    paste("Rejected")
                                )
                            ),
                            HTML
                        )
                    ),
                    color = "black",
                    size = 3,
                    shape = 21,
                    alpha = 0.4
                ) +
                ggplot2::scale_fill_brewer(palette = "Set2", direction = -1) +
                ggplot2::theme(legend.position = "none") +
                ggplot2::geom_point(
                    data = nd2,
                    ggplot2::aes(
                        x = GPA,
                        y = prob,
                        text = purrr::map(
                            paste(
                                '</br>GPA:',
                                GPA,
                                '</br>Probability:',
                                paste(round(prob, 3) * 100, "%")
                            ),
                            HTML
                        )
                    ),
                    alpha = 0,
                    shape = 21
                )+
                ggplot2::geom_ribbon(data = nd3,aes(x=GPA,ymin=Lower,ymax=Upper,y=Fitted),color = "grey85", fill ="grey85")+
                ggplot2::geom_smooth(data = nd3,ggplot2::aes(x=GPA,y=Fitted),color = "royalblue1")
            
            v1 <- gginnards::move_layers(v1,"GeomSmooth",position = "bottom")
            v1 <- gginnards::move_layers(v1,"GeomRibbon",position = "bottom")
            
            
            plotly::ggplotly(v1 + ggplot2::ggtitle(
                paste(school_name, " - ", "Pr(Accepted) by LSAT by GPA", sep = "")
            ),
            tooltip = c("text"))
        } else{
            v1 <- visreg::visreg(
                model,
                "GPA",
                scale = "response",
                rug = 2,
                xlab = "GPA",
                ylab = "Pr(Accepted)",
                type = "conditional",
                by = "LSAT",
                breaks = c(
                    input$lsat_in2 - 1,
                    input$lsat_in2,
                    input$lsat_in2 + 1
                ),
                gg = TRUE,
                line.par=list(col="royalblue1")
            ) +
                ggplot2::scale_x_continuous(limits = c(2.5, 4.33),
                                            breaks = c(seq(2.5, 4.3, 0.1))) +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::geom_point(
                    data = nd2,
                    ggplot2::aes(
                        x = GPA,
                        y = prob,
                        text = purrr::map(
                            paste(
                                '</br>GPA:',
                                GPA,
                                '</br>Probability:',
                                paste(round(prob, 3) * 100, "%")
                            ),
                            HTML
                        )
                    ),
                    alpha = 0,
                    shape = 21
                )+
                ggplot2::theme(legend.position = "none") +
                ggplot2::geom_ribbon(data = nd3,aes(x=GPA,ymin=Lower,ymax=Upper,y=Fitted),color = "grey85", fill ="grey85")+
                ggplot2::geom_smooth(data = nd3,ggplot2::aes(x=GPA,y=Fitted),color = "royalblue1")
            
            v1 <- gginnards::move_layers(v1,"GeomSmooth",position = "bottom")
            v1 <- gginnards::move_layers(v1,"GeomRibbon",position = "bottom")

            
            return(plotly::ggplotly(v1 + ggplot2::ggtitle(
                paste(school_name, " - ", "Pr(Accepted) by GPA by LSAT", sep = "")
            ), tooltip = "text"))
        }
    })
    
    output$predplot11 <- renderUI({
        if (input$predcurve1 == 1) {
            plotly::plotlyOutput("predplot1")
        }
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
        df_t <- subset(df_t, is.na(df_t$LSAT) == FALSE)
        df_t <- subset(df_t, is.na(df_t$pr) == FALSE)
        model <-
            stats::glm(data = df_t,
                       pr ~ GPA + LSAT,
                       family = stats::binomial(link = logit))
        
        
        nd1 <-
            data.frame(GPA = input$gpa_in2,
                       LSAT = seq(
                           from = 150,
                           to = 180,
                           by = 1
                       ))
        nd1 <-
            rbind(nd1, data.frame(
                GPA = input$gpa_in2 - 0.05,
                LSAT = seq(
                    from = 150,
                    to = 180,
                    by = 1
                )
            ))
        nd1 <-
            rbind(nd1, data.frame(
                GPA = input$gpa_in2 + 0.05,
                LSAT = seq(
                    from = 150,
                    to = 180,
                    by = 1
                )
            ))
        
        nd1$prob <- predict(model, newdata = nd1, type = "response")
        
        nd3 <- data.frame(GPA = input$gpa_in2 - 0.05, LSAT = seq(from = max(df_t$LSAT)-1, to = 180, by = 1))
        nd3 <- rbind(nd3,data.frame(GPA = input$gpa_in2, LSAT = seq(from = max(df_t$LSAT)-1, to = 180, by = 1)))
        nd3 <- rbind(nd3,data.frame(GPA = input$gpa_in2 + 0.05, LSAT = seq(from = max(df_t$LSAT)-1, to = 180, by = 1)))
        
        ilink <- stats::family(model)$linkinv
        nd3 <-
            cbind(nd3,
                  stats::predict(model, nd3, type = "link", se.fit = TRUE)[1:2])
        nd3 <-
            transform(
                nd3,
                Fitted = ilink(fit),
                Upper = ilink(fit + (2 * se.fit)),
                Lower = ilink(fit - (2 * se.fit))
            )
        
        
        v2 <- visreg::visreg(
            model,
            "LSAT",
            scale = "response",
            rug = 2,
            by = "GPA",
            breaks = c(
                input$gpa_in2 - 0.05,
                input$gpa_in2,
                input$gpa_in2 + 0.05
            ),
            plot = FALSE
        )
        
        if (input$show_points == 1) {
            v1 <- visreg::visreg(
                model,
                "LSAT",
                scale = "response",
                rug = 2,
                xlab = "LSAT",
                ylab = "Pr(Accepted)",
                type = "conditional",
                by = "GPA",
                breaks = c(
                    input$gpa_in2 - 0.05,
                    input$gpa_in2,
                    input$gpa_in2 + 0.05
                ),
                gg = TRUE,
                line.par=list(col="royalblue1")
            ) +
                ggplot2::scale_x_continuous(limits = c(150, 180),
                                            breaks = c(150:180)) +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::geom_point(
                    data = v2$res,
                    ggplot2::aes(
                        x = LSAT,
                        y = visregRes,
                        fill = factor(visregPos),
                        text = purrr::map(
                            paste(
                                '</br>Probability:',
                                paste(round(visregRes, 3) * 100, "%"),
                                '</br>LSAT:',
                                LSAT,
                                '</br>GPA:',
                                GPA,
                                '</br>Result:',
                                ifelse(
                                    visregPos == TRUE,
                                    paste("Accepted"),
                                    paste("Rejected")
                                )
                            ),
                            HTML
                        )
                    ),
                    color = "black",
                    size = 3,
                    shape = 21,
                    alpha = 0.4
                ) +
                ggplot2::scale_fill_brewer(palette = "Set2", direction = -1) +
                ggplot2::geom_point(
                    data = nd1,
                    ggplot2::aes(
                        x = LSAT,
                        y = prob,
                        text = purrr::map(
                            paste(
                                '</br>LSAT:',
                                LSAT,
                                '</br>Probability:',
                                paste(round(prob, 3) * 100, "%")
                            ),
                            HTML
                        )
                    ),
                    alpha = 0,
                    shape = 21
                ) +
                ggplot2::theme(legend.position = "none")+
                ggplot2::geom_ribbon(data = nd3,aes(x=LSAT,ymin=Lower,ymax=Upper,y=Fitted),color = "grey85", fill ="grey85")+
                ggplot2::geom_smooth(data = nd3,ggplot2::aes(x=LSAT,y=Fitted),color = "royalblue1")
            
            v1 <- gginnards::move_layers(v1,"GeomSmooth",position = "bottom")
            v1 <- gginnards::move_layers(v1,"GeomRibbon",position = "bottom")
            
            plotly::ggplotly(v1 + ggplot2::ggtitle(
                paste(school_name, " - ", "Pr(Accepted) by GPA by LSAT", sep = "")
            ),
            tooltip = c("text"))
        } else{
            v1 <- visreg::visreg(
                model,
                "LSAT",
                scale = "response",
                rug = 2,
                xlab = "LSAT",
                ylab = "Pr(Accepted)",
                type = "conditional",
                by = "GPA",
                breaks = c(
                    input$gpa_in2 - 0.05,
                    input$gpa_in2,
                    input$gpa_in2 + 0.05
                ),
                gg = TRUE,
                line.par=list(col="royalblue1")
            ) +
                ggplot2::scale_x_continuous(limits = c(150, 180),
                                            breaks = c(150:180)) +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::geom_point(
                    data = nd1,
                    ggplot2::aes(
                        x = LSAT,
                        y = prob,
                        text = purrr::map(
                            paste(
                                '</br>LSAT:',
                                LSAT,
                                '</br>Probability:',
                                paste(round(prob, 3) * 100, "%")
                            ),
                            HTML
                        )
                    ),
                    alpha = 0,
                    shape = 21
                )+
                ggplot2::theme(legend.position = "none") +
                ggplot2::geom_ribbon(data = nd3,aes(x=LSAT,ymin=Lower,ymax=Upper,y=Fitted),color = "grey85", fill ="grey85")+
                ggplot2::geom_smooth(data = nd3,ggplot2::aes(x=LSAT,y=Fitted),color = "royalblue1")
            
            v1 <- gginnards::move_layers(v1,"GeomSmooth",position = "bottom")
            v1 <- gginnards::move_layers(v1,"GeomRibbon",position = "bottom")
            
            return(plotly::ggplotly(v1 + ggplot2::ggtitle(
                paste(school_name, " - ", "Pr(Accepted) by LSAT by GPA", sep = "")
            ), tooltip = "text"))
        }
        
    })
    
    output$predplot22 <- renderUI({
        if (input$predcurve2 == 1) {
            plotly::plotlyOutput("predplot2")
        }
    })
    
    query_result2 <-  reactive({
        df5 <- query_result()
        xy <-
            vector("list", 0) # create an empty list into which values are to be filled
        school_names <- listOutput()[, 1]
        for (i in school_names) {
            df_t <-
                subset(df5, School == i) #changed from df to alldata because I commented out urm filter
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
            df_t <- subset(df_t, URM == input$urm_in)
            if (input$sent_yn2 == 1) {
                df_t <- subset(df_t,
                               lubridate::month(Sent) %in% input$month_in)
            }
            df_t$pr <- ifelse(df_t$Result == "Accepted", 1, 0)
            df_t <- subset(df_t, is.na(df_t$GPA) == FALSE)
            df_t <- subset(df_t, is.na(df_t$LSAT) == FALSE)
            df_t <- subset(df_t, is.na(df_t$pr) == FALSE)
            model <-
                stats::glm(
                    data = df_t,
                    pr ~ GPA + LSAT,
                    family = stats::binomial(link = logit)
                )
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
                df_w <- subset(df_w,
                               lubridate::month(Sent) %in% input$month_in)
            }
            df_w$wpr <-
                ifelse(df_w$Result == "Waitlisted", 1, 0)
            df_w <- subset(df_w, is.na(df_w$GPA) == FALSE)
            df_w <- subset(df_w, is.na(df_w$wpr) == FALSE)
            wmodel <-
                stats::glm(
                    data = df_w,
                    wpr ~ GPA + LSAT,
                    family = stats::binomial(link = logit)
                )
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
        dffr$rank <-
            ranklist$rank[match(dffr$School, ranklist$X2)]
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
        
        olddata <- olddata[olddata$check == "No",]
        olddata$check <- NULL
        return(rbind(olddata, df.temp1))
    })
    
    observeEvent(input$mergeButton, {
        updateRadioButtons(session, "dataselector",
                           selected = "c")
    })
    
    output$schol1 <- plotly::renderPlotly({
        df1 <- query_result()
        posn.j <-
            ggplot2::position_jitter(width = 0.2, height = 0.05)
        df1 <- df1[df1$School %in% input$school1,]
        if (input$schol_urm == 1) {
            df1 <- df1[df1$URM == 1, ]
        }
        df1 <- df1[df1$Money != 0,]
        df1 <- df1[is.na(df1$Money) == FALSE,]
        df1 <- df1[df1$Money >= as.numeric(input$money_val1),]
        df1 <- df1[df1$Money <= as.numeric(input$money_val2),]
        df1 <- df1[is.na(df1$Money) == FALSE,]
        df1 <- df1[is.na(df1$GPA) == FALSE,]
        gmin <- min(df1$GPA, na.rm = TRUE)
        gmax <- max(df1$GPA, na.rm = TRUE)
        b1 <- seq(gmin, gmax, 0.05)
        df1$Money <- as.factor(df1$Money)
        df1$LSAT <- as.factor(df1$LSAT)
        
        p1 <- ggplot2::ggplot(df1, ggplot2::aes(x = LSAT,
                                                y = GPA)) +
            ggplot2::geom_point(
                ggplot2::aes(fill = Money, text = purrr::map(
                    paste(
                        '</br>User:',
                        User,
                        '</br>LSAT:',
                        LSAT,
                        '</br>GPA:',
                        GPA,
                        '</br>Money:',
                        Money,
                        '</br>URM:',
                        URM
                    ),
                    HTML
                )),
                color = "black",
                size = 2.4,
                alpha = 0.7,
                position = posn.j
            ) +
            ggplot2::scale_y_continuous("GPA",
                                        limits = c(gmin, gmax),
                                        breaks = b1) +
            ggplot2::scale_x_discrete("LSAT") +
            ggplot2::ggtitle(paste(df1$School))
        return(plotly::ggplotly(p1, tooltip = 'text'))
    })
    
    output$schol2 <- plotly::renderPlotly({
        df1 <- query_result()
        posn.j <-
            ggplot2::position_jitter(width = 0.2, height = 0.05)
        df1 <- df1[df1$School %in% input$school2,]
        if (input$schol_urm == 1) {
            df1 <- df1[df1$URM == 1, ]
        }
        df1 <- df1[df1$Money != 0,]
        df1 <- df1[is.na(df1$Money) == FALSE,]
        df1 <- df1[df1$Money >= as.numeric(input$money_val1),]
        df1 <- df1[df1$Money <= as.numeric(input$money_val2),]
        df1 <- df1[is.na(df1$Money) == FALSE,]
        df1 <- df1[is.na(df1$GPA) == FALSE,]
        gmin <- min(df1$GPA)
        gmax <- max(df1$GPA)
        b1 <- seq(gmin, gmax, 0.05)
        df1$Money <- as.factor(df1$Money)
        df1$LSAT <- as.factor(df1$LSAT)
        
        ggplot2::scale_y_continuous(limits = c(gmin, gmax))
        
        p1 <- ggplot2::ggplot(df1, ggplot2::aes(x = LSAT,
                                                y = GPA)) +
            ggplot2::geom_point(
                ggplot2::aes(fill = Money, text = purrr::map(
                    paste(
                        '</br>User:',
                        User,
                        '</br>LSAT:',
                        LSAT,
                        '</br>GPA:',
                        GPA,
                        '</br>Money:',
                        Money,
                        '</br>URM:',
                        URM
                    ),
                    HTML
                )),
                color = "black",
                size = 2.4,
                alpha = 0.7,
                position = posn.j
            ) +
            ggplot2::scale_y_continuous("GPA",
                                        limits = c(gmin, gmax),
                                        breaks = b1) +
            ggplot2::scale_x_discrete("LSAT") +
            ggplot2::ggtitle(paste(df1$School))
        return(plotly::ggplotly(p1, tooltip = 'text'))
    })
    
    val2 <- reactiveVal(0)
    
    observeEvent(input$tweetButton, {
        val2(1)
    }, once = TRUE)
    
    output$twitwarn <- renderUI(if (val2() != 1) {
        p(em(
            strong(
                "You will be prompted to sign in to Twitter. The Twitter API cannot be accessed without logging in with an account. Your login information is NOT handled by this application. You sign directly into Twitter via their API login in your browser."
            )
        ))
    })
    
    tweet_df <- eventReactive(input$tweetButton, {
        temp <-
            rtweet::get_timeline(
                user = input$twitUN,
                n = input$num_tweets,
                include_rts = TRUE
            )
        # temp <- rtweet::get_timeline(user = "elonmusk", n = 50, include_rts = TRUE)
        # temp <- temp[,c("created_at", "screen_name","text","reply_to_screen_name","is_retweet","hashtags","mentions_screen_name", "urls_expanded_url")]
        # colnames(temp) <- c("Created", "User Name", "Tweet Text","Reply to User","Is Retweet","Hashtags","User Mentions", "URLs")
        return(temp)
    })
    
    # tweet_table_q <- reactive({
    #     req(tweet_df())
    #     tweet_df() %>%
    #         select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
    #         mutate(
    #             Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
    #             URLs = purrr::map_chr(urls_expanded_url, make_url_html)
    #         )%>%
    #         select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
    # })
    
    output$tweet_table <-
        DT::renderDataTable(DT::datatable(tweet_df()))
    
    output$tweet_time_series <- plotly::renderPlotly({
        temp <- tweet_df()
        temp <- rtweet::ts_plot(temp, "days")
        temp <-
            data.frame(x = temp[["data"]][["time"]], y = temp[["data"]][["n"]])
        p1 <- ggplot2::ggplot(temp, ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_area(fill = "#69b3a2", alpha = 0.5) +
            geom_point(
                shape = 21,
                color = "black",
                fill = "#69b3a2",
                size = 3
            ) +
            ggplot2::geom_line(color = "#69b3a2") +
            ggplot2::ylab("Tweets") +
            ggplot2::xlab("Time") +
            ggplot2::theme_minimal()
        
        # p1 <- plotly::plot_ly(x = temp$x, y = temp$y, type = 'scatter', mode = 'lines', fill = "tozeroy")
        # p1 <- plotly::add_trace(p1, x = temp$x, y = temp$y, type="scatter", mode="markers", fill = "tonexty")
        
        return(p1)
        
        # plotly::ggplotly(p1)
    })
}
# Run the application
shinyApp(ui = ui, server = server)
