#if(!require(geomorph)) {install.packages("remotes")
#  remotes::install_github("geomorphR/geomorph", ref = "Stable", build_vignettes = F)
#}

library(shiny);library(shinyjs); library(shinyWidgets); library(shinydashboard); library(shinythemes) 
library(shinyalert); library(shinyMatrix); library(shinyjqui); library(shinymeta); library(prettycode)
library(geomorph); library(ape); library(stringr); library(stringi); library(RColorBrewer);
library(reactlog); library(StereoMorph); library(shinybusy)

source("support.functions.R")
rm(list = ls())

# defining elements upon startup
data(plethspecies) 
example_mat <- cbind(plethspecies$phy$tip.label, 
                     c(1,3,1,1,3,1,2,2,1),
                     c("A", "A", "B", "A", "B", "B", "B", "A", "A"), 
                     c(0.45, 0.22, 1.43, 3.79, 4.01, 0.95, 0.60, 1.32, 2.03))
colnames(example_mat) <- c("Species", "Discrete Example Data A", "Discrete Example Data B", "Continuous Example Data")
selected_columns <- NULL
semilms_manual_matrix <- matrix("", ncol = 3, nrow = 1)
colnames(semilms_manual_matrix) <- c("Before", "Slide", "After")
symmetry_manual_matrix <- matrix(NA, ncol = 4, nrow = 1)
colnames(symmetry_manual_matrix) <- c("Spec", "Indiv", "Rep", "Side")
color.ramp.options <- rbind(c('yellow', 'red'), c('blue', 'green'), c('pink', 'purple'))
color.palette.options <- c("Dark2", "Accent", "PuBuGn")
newmat <- NULL
old_independent_vars <- NULL
drop_these <- NULL
symmetry_landpairs_manual_matrix <- matrix(NA, ncol = 2, nrow = 1) 
colnames(symmetry_landpairs_manual_matrix) <- c("Side 1", "Side 2")

options(shiny.maxRequestSize = 30*1024^2, # sets file limit size
        shiny.suppressMissingContextError = TRUE#,
        #shiny.error=recover, # test this
        #shiny.fullstacktrace = T, # try this when debugging
        #shiny.sanitize.errors = T, # try this if some error won't go away (can also wrap errors in safeError())
        #shiny.reactlog = TRUE # this allows for the reaction log to be generated (hit command + fn + f3 to see it)
)


ui <- fillPage(
  navbarPage(
    title = "gmShiny v0.1.3",
    id = "navbar",
    theme = shinytheme("flatly"),
    footer = div(style = "position: absolute; bottom:0; padding: 12px; height: 50px; width: 100%;
                        background-color: rgba(44, 62, 80, 0.75); color: #ceecf0;",
                 fluidRow(column(12, align = "left", 
                                 prettyToggle("alert_on_off", 
                                              label_on = "Instructions On", label_off = "Instructions Off",
                                              value = F, shape = "curve", icon_on = icon("check"),icon_off = icon("remove"),
                                              status_on = "info", status_off = "warning",  inline = T))
                 )),
    tabPanel(
      "Data Input",
      add_busy_spinner(spin = "fading-circle",color = "#ceecf0", timeout = 300, height = '40px', width = '40px'),
      fluidRow(class = "toprow",
               column(width = 8), 
               column(width = 2, 
                      align = "right", 
                      conditionalPanel(
                        "output.datasets_dont_match", 
                        actionButton(inputId = "go_pruning", label = "Prune Datasets To Match",  width = '100%', 
                                     style='padding:6px; font-size:85%; background-color: #003366; border-color: #003366;'))),  # this button shows up when the shape, phy, and/or trait files do not contain exactly the same species in the same order. Clicking the button trims all uploaded datasets to just the species/specimen names that match
               conditionalPanel(
                 "output.example_tps_selected || output.file_tps_selected || output.file_phy_selected || output.file_trait_selected", 
                 column(width = 2, 
                        align = "right", 
                        actionButton(inputId = "go_file_reset", label = "Clear All Inputs",  width = '100%', style='padding:6px; font-size:85%'))), # this button tries to reset all the inputs, basically resetting the app (??? not perfect) # icon = icon("cocktail"), class = "btn-warning")),
               conditionalPanel(
                 "!(output.example_tps_selected || output.file_tps_selected ||  output.file_phy_selected || output.file_trait_selected)",
                 column(width = 2, 
                        align = "right", 
                        actionButton(inputId = "go_example", label = "Use Example Plethodon Data", width = '100%', style='padding:6px; font-size:85%'))) # this button activates the plethodon example dataset # icon = icon("dragon"), class = "btn-block btn-success"),
      ), 
      hr(),
      fluidRow(
        column(width = 4, 
               conditionalPanel(
                 "!output.file_tps_selected",
                 fluidRow(
                   column(12, align = 'right',
                          div(style = "padding: 0px; margin-bottom: 0px; font-size: 80%; height: 0px;",
                              radioButtons(inputId = "shape_file_type", NULL, 
                                           choices = c("TPS or NTS" = "TPSorNTS", "StereoMorph"="StereoMorph"),
                                           selected = "TPSorNTS", inline = T)))),
                 div(style = "margin-top: -10px; padding: 0px; margin-bottom: 0px;",
                     conditionalPanel(
                       condition = "input.shape_file_type == 'TPSorNTS'",
                       fileInput(inputId = "file_tps", label = "Choose Shape File", multiple = FALSE,  width = "auto",
                                 accept = c("text/tps",".tps", "text/TPS", ".TPS", "text/nts",".nts"), 
                                 placeholder = "No shape file selected")),
                     conditionalPanel(
                       condition = "input.shape_file_type == 'StereoMorph'",
                       fileInput(inputId = "file_stereomorph", "Choose StereoMorph File(s)", 
                                 accept = c("text/txt",".txt",  "text/shapes", ".shapes"),
                                 multiple = T, width = "auto", 
                                 placeholder = "No shape file selected")))), # upload tps files here
               
               conditionalPanel(
                 condition = "output.file_tps_selected", 
                 fluidRow(column(12, align = "center", h4("Uploaded Shape Data", style = "margin: 20px;"))),
                 hr(),
                 div(style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 255px); position:relative;", 
                     conditionalPanel(
                       condition = "input.shape_file_type == 'TPSorNTS' && output.file_tps_selected", 
                       # this panel shows up once a tps file has been selected
                       fluidRow(
                         column(4, radioButtons(inputId = "raw_lms_already_aligned", "Specimens Are:",
                                                choices = c("Not Yet Aligned" = F, "Already Aligned" = T),
                                                selected = F, width = "100%")),
                         column(4, radioButtons(inputId = "spec_id", label = "Extract ID From:", # option to specify how the specimens are labeled
                                                choices = c("ID line" = "ID", "IMAGE line" = "imageID", "Assign New Names" = "None"),
                                                selected = "ID", width = "100%")),
                         column(4, radioButtons(inputId = "neg_lms", "Negative LMs Are:", # option to indicate whether negative LMs should be taken as missing data or as real negative values
                                                choices = c("True LMs" = F, "Missing Data" = T), 
                                                selected = F, width = "100%"))),  hr()),
                     conditionalPanel(
                       condition = "input.shape_file_type == 'StereoMorph' && !output.stereomorph_curvetotal_n == 0",  
                       div(style = "padding: 0px; margin: 0px;",
                           fluidRow(
                             column(9, h5(strong("Number of Curve Points"))),
                             column(3, actionButton("go_run_stereomorph_curves", "Apply", width = '100%', 
                                                    style='padding:6px; font-size:85%; background-color: #003366; border-color: #003366;'))
                           ),
                           fluidRow(
                             column(2, align = 'center', numericInput(inputId = "stereomorph_curve1_n", HTML("<span style='font-size: 80%'>Curve 1</span>"), value = 3, min = 0, max = 100, step = 1, width = '100%')),
                             conditionalPanel(
                               "output.stereomorph_curvetotal_n > 1",
                               column(2, align = 'center', numericInput(inputId = "stereomorph_curve2_n", HTML("<span style='font-size: 80%'>Curve 2</span>"), value = 3, min = 0, max = 100, step = 1, width = '100%')),
                             ),
                             conditionalPanel(
                               "output.stereomorph_curvetotal_n > 2",
                               column(2, align = 'center', numericInput(inputId = "stereomorph_curve3_n", HTML("<span style='font-size: 80%'>Curve 3</span>"), value = 3, min = 0, max = 100, step = 1, width = '100%'))
                             ), 
                             conditionalPanel(
                               "output.stereomorph_curvetotal_n > 3",
                               column(2, align = 'center', numericInput(inputId = "stereomorph_curve4_n", HTML("<span style='font-size: 80%'>Curve 4</span>"), value = 3, min = 0, max = 100, step = 1, width = '100%'))
                             ),
                             conditionalPanel(
                               "output.stereomorph_curvetotal_n > 4",
                               column(2, align = 'center', numericInput(inputId = "stereomorph_curve5_n", HTML("<span style='font-size: 80%'>Curve 5</span>"), value = 3, min = 0, max = 100, step = 1, width = '100%'))
                             ),
                             conditionalPanel(
                               "output.stereomorph_curvetotal_n > 5",
                               column(2, align = 'center', numericInput(inputId = "stereomorph_curve6_n", HTML("<span style='font-size: 80%'>Curve 6</span>"), value = 3, min = 0, max = 100, step = 1, width = '100%'))
                             ))
                       ),  hr() ),
                     
                     verbatimTextOutput("shape_file"), br()))), # this displays a preview of the input tps file, mostly for spot checking and general confirmation that the intended file was uploaded
        column(width = 4, 
               conditionalPanel(
                 "!output.file_phy_selected",
                 fileInput(inputId = "file_phy", label = "Choose Phylogeny File", multiple = FALSE, width = "auto", # input phylogeny file. at the moment, only .tre files have been tested or coded for.
                           accept = c("text/tre",".tre", ".nexus", ".nex", "text/nex"), placeholder = "No tree file selected")
               ),
               conditionalPanel(
                 condition = "output.file_phy_selected",
                 fluidRow(column(12, align = "center", h4("Uploaded Phylogeny", style = "margin: 20px;"))),
                 hr(),
                 div(style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 255px); position:relative;",
                     checkboxInput(inputId = "show_tip_label_phy_preview", label = "Display Tip Labels", TRUE),
                     plotOutput(height = "auto", "phylogeny")))), #this displays the phylogeny uploaded, again for spot checking.
        column(width = 4,
               conditionalPanel(
                 "!output.file_trait_selected",
                 fileInput(inputId = "file_trait", label = "Choose Trait File", multiple = FALSE, width = "auto", # input trait file. at the moment, only .csv files are allowed
                           accept = c("text/csv",".csv", "text/xls", ".xls", "text/xlsx", ".xlsx"), 
                           placeholder = "No trait file selected")
               ),
               conditionalPanel(
                 condition = "output.file_trait_selected", 
                 fluidRow(column(12, align = "center", h4("Uploaded Trait Data", style = "margin: 20px;"))),
                 hr(),
                 #hr(style = "margin-top: 10px; margin-bottom: 10px; padding: 0px;"),
                 div(style = "overflow: hidden; overflow-y:scroll; max-height: calc(100vh - 255px); position:relative; overflow-x: hidden;", 
                     fluidRow(
                       column(
                         3, div(style = "margin-bottom: 0px;",h5(strong("Select Column(s):")))),
                       column(
                         9, div(style = "margin-top: 20px; margin-bottom: 0px;", 
                                checkboxGroupInput(inputId = "trait_column", label = NULL, inline = F, choices = 2, selected = 2))
                       )),
                     conditionalPanel(
                       "output.any_traits_selected",
                       hr(style = "margin-top: 10px; margin-bottom: 10px; padding: 0px;"),
                       fluidRow(
                         column(3, offset = 3,  div(style = "margin: 0px; padding: 0px;", textOutput("trait_1_name", container = h5))), 
                         column(3, div(style = "margin: 0px; padding: 0px;", textOutput("trait_2_name", container = h5))), 
                         column(3, div(style = "margin: 0px; padding: 0px;", textOutput("trait_3_name", container = h5)))
                       ),
                       fluidRow(
                         column(
                           3,
                           div(style = "margin: 0px; padding: 0px;", h5(strong("Data Type:")),
                               conditionalPanel(
                                 "input.trait_1_treatment == 'cont' || (output.two_traits_selected && input.trait_2_treatment == 'cont') || (output.three_traits_selected && input.trait_3_treatment == 'cont')",
                                 br(), br(), h5(strong("Transformation:"))))),
                         column(3,
                                div(style = "margin: 0px; padding: 0px;",
                                    radioButtons(inputId = "trait_1_treatment", label = NULL,
                                                 choices = c("Discrete" = "disc", "Continuous" = "cont"), selected = "cont", inline = F), br(),
                                    conditionalPanel(
                                      "input.trait_1_treatment == 'cont'",
                                      radioButtons(inputId = "trait_1_transformation", label = NULL, 
                                                   choices = c("None" = "raw", "Log" = "log", "Square Root" = "sqrt"), 
                                                   selected = "raw", inline = F)))),
                         conditionalPanel(
                           condition = "output.two_traits_selected",
                           column(3,
                                  div(style = "margin: 0px; padding: 0px;",
                                      radioButtons(inputId = "trait_2_treatment", label = NULL,
                                                   choices = c("Discrete" = "disc", "Continuous" = "cont"), selected = "cont", inline = F), br(),
                                      conditionalPanel(
                                        "output.two_traits_selected && input.trait_2_treatment == 'cont'",
                                        radioButtons(inputId = "trait_2_transformation", label = NULL, 
                                                     choices = c("None" = "raw", "Log" = "log", "Square Root" = "sqrt"), 
                                                     selected = "raw", inline = F)))),  
                           conditionalPanel(
                             condition = "output.three_traits_selected", 
                             column(3,
                                    div(style = "margin: 0px; padding: 0px;",
                                        radioButtons(inputId = "trait_3_treatment", label = NULL,
                                                     choices = c("Discrete" = "disc", "Continuous" = "cont"), selected = "cont", inline = F), br(),
                                        conditionalPanel(
                                          "output.three_traits_selected && input.trait_3_treatment == 'cont'",
                                          radioButtons(inputId = "trait_3_transformation", label = NULL, 
                                                       choices = c("None" = "raw", "Log" = "log", "Square Root" = "sqrt"), 
                                                       selected = "raw", inline = F))))))
                       ),
                       hr(style = "margin-top: 0px; margin-bottom: 10px; padding: 0px;"),
                       tableOutput("trait_table")), br()) 
               )))), # this displays the trait file and columns selected. also allows for searches/sorting through the trait file.
    tabPanel(
      title = "Data Prep",
      id = "data_prep",
      add_busy_spinner(spin = "fading-circle",color = "#ceecf0", timeout = 300, height = '40px', width = '40px'),
      tabsetPanel(
        id = "tab_dataprep", 
        tabPanel(
          title = "Define Links and Semi-Landmarks",
          id = "tab_dataprep_definelinksandsemilandmarks",
          sidebarLayout(
            mainPanel(
              width = 9, 
              style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 130px); position:relative;",
              br(),
              wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                        fluidRow(
                          plotOutput("all_specimens", height = 700,
                                     click = "link_click_initiated", dblclick = "link_click_end", brush = "semis_selected")),
                        br(), br(), 
                        fluidRow(
                          column(4, offset = 2, align= "right",
                                 downloadButton("export_plot_all_specimens", label = "Export Plot", 
                                                style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                          column(4, align = "left",
                                 downloadButton("export_plot_all_specimen_code", "Export Code", icon = shiny::icon("registered"),
                                                style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
                        ), 
                        br()
              ), 
              br(), br()), # clicks and double clicks are for drawing the links between landmarks. the brush is for selecting semilandmarks
            sidebarPanel(
              width = 3,  style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 150px); position:relative;",
              hr(style="border-color: purple;"),
              fluidRow(
                align = "center", h4(strong("Settings"))), 
              hr(style="border-color: purple;"),
              fluidRow(
                column(6, align = "center", actionButton("flip_lms_ho", "Flip LMs Horizontally", width = "100%", style='padding:6px; font-size:80%')),
                column(6, align = "center", actionButton("flip_lms_vert", "Flip LMs Vertically", width = "100%", style='padding:6px; font-size:80%'))
              ), 
              conditionalPanel(
                "input.shape_file_type != 'StereoMorph' && output.links_dbclick_initiated",
                hr(),
              ),
              conditionalPanel(
                "input.shape_file_type == 'StereoMorph' && output.stereomorph_curvetotal_n > 0",
                hr(),
                fluidRow(
                  column(12, align = "center",
                         actionButton("semis_stereomorph", "Add Links to Match StereoMorph Curves",
                                      width = 300, style='padding:6px; font-size:80%')))),
              conditionalPanel(
                "output.links_dbclick_initiated && input.shape_file_type == 'StereoMorph' && output.stereomorph_curvetotal_n > 0", br()
              ),
              conditionalPanel(
                condition = "output.links_dbclick_initiated", 
                fluidRow(
                  align = "center",
                  actionButton(inputId = "link_reset", label = "Reset Landmark Links", 
                               width = 200, style='padding:6px; font-size:80%'))),
              hr(),
              fluidRow(
                align = "center",
                column(12,
                       fileInput("semilms_upload_file_input", "Upload Semilandmark Matrix", 
                                 accept = c("text/csv",".csv", "text/xls", ".xls", "text/xlsx", ".xlsx"),
                                 placeholder = "Select a CSV or excel file"))),
              fluidRow(align = "center", h5(strong("Semilandmark Matrix"))),
              fluidRow(
                column(8, offset = 2, 
                       
                       div(style = "font-size:80%; text-align: center;",
                           matrixInput(
                             inputId = "semilms_manual_input", label = NULL,
                             value = semilms_manual_matrix,
                             cols = list(names = T), rows = list(names = F, extend = T))))),
              conditionalPanel(
                "output.semis_initiated",
                fluidRow(align = "center",
                         downloadButton("export_semilm_mat", label = "Export Semilandmark Matrix", 
                                        style='width: 200px; padding:6px; font-size:80%; 
                             background-color: #337ab7; border-color: #337ab7;')
                ),br()),
              conditionalPanel(
                condition = "output.semis_initiated",
                fluidRow(
                  align = "center",
                  actionButton(inputId = "go_semilms_apply", "Apply Semilandmark Matrix", width = 200, 
                               style='padding:6px; font-size:85%; background-color: #003366; border-color: #003366;')
                ),br(),
                fluidRow(
                  align = "center",
                  actionButton(inputId = "semilms_reset", label = "Reset Semilandmark Selection",  
                               width = 200, style='padding:6px; font-size:80%'), br())), # resetting the semi landmark selections ??? does this reset completely and rerun if we selected some sliders then reset them, and did it again
              hr(),
              fluidRow(column(12,h5(strong("Color Options:")))),
              fluidRow(div(style = "margin: 0px; padding: 0px; vertical-align: bottom;",
                           column(2, align = "center", h6("SemiLMs")),
                           column(2, align = "center", h6("Brackets")),
                           column(2, align = "center", h6("Other LMs")),
                           column(2, align = "center", h6("Individ")),
                           column(2, align = "center", h6("Labels")),
                           column(2, align = "center", h6("Links")))
              ),
              fluidRow(div(style = "margin: 0px; padding: 0px; vertical-align: bottom;",
                           column(2, align = "center", colorSelectorDrop.ekb("semilms_color", "Semilandmarks", selected = all_color_options[7])),
                           column(2, align = "center", colorSelectorDrop.ekb("semilms_color_brackets", "Bracketing Landmarks", selected = all_color_options[4])),
                           column(2, align = "center", colorSelectorDrop.ekb("semilms_color_other", "Other Landmarks")),
                           column(2, align = "center", colorSelectorDrop.ekb("semilms_color_individlms", "Individual Specimen Landmarks", selected = all_color_options[58], dropdownside = "right")),
                           column(2, align = "center", colorSelectorDrop.ekb("semilms_color_labels", "Labels", selected = all_color_options[1], dropdownside = "right")),
                           column(2, align = "center", colorSelectorDrop.ekb("semilms_color_links", "Links", dropdownside = "right"))
              )), br(), br()
            ))),
        tabPanel(
          title = "Visualize Outliers and Individual Specimens", 
          id = "tab_dataprep_visualizeoutliersandindividualspecimens",
          sidebarLayout(
            mainPanel(
              id = "scroll_outlier_selected",
              width = 9, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 130px); position:relative;", 
              br(), 
              wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;", 
                        plotOutput("visualize_outliers_all", height = 700, click = "outlier_selected"), # click is for selecting and visualizing each specimen
                        fluidRow(
                          column(
                            4, offset = 2, align = "right", 
                            downloadButton("export_visualize_outliers_all", "Export Plot", 
                                           style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                          column(
                            4, align = "left",
                            downloadButton("export_visualize_outliers_all_code", "Export Code", icon = shiny::icon("registered"),
                                           style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')
                          )), 
                        br()),
              
              conditionalPanel(
                "output.outlier_selected", 
                wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                          
                          plotOutput("outlier_selected_lms", height = 700), 
                          fluidRow(
                            column(
                              4, offset = 2,
                              align = "right",
                              downloadButton("export_outlier_selected_lms", "Export Plot", 
                                             style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                            column(
                              4, align = "left",
                              downloadButton("export_outlier_selected_lms_code", "Export Code", icon = shiny::icon("registered"),
                                             style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
                          ), br())
              ), br(), br()),
            sidebarPanel(
              width = 3, style = "overflow: hidden; overflow-y: scroll; min-height: calc(100vh - 200px); max-height: calc(100vh - 150px); position:relative;",
              hr(style="border-color: purple;"),
              fluidRow(align = "center", h4(strong("Settings"))), 
              hr(style="border-color: purple;"),
              fluidRow(column(12, sliderInput("outlier_plot_txt_cex", "Text Size (Outliers Plot)", min = 0.1, max = 3.1, value = 1, step = .1))),
              fluidRow(column(12, sliderInput("outlier_plot_pt_cex", "Point Size (Outliers Plot)", min = 0.1, max = 3.1, value = 1, step = .1))),
              fluidRow(
                align = "center",
                actionButton("find_mean_spec", "Find Specimen Closest to Mean Shape", 
                             style='width: 67%; padding:6px; font-size:80%;')), br(), 
              checkboxInput("outlier_plot_show_point_names_tf", "Show Names"),
              conditionalPanel(
                "output.one_disc_traits_selected",
                checkboxInput("outlier_group_tf", "Visualize Outliers by Trait Group", value = F)),
              conditionalPanel(
                "input.outlier_group_tf",
                fluidRow(column(6, offset = 1, h5("Trait:")), column(5, h5("Level:"))),
                fluidRow(column(6, offset = 1, radioButtons("outlier_group", NULL, choices = 1:3, selected = 1)),
                         column(5, radioButtons("outlier_group_level_plotted", NULL, choices = 1:3, selected = 1)))),
              br(),
              fluidRow(
                column(8, style = "margin-top: 0px;", selectizeInput("remove_outlier_specimen", "Exclude Specimen from Dataset", choices = c("specimen1", "specimen2"),
                                                                     options = list(placeholder = 'Select or enter specimen', onInitialize = I('function() { this.setValue(""); }')))), 
                column(4, style = "margin-top: 25px;", actionButton("go_remove_outlier_specimen", "Remove", style='padding:6px; font-size:80%; width: 100%'))), 
              conditionalPanel(
                "output.outlier_removed",
                fluidRow(align = "center", "Removed:", textOutput("outlier_removed_names")),br(),
                fluidRow(align = "center", actionButton("go_remove_outlier_specimen_reset", "Undo Removal", 
                                                        style='padding:6px; font-size:80%; width: 33%'))), 
              
              conditionalPanel("output.outlier_selected", 
                               hr(),
                               fluidRow(column(12,h5(strong("Individual Plot Color Options:")))),
                               fluidRow(div(style = "margin: 0px; padding: 0px; vertical-align: bottom;",
                                            column(2, align = "center", h6("SemiLMs")),
                                            column(2, align = "center", h6("Brackets")),
                                            column(2, align = "center", h6("Other LMs")),
                                            column(2, align = "center", h6("Labels")),
                                            column(2, align = "center", h6("Links")))
                               ),
                               fluidRow(div(style = "margin: 0px; padding: 0px; vertical-align: bottom;",
                                            column(2, align = "center", colorSelectorDrop.ekb("vis_outliers_color", "Semilandmarks", selected = all_color_options[7])),
                                            column(2, align = "center", colorSelectorDrop.ekb("vis_outliers_color_brackets", "Bracketing Landmarks", selected = all_color_options[4])),
                                            column(2, align = "center", colorSelectorDrop.ekb("vis_outliers_color_other", "Other Landmarks")),
                                            column(2, align = "center", colorSelectorDrop.ekb("vis_outliers_color_labels", "Labels", selected = all_color_options[1], dropdownside = "right")),
                                            column(2, align = "center", colorSelectorDrop.ekb("vis_outliers_color_links", "Links", dropdownside = "right"))
                               ))), 
              br()
            ))),
        tabPanel(
          "Generalized Procrustes Alignment",
          id = "tab_dataprep_generalizedprocrustesalignment",
          br(), 
          fluidRow(
            column(4, wellPanel(
              style = "align: center; border-color: white; background-color: rgba(255,250,250,.25);
                                overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 200px); position:relative;", 
              h4("Raw Imported LMs"),
              verbatimTextOutput("raw_lms_rx"))),
            column(4, align = "center",
                   wellPanel(
                     style = "align: center; border-color: white; background-color: rgba(255,250,250,.25);
                                overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 200px); position:relative;", 
                     actionButton(inputId = "go_run_gpa", label = "Run GPA",  
                                  style='width: 100%; height: 42px; padding:6px; 
                                             font-size:120%; background-color: #003366; border-color: #003366;'), hr(),
                     conditionalPanel(
                       "output.semis_initiated", 
                       fluidRow(
                         column(
                           12, align = "center",
                           checkboxInput("ProcD", "Use Procrustes distance as optimization criterion of semi-landmarks")
                         ))
                     ),
                     fluidRow(
                       column(
                         12, align = "center",
                         checkboxInput("Proj", "Project Procrustes aligned specimens into tangent space", value = TRUE)
                       )
                     ),
                     hr(),
                     fluidRow(
                       h4("Dropped Specimens"),
                       tableOutput("outlier_removed_names_tab")),
                     br(), hr(),
                     fluidRow(
                       h4("Missing Landmarks to Estimate"),
                       tableOutput("missing_lms_to_estimate")),
                     hr(),
                     fluidRow(
                       h4("Semilandmark Matrix"),
                       tableOutput("semilandmark_matrix")),
                     hr(),
                     fluidRow(align = "center", 
                              downloadButton("export_run_gpa_code", "Export GPA Code", icon = shiny::icon("registered"),
                                             style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'
                              )))),
            column(4,
                   wellPanel(
                     style = "align: center; border-color: white; background-color: rgba(255,250,250,.25);
                                overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 200px); position:relative;", 
                     fluidRow(
                       column(6, align = "left", h4("Aligned LMs"))
                     ),
                     conditionalPanel(
                       condition = "!output.show_gpa", 
                       h5(em("LMs Not Yet Aligned"))), 
                     conditionalPanel(
                       condition = "output.show_gpa", 
                       verbatimTextOutput("gpa_aligned_rx")
                     )) 
            )))
      )), 
    tabPanel(
      "Morphospace and Warp Grids", # why are the conditional panels not working on this tab panel? ???
      add_busy_spinner(spin = "fading-circle",color = "#ceecf0", timeout = 300, height = '40px', width = '40px'),
      sidebarLayout(
        position = "right", 
        sidebarPanel(
          tags$style(".well {border-color: gray; background-color: #f5feff}"),
          width = 3, style = "overflow-y: scroll; max-height: calc(100vh - 130px); position:relative;",
          hr(style="border-color: purple;"),
          fluidRow(align = "center", h4(strong("Settings"))), 
          hr(style="border-color: purple;"),
          conditionalPanel(
            "output.file_phy_selected",
            fluidRow(column(6, div(style = "vertical-align: top; height: 50px;",strong("Alignment:"))),
                     column(6, div(style = "vertical-align: top; height: 50px;",
                                   radioButtons(inputId = "align_to_phy_tf", NULL, 
                                                choices = c("PCA" = FALSE, "PaCA" = TRUE), 
                                                selected = F, inline = T)))),
            fluidRow(column(6, div(style = "vertical-align: top; height: 50px;",strong("Centering:"))),
                     column(6, div(style = "vertical-align: top; height: 50px;",
                                   radioButtons(inputId = "gls_center_tf", NULL, 
                                                choices = c("OLS" = "FALSE", "GLS" = "TRUE"), 
                                                selected = "FALSE", inline = T)))),
            conditionalPanel(
              "input.gls_center_tf == 'TRUE'", 
              fluidRow(column(6, div(style = "vertical-align: top; height: 50px;", strong("Project Transformed Residuals:"))),
                       column(6, div(style = "vertical-align: bottom; height: 50px;", 
                                     radioButtons(inputId = "transform_resid_tf", NULL, 
                                                  choices = c("FALSE" = FALSE, "TRUE" = TRUE), 
                                                  selected = FALSE, inline = T)))), br())),
          fluidRow(align = "center", 
                   downloadButton("export_component_scores", "Export Component Scores", 
                                  style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7;  border-color: #337ab7;')), br(),
          fluidRow(align = "center", 
                   downloadButton("export_rotation_loadings", "Export Rotation Loadings", 
                                  style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7;  border-color: #337ab7;')), br(),
          fluidRow(
            column(
              9,
              selectInput(inputId = "pca_x_axis", label = "X axis:", # selection option for which pc axes to display on the x axis. this is updated based on the data uploaded
                          choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4, "PC5" = 5, "PC6" = 6, "PC7" = 7, 
                                      "PC8" = 8),
                          selected = 1, multiple = F)),
            column(3, align = "center", actionButton(inputId = "flip_x_axis", "Flip", style = 'font-size:80%; margin:20px 0px 0px 0px;'))),
          fluidRow(
            column(
              9,
              selectInput(inputId = "pca_y_axis",  label = "Y axis:", # selection option for which pc axes to display on the y axis. this is updated based on the data uploaded
                          choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3,"PC4" = 4, "PC5" = 5, "PC6" = 6, "PC7" = 7, 
                                      "PC8" = 8),
                          selected = 2, multiple = F)),
            column(3, align = "center", actionButton(inputId = "flip_y_axis", "Flip", style = 'font-size:80%; margin:20px 0px 0px 0px;')
            )) ,
          sliderInput(inputId = "tip_cex", label = "Point Size:", # adjusts size of the points in morphospace
                      value = 1, min = 0, max = 5, step = 0.1),
          selectInput(inputId = "tip_pch", label = "Point Shape:",
                      choices = c("Filled Circle" = "19", "Hollow Circle" = "1", "Filled Diamond" = "18"), # how to color the dots. updated when trait file is uploaded. 
                      selected = "19",
                      multiple = F),
          fluidRow(column(12, div(style = "height: 65px;", selectInput(inputId = "tip_col_category", label = "Point Color:", # how to color the dots. updated when trait file is uploaded.
                                                                       choices = c("All One Color" = "all_1_col"),# "By Trait 1" = "by_trait_1", "By Trait 2" = "by_trait_2","By Trait 3" = "by_trait_3"), 
                                                                       selected = "all_1_col", multiple = F)))),
          conditionalPanel(
            condition = "input.tip_col_category == 'all_1_col'", 
            fluidRow(
              column(9, colorSelectorInput(inputId = "tip_col", label = NULL, selected = "black",
                                           choices = c(brewer.pal(name = "Spectral", n = 10), "black"), ncol = 11,
                                           mode = "radio", display_label = F)),
              column(3, div(style = "margin-top:-5px;", checkboxInput("tip_col_other_tf", "Other"))))), 
          conditionalPanel(condition = "input.tip_col_other_tf", fluidRow( 
            column(12, selectizeInput(inputId = "tip_col_other", label = NULL, choices = colors(), selected = "gray")))),
          conditionalPanel(
            condition = "input.tip_col_category == 'by_trait_1' || input.tip_col_category == 'by_trait_2' || input.tip_col_category == 'by_trait_3' || input.tip_col_category == 'csize' ||
            input.show_convex_hull_1 || input.show_convex_hull_2 || input.show_convex_hull_3", 
            fluidRow(
              column(1, colorSelectorDrop.ekb(inputId = "trait_colors_lev1", label = NULL, selected = all_color_options[1])),
              column(1, colorSelectorDrop.ekb(inputId = "trait_colors_lev2", label = NULL, selected = all_color_options[5])),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 2", colorSelectorDrop.ekb(inputId = "trait_colors_lev3", label = NULL, selected = all_color_options[6]))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 3", colorSelectorDrop.ekb(inputId = "trait_colors_lev4", label = NULL, selected = all_color_options[8]))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 4", colorSelectorDrop.ekb(inputId = "trait_colors_lev5", label = NULL, selected = all_color_options[52]))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 5", colorSelectorDrop.ekb(inputId = "trait_colors_lev6", label = NULL, selected = all_color_options[9]))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 6", colorSelectorDrop.ekb(inputId = "trait_colors_lev7", label = NULL, selected = all_color_options[48], dropdownside = "right"))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 7", colorSelectorDrop.ekb(inputId = "trait_colors_lev8", label = NULL, selected = all_color_options[40], dropdownside = "right"))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 8", colorSelectorDrop.ekb(inputId = "trait_colors_lev9", label = NULL, selected = all_color_options[60], dropdownside = "right"))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 9", colorSelectorDrop.ekb(inputId = "trait_colors_lev10", label = NULL, selected = all_color_options[23], dropdownside = "right"))), 
              column(1, conditionalPanel(condition = "output.col_n_levels  > 10", colorSelectorDrop.ekb(inputId = "trait_colors_lev11", label = NULL, selected = all_color_options[35], dropdownside = "right"))), br(), br())), 
          
          conditionalPanel(condition = "input.show_tip_label", # this option only shows up when the user chooses to display the names of each point
                           sliderInput(inputId = "tip_txt_cex", label = "Point Label Size", # adjusts the size of those name labels
                                       value = 1, min = 0, max = 5, step = 0.1)),
          h5(strong("Show:")), # lots of options for what to show on the morphospace
          checkboxInput("show_tip_label", "Point Labels", FALSE), # option to show or hide tip labels
          conditionalPanel(
            condition = "(input.trait_1_treatment == 'disc' || input.trait_2_treatment == 'disc' || input.trait_3_treatment == 'disc') && output.file_trait_selected", 
            checkboxInput("show_convex_hull_1", "Convex Hulls Around Trait 1", FALSE)), # option of whether to show convex hulls of the discrete trait. Updated when trait file is uploaded to match the trait column name.
          conditionalPanel(
            condition = "output.two_disc_traits_selected && output.file_trait_selected", 
            checkboxInput("show_convex_hull_2", "Convex Hulls Around Trait 2", FALSE)), 
          conditionalPanel(
            condition = "output.three_disc_traits_selected && output.file_trait_selected", 
            checkboxInput("show_convex_hull_3", "Convex Hulls Around Trait 3", FALSE)),
          conditionalPanel(
            condition = "output.example_tps_selected || output.file_phy_selected",  # this option is displayed if a phylogeny is uploaded (or you're using sample data)
            checkboxInput("include_phylo", "Phylogeny", FALSE)), # option to show or hide phylogeny
          conditionalPanel(
            condition = "input.include_phylo",  # this option is displayed if a phylogeny is uploaded (or you're using sample data)
            checkboxInput("show_more_phylo_options", em("More Phylogeny Style Options"), FALSE)), # option to show or hide phylogeny
          conditionalPanel(
            condition = "input.show_more_phylo_options", 
            wellPanel(
              div(style = "margin: 2px; padding: 0px; font-size:85%;", 
                  fluidRow(column(11, offset = .5, p(strong("Colors:")))),
                  fluidRow(column(4, align = "center", h6("Nodes")), 
                           conditionalPanel("input.show_node_label", column(4, align = "center", h6("Node Labels"))), 
                           column(4, align = "center", h6("Edges"))),
                  fluidRow(column(4, align = "center", colorSelectorDrop.ekb(inputId = "node_col", label = " ", selected = "#737373")),
                           conditionalPanel("input.show_node_label", column(4, align = "center", colorSelectorDrop.ekb(inputId = "node_txt_col", label = " ", selected = "#737373"))),
                           column(4, align = "center", colorSelectorDrop.ekb(inputId = "edge_col", dropdownside = "right", label = " "))),br(),
                  fluidRow(column(11, offset = .5, selectInput("node_pch", "Node Shape:", 
                                                               choices = c("Filled Circle" = 19, "Open Circle" = 1,
                                                                           "Filled Square" = 15, "Open Square" = 0, "Filled Triangle" = 17,
                                                                           "Open Triangle" = 2, "X" = 4, "Diamond" = 5)))),
                  fluidRow(column(11, offset = .5, checkboxInput("show_node_label", strong("Show Node Labels"), FALSE))), # display option to show node labels if displaying phylogeny
                  fluidRow(column(11, offset = .5, sliderInput("node_cex", "Node Size:", min = 0, max = 5, step = 0.1, value = 1))),
                  conditionalPanel(
                    "input.show_node_label", fluidRow(column(11, offset = .5, sliderInput("node_txt_cex", "Node Label Size:", min = 0, max = 5, step = 0.1, value =1)))),
                  fluidRow(column(11, offset = .5, sliderInput("edge_width", "Edge Width:", min = 0, max = 5, step = 0.1, value =1)))
              ))),
          conditionalPanel(
            condition = "output.morpho_click_initiated || output.morpho_dbclick_initiated", 
            hr(style="border-color: purple;"),  
            fluidRow(align = "center", h4(strong("Warp Grid Settings"))), hr(style="border-color: purple;"),
            sliderInput(inputId="warp_mag", label = "Warp Magnitude:", value = 1, min = 0, max = 5, step = 0.1), # warp magnitude option (applied to both the big and overlain warp grids)
            radioButtons(inputId="warp_type", label ="Warp Type:", 
                         choices = c("Thin Plate Spline" = "TPS", "Vectors" = "vector", "Points" = "points"),
                         selected = "TPS"),
            radioButtons(inputId="warp_var_displayed", label = "Variation Displayed:", # options to have the warp grid represent the full morphospace or only particular axes of interest
                         choices = c("Full Morphospace" = "full_morphospace", "Particular Axis or Axes" = "selected_axes"),
                         selected = "full_morphospace"),
            conditionalPanel(
              condition = "input.warp_var_displayed == 'selected_axes'", # which axes are available to choose from
              checkboxGroupInput(inputId = "warp_axes_selected", label = "Visualize Variation Across PC:", # these options are updated based on input data dimensions
                                 choices = c(1, 2), selected = c(1,2), inline = T)),
            wellPanel(radioButtons(inputId="warp_comparison_start", label = "Warp Comparison Start (Reference):",  # options to choose which specimens or projected parts of morphospace should be the Ref and Target shapes
                                   choices = c("Mean Shape" = "mean", 
                                               "Observed Point (Clicked)" = "selected_obs",
                                               "Observed Point (By Name)" = "selected_obs_byname"),  # selected is referring to the specimen you clicked on morphospace
                                   selected = "mean"), # updated if you double click to include the option of Selected Projected Point (X)
                      conditionalPanel(
                        "input.warp_comparison_start == 'selected_obs_byname'",
                        selectizeInput(inputId = "warp_specific_ref_specimen", label =  "Select Reference Specimen by Name:", 
                                       choices = list("Specimen 1" = 1))),
                      hr(),
                      radioButtons(inputId="warp_comparison_end", label = "Warp Comparison End (Target):", 
                                   choices = c("Mean Shape" = "mean", "Observed Point (Clicked)" = "selected_obs" , 
                                               "Observed Point (By Name)" = "selected_obs_byname"), 
                                   selected = "selected_obs"),
                      conditionalPanel(
                        "input.warp_comparison_end == 'selected_obs_byname'",
                        selectizeInput(inputId = "warp_specific_targ_specimen", 
                                       label =  "Select Target Specimen by Name:", choices = list("Specimen 1" = 1))),
                      hr(),
                      checkboxInput(inputId="warp_comparison_show_arrow", label = strong("Show Comparison Trajectory"), TRUE)), # option of whether to display an arrow between the Ref and Target parts of morphospace displayed on the warpgrid
            
            checkboxInput(inputId="show_more_warp_options", em("Show More Warp Grid Style Options"), FALSE),
            conditionalPanel(
              condition = "input.show_more_warp_options", 
              wellPanel(
                id = "more_warp_options", 
                div(style = "margin: 2px; padding: 0px; font-size:85%;", 
                    fluidRow(p(strong("Colors"))),
                    fluidRow( 
                      conditionalPanel(
                        condition = "input.warp_type == 'TPS' || input.warp_type == 'points' ",
                        column(width = 4, align = "center", style = "vertical-align: bottom;", p("Target Points:"))),
                      conditionalPanel(
                        "output.links_dbclick_initiated && (input.warp_type == 'TPS' || input.warp_type == 'points')",
                        column(width = 4, align = "center", style = "vertical-align: bottom;", p("Target Links:"))),
                      conditionalPanel(
                        "input.warp_type == 'TPS'",
                        column(width = 4, align = "center", style = "vertical-align: bottom;", p("GridLines:")))),
                    fluidRow(
                      conditionalPanel(
                        condition = "input.warp_type == 'TPS' || input.warp_type == 'points' ",
                        column(width = 4, align = "center", style = "vertical-align: bottom; height: 20px;", 
                               colorSelectorDrop.ekb(inputId = "warp_tar_pt_bg", label = " ", ))),
                      conditionalPanel(
                        "output.links_dbclick_initiated && (input.warp_type == 'TPS' || input.warp_type == 'points')",
                        column(width = 4, align = "center", style = "vertical-align: bottom; height: 20px;", 
                               colorSelectorDrop.ekb(inputId = "warp_tar_link_col", label = " "))),
                      conditionalPanel(
                        "input.warp_type == 'TPS'",
                        column(width = 4, align = "center",  style = "vertical-align: bottom; height: 20px;", 
                               colorSelectorDrop.ekb(inputId = "warp_grid_col", label = " ", dropdownside = "right")))),
                    conditionalPanel("input.warp_type == 'TPS' || input.warp_type == 'points' ", br()),
                    fluidRow(
                      conditionalPanel(
                        "input.warp_type == 'points' || input.warp_type == 'vector'",
                        column(width = 4, align = "center", style = "vertical-align: bottom;", p("Ref Points:"))),
                      conditionalPanel(
                        "output.links_dbclick_initiated && (input.warp_type == 'points' || input.warp_type == 'vector')",
                        column(width = 4, align = "center", style = "vertical-align: bottom;", p("Ref Links:"))), # !!! this does target links when warp type is vectors
                      conditionalPanel(
                        "input.warp_labels",
                        column(width = 4, align = "center", style = "vertical-align: bottom;", p("Labels:")))),
                    fluidRow(
                      conditionalPanel(
                        "input.warp_type == 'points' || input.warp_type == 'vector'",
                        column(width = 4, align = "center", style = "vertical-align: bottom; height: 20px;", 
                               colorSelectorDrop.ekb(inputId = "warp_pt_bg", label = " ", selected = "gray"))),
                      conditionalPanel(
                        "output.links_dbclick_initiated && (input.warp_type == 'points' || input.warp_type == 'vector')",
                        column(width = 4, align = "center", style = "vertical-align: bottom; height: 20px;", 
                               colorSelectorDrop.ekb(inputId = "warp_link_col", label = " ", selected = "gray", dropdownside = "right"))),
                      conditionalPanel(
                        "input.warp_labels",
                        column(width = 4, align = "center",  style = "vertical-align: bottom; height: 20px;", 
                               colorSelectorDrop.ekb(inputId = "warp_txt_col", label = " ")))),
                    conditionalPanel(
                      "output.links_dbclick_initiated || input.warp_type == 'points' || input.warp_type == 'vector'",
                      br()), 
                    fluidRow(p(strong("Point Size"))),
                    fluidRow(
                      conditionalPanel(
                        "input.warp_type == 'points' || input.warp_type == 'vector'",
                        column(width = 4, align = "center",
                               numericInput(inputId = "warp_pt_size", label = "Reference", value = 1.5, min = 0, max = 10, step = 0.1))),
                      conditionalPanel(
                        "input.warp_type == 'TPS' || input.warp_type == 'points'",
                        column(width = 4, align = "center",
                               numericInput(inputId = "warp_tar_pt_size", label = "Target", value = 1, min = 0, max = 10, step = 0.1)))),
                    fluidRow(style = "height: 10px;", p(strong("Labels"))),
                    fluidRow(
                      column(width = 12, 
                             checkboxInput("warp_labels", "Show Landmark Labels", value = F))),
                    conditionalPanel(
                      "input.warp_labels",
                      fluidRow(
                        column(width = 4, align = "center",
                               numericInput(inputId = "warp_txt_cex", label = "Size", value = .8, min = 0, max = 3, step = 0.1)),
                        #column(width = 4, align = "center", 
                        #       numericInput(inputId = "warp_txt_adj", label = "Adjustment", value = .5, min = 0, max = 5, step = 0.1)),
                        column(width = 4, align = "center",
                               selectInput(inputId = "warp_txt_pos", label = "Position", selected = 1, 
                                           choices = list("Below" = 1, "Left" = 2, "Above" = 3, "Right" = 4)))),
                      br()),
                    conditionalPanel(
                      "output.links_dbclick_initiated",
                      fluidRow(p(strong("Links"))),
                      conditionalPanel(
                        "input.warp_type == 'points' || input.warp_type == 'vector'",
                        fluidRow(
                          column(width = 4, align = "center", 
                                 numericInput(inputId = "warp_link_lwd", label = "Width (Ref)", value = 2, min = 0, max = 10)),
                          column(width = 4, align = "center",
                                 selectInput(inputId = "warp_link_lty", label = "Type (Ref)", selected = 1, 
                                             choices = list("____" = 1, "_ _ _" = 2, "......" = 3,
                                                            "_._._" = 4,"__ __" = 5, "_ __ _" = 6))))),
                      conditionalPanel(
                        "input.warp_type == 'TPS' || input.warp_type == 'points'",
                        fluidRow(
                          column(width = 4, align = "center",
                                 numericInput(inputId = "warp_tar_link_lwd", label = "Width (Target)", value = 2, min = 0, max = 10)),
                          column(width = 4, align = "center", 
                                 selectInput(inputId = "warp_tar_link_lty", label = "Type (Target)", selected = 1, 
                                             choices = list("____" = 1, "_ _ _" = 2, "......" = 3,
                                                            "_._._" = 4,"__ __" = 5, "_ __ _" = 6)))))),
                    conditionalPanel(
                      "input.warp_type == 'TPS'",
                      fluidRow(p(strong("GridLines"))),
                      fluidRow(
                        column(width = 4, align = "center",
                               numericInput(inputId = "warp_grid_lwd", label = "Width", value = 1, min = 0, max = 10, step = 0.5)),
                        column(width = 4, align = "center", 
                               selectizeInput(inputId = "warp_grid_lty", label = "Line Type", selected = 1, 
                                              choices = list("____" = 1, "_ _ _" = 2, "......" = 3,
                                                             "_._._" = 4,"__ __" = 5, "_ __ _" = 6))),
                        column(width = 4, align = "center", 
                               numericInput(inputId = "warp_n_col_cell", label = "Density", value = 20, min = 0, max = 100, step = 5))))
                ))),
            fluidRow(
              align = "center", 
              actionButton("warp_reset", "Clear Warp Grid", width = 200, style='padding:6px; font-size:80%'), br(), br() # this button resets the warp grid specimen selected
            ))), 
        mainPanel(
          id = "scroll_warp_initiated",
          width = 9, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 100px); position:relative;", # Morphospace. click selects a specimen for the warpgrid and pc info. doubleclick is for random spot in morphospace
          wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                    plotOutput(
                      "morphospace", click = "morphospace_specimen_click", dblclick = "morphospace_projection_dbclick", 
                      width = "100%", height = 700),br(),
                    fluidRow(
                      column(
                        4, offset = 2, align = "right", 
                        downloadButton(
                          "export_morphospace", "Export Plot", 
                          style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7;  border-color: #337ab7;')),
                      column(
                        4, align = "left",
                        downloadButton(
                          "export_morphospace_code", "Export Code", icon = shiny::icon("registered"),
                          style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
                    ), 
                    br()), 
          br(), 
          conditionalPanel(
            condition = "input.tip_col_category == 'by_trait_1' || input.tip_col_category == 'by_trait_2'|| input.tip_col_category == 'by_trait_3' || input.tip_col_category == 'csize'",
            absolutePanel(id = "legend_tip_col_draggable", 
                          top= 100, left = 100, fixed=F, draggable = T,
                          plotOutput("legend_tip_col", height = "100px"))),
          conditionalPanel(
            condition = "(input.tip_col_category == 'by_trait_1' && input.show_convex_hull_2) || 
                         (input.tip_col_category == 'by_trait_1' && input.show_convex_hull_3) || 
                         (input.tip_col_category == 'by_trait_2' && input.show_convex_hull_1) || 
                         (input.tip_col_category == 'by_trait_2' && input.show_convex_hull_3) || 
                         (input.tip_col_category == 'by_trait_3' && input.show_convex_hull_1) || 
                         (input.tip_col_category == 'by_trait_3' && input.show_convex_hull_2) ||
                         (input.tip_col_category == 'csize' && input.show_convex_hull_1) || 
                         (input.tip_col_category == 'csize' && input.show_convex_hull_2) ||
                         (input.tip_col_category == 'csize' && input.show_convex_hull_3) ||
                         (input.tip_col_category == 'all_1_col' && input.show_convex_hull_1) || 
                         (input.tip_col_category == 'all_1_col' && input.show_convex_hull_2) || 
                         (input.tip_col_category == 'all_1_col' && input.show_convex_hull_3)",
            absolutePanel(id = "legend_hull_col_1_draggable", 
                          top= 200, left = 100, fixed=F, draggable = T,
                          plotOutput("legend_hull_col_1", height = "100px"))),
          conditionalPanel(
            condition = "(input.tip_col_category == 'by_trait_1' && input.show_convex_hull_2 && input.show_convex_hull_3) || 
                         (input.tip_col_category == 'by_trait_2' && input.show_convex_hull_1 && input.show_convex_hull_3) || 
                         (input.tip_col_category == 'by_trait_3' && input.show_convex_hull_1 && input.show_convex_hull_2) ||
                         (input.tip_col_category == 'all_1_col' && input.show_convex_hull_1 && input.show_convex_hull_2) || 
                         (input.tip_col_category == 'all_1_col' && input.show_convex_hull_1 && input.show_convex_hull_3) || 
                         (input.tip_col_category == 'all_1_col' && input.show_convex_hull_2 && input.show_convex_hull_3)",
            absolutePanel(id = "legend_hull_col_2_draggable", 
                          top= 300, left = 100, fixed=F, draggable = T,
                          plotOutput("legend_hull_col_2", height = "100px"))),
          conditionalPanel(
            condition = "input.tip_pch == 'by_trait_1' || input.tip_pch == 'by_trait_2' ||  input.tip_pch == 'by_trait_3'",
            absolutePanel(id = "legend_shape_draggable", 
                          top = 100, left = 300, fixed = F, draggable = T, 
                          plotOutput("legend_shape", height = "100px"))),
          conditionalPanel(
            condition = "output.morpho_click_initiated || output.morpho_dbclick_initiated",
            br(),
            wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                      fluidRow(
                        column(12, offset = 1, textOutput("warp_grid_name", container =h4))
                      ), hr(),
                      #fluidRow(column(9, offset = 1, h4("Global Integration"))),
                      plotOutput("warp_grid", width = "100%", height = 600), 
                      fluidRow(
                        column(4, offset = 2, 
                               align = "right",
                               downloadButton("export_warp_grid", "Export Warp Grid", 
                                              style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7;  border-color: #337ab7;')),
                        column(4, align = 'left',
                               downloadButton("export_warp_grid_code", "Export Code", icon = shiny::icon("registered"),
                                              style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; 
                                              border-color: #337ab7;'))), 
                      br()), br())
        ))),
    tabPanel(
      "Shape Patterns",
      add_busy_spinner(spin = "fading-circle",color = "#ceecf0", timeout = 300, height = '40px', width = '40px'),
      tabsetPanel(
        id = "tab_shapepatterns",
        tabPanel(
          "Modularity",
          sidebarLayout(
            mainPanel(width = 9, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;", 
                      br(),
                      wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                                fluidRow(column(9, offset = 1, h4("Module Visualization"))), hr(),
                                fluidRow(column(12, plotOutput("plot_modularity_visualization", width = "100%", height = 500))),
                                fluidRow(
                                  column(6, align = "right", 
                                         downloadButton(
                                           "export_modularity_visualization_plot", "Export Modularity Visualization", 
                                           style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                                  column(6, align = "left",
                                         downloadButton("export_modularity_visualization_code", "Export Code", icon = shiny::icon("registered"),
                                                        style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; 
                                              border-color: #337ab7;'))
                                )),
                      br(),
                      wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                                fluidRow(column(9, offset = 1, h4("Degree of Modularity"))), hr(),
                                fluidRow(
                                  column(6, plotOutput("plot_modularity_test", width = "100%", height = 400)),
                                  column(6, verbatimTextOutput("stats_modularity_test"))),
                                fluidRow(
                                  column(6, align = "right", 
                                         downloadButton("export_modularity_test_plot", "Export Modularity Plot", 
                                                        style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                                  column(6, align = "left", 
                                         downloadButton("export_modularity_test_code", "Export Code", icon = shiny::icon("registered"),
                                                        style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
                                )), 
                      br(),
                      conditionalPanel(
                        "output.file_phy_selected",
                        wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                                  fluidRow(column(9, offset = 1, h4("Evolutionary Rate Variation Between Modules"))),
                                  hr(),
                                  fluidRow(
                                    column(6, plotOutput("plot_compare_multi_evol_rates", width = "100%", height = 400)),
                                    column(6, verbatimTextOutput("stats_compare_multi_evol_rates"))),
                                  fluidRow(
                                    column(6, align = "right", 
                                           downloadButton("export_compare_multi_evol_rates_plot", "Export Rate Plot", 
                                                          style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                                    column(6, align = "left", 
                                           downloadButton("export_compare_multi_evol_rates_code", "Export Code", icon = shiny::icon("registered"),
                                                          style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
                                  )))),
            sidebarPanel(
              width = 3, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;", 
              hr(style="border-color: purple;"),
              fluidRow(align = "center", h4(strong("Settings"))), 
              hr(style="border-color: purple;"),
              radioButtons("modularity_n_groups", "Number of Modules:", 
                           choices = 2:8, inline = T), br(),
              fluidRow(column(7,div(style = "height:10px;", h5(strong("Module 1 Landmarks:")))), 
                       column(5, div(style = "height:10px;", 
                                     colorSelectorDrop.ekb("module_1_col", label = NULL, dropdownside = "right")))), 
              orderInput('modularity_group_1', NULL, items = NULL, width = "300px",
                         connect = c('modularity_group_2', 'modularity_group_3', 'modularity_group_4', 'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')), br(),
              fluidRow(column(7,div(style = "height:10px;", h5(strong("Module 2 Landmarks:")))), 
                       column(5, div(style = "height:10px;", colorSelectorDrop.ekb("module_2_col", label = NULL, dropdownside = "right", selected = all_color_options[1])))),
              orderInput('modularity_group_2', NULL, items = NULL, placeholder = 'Drag landmarks here...', width = "300px",
                         connect = c('modularity_group_1', 'modularity_group_3', 'modularity_group_4', 'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')), br(),
              conditionalPanel(
                "input.modularity_n_groups > 2",
                fluidRow(column(7,div(style = "height:10px;", h5(strong("Module 3 Landmarks:")))), column(5, div(style = "height:10px;", colorSelectorDrop.ekb("module_3_col", label = NULL, selected = all_color_options[4], dropdownside = "right")))),
                orderInput('modularity_group_3', NULL, items = NULL, placeholder = 'Drag landmarks here...', width = "300px", 
                           connect = c( 'modularity_group_1','modularity_group_2','modularity_group_4', 'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')), br()),
              conditionalPanel(
                "input.modularity_n_groups > 3",
                fluidRow(column(7,div(style = "height:10px;", h5(strong("Module 4 Landmarks:")))), column(5, div(style = "height:10px;", colorSelectorDrop.ekb("module_4_col", label = NULL, selected = all_color_options[6], dropdownside = "right")))),
                orderInput('modularity_group_4', NULL, items = NULL, placeholder = 'Drag landmarks here...', width = "300px", 
                           connect = c( 'modularity_group_1','modularity_group_2','modularity_group_3',  'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')), br()),
              conditionalPanel(
                "input.modularity_n_groups > 4",
                fluidRow(column(7, div(style = "height:10px;", h5(strong("Module 5 Landmarks:")))), column(5, div(style = "height:10px;", colorSelectorDrop.ekb("module_5_col", label = NULL, selected = all_color_options[11]), dropdownside = "right"))),
                orderInput('modularity_group_5', NULL, items = NULL, placeholder = 'Drag landmarks here...', width = "300px",
                           connect = c('modularity_group_1','modularity_group_2','modularity_group_3','modularity_group_4', 'modularity_group_6',  'modularity_group_7', 'modularity_group_8')), br()),
              conditionalPanel(
                "input.modularity_n_groups > 5",
                fluidRow(column(7, div(style = "height:10px;", h5(strong("Module 6 Landmarks:")))), column(5, div(style = "height:10px;", colorSelectorDrop.ekb("module_6_col", label = NULL, selected = all_color_options[34], dropdownside = "right")))),
                orderInput('modularity_group_6', NULL, items = NULL, placeholder = 'Drag landmarks here...', width = "300px",
                           connect = c('modularity_group_1','modularity_group_2','modularity_group_3','modularity_group_4', 'modularity_group_5','modularity_group_7',  'modularity_group_8')), br()),
              conditionalPanel(
                "input.modularity_n_groups > 6",
                fluidRow(column(7, div(style = "height:10px;", h5(strong("Module 7 Landmarks:")))), column(5, div(style = "height:10px;", colorSelectorDrop.ekb("module_7_col", label = NULL, selected = all_color_options[44], dropdownside = "right")))),
                orderInput('modularity_group_7', NULL, items = NULL, placeholder = 'Drag landmarks here...', width = "300px",
                           connect = c( 'modularity_group_8', 'modularity_group_1', 'modularity_group_2', 'modularity_group_3', 'modularity_group_4', 'modularity_group_5', 'modularity_group_6')), br()),
              conditionalPanel(
                "input.modularity_n_groups > 7",
                fluidRow(column(7,div(style = "height:10px;", h5(strong("Module 8 Landmarks:")))), column(5, div(style = "height:10px;", colorSelectorDrop.ekb("module_8_col", label = NULL, selected = all_color_options[53], dropdownside = "right")))),
                orderInput('modularity_group_8', NULL, items = NULL, placeholder = 'Drag landmarks here...', width = "300px",
                           connect = c('modularity_group_1', 'modularity_group_2', 'modularity_group_3', 'modularity_group_4', 'modularity_group_5', 'modularity_group_6', 'modularity_group_7'))),
              br(),
              fluidRow(column(12, align = "center", 
                              actionButton("apply_modular_groups_go", label = "Assign LMs to Modules", width = '100%', 
                                           style='padding:6px; font-size:85%; background-color: #003366; border-color: #003366;'))),
              hr(),
              fluidRow(column(12, sliderInput("plot_modularity_visualization_cex", label= "Landmark Size:", min = 0, max = 4, value = 1, step = .1))),
              hr(),
              conditionalPanel(
                "output.file_phy_selected",
                radioButtons("modularity_phylo_tf", "Evaluate Modularity in a Phylogenetic Context:", choices = c("True" = T, "False" = F), selected = F), br()),
              numericInput(inputId = "modularity_perm", label = "Number of Permutations:", value = 999, min = 0, max = 99999, step = 100), br()
            ))),
        tabPanel(
          "Integration",
          sidebarLayout(
            mainPanel(
              width = 9, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 130px); position:relative;", br(),
              wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                        fluidRow(column(9, offset = 1, h4("Global Integration"))),
                        hr(),
                        fluidRow(column(12, plotOutput("global_integration_plot", width = "100%", height = 600))),
                        fluidRow(
                          column(6, align = 'right',
                                 downloadButton('export_global_integration_plot', 'Export Global Integration Plot', 
                                                style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                          column(6, align = 'left',  
                                 downloadButton("export_global_integration_code", "Export Code", icon = shiny::icon("registered"),
                                                style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
                        )),
              br(), 
              wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                        fluidRow(column(9, offset = 1, h4("Integration Between Modules (defined in 'Modularity' tab)"))), hr(),
                        fluidRow(
                          column(6, plotOutput("integration_test_plot", width = "100%", height = 400)),
                          column(6, fluidRow(verbatimTextOutput("integration_test_results")))),
                        fluidRow(
                          column(6, align = 'right',
                                 downloadButton('export_integration_test_plot', 'Export Integration Test Plot', 
                                                style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                          column(6, align = 'left',
                                 downloadButton("export_integration_test_code", "Export Code", icon = shiny::icon("registered"),
                                                style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
                        ), 
                        br()), br(), br()),
            sidebarPanel(
              width = 3, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 130px); position:relative;", 
              hr(style="border-color: purple;"),
              fluidRow(align = "center", h4(strong("Settings"))), 
              hr(style="border-color: purple;"),
              conditionalPanel(
                "output.file_trait_selected",
                radioButtons("integration_group_by", "Integration Groups Defined:",
                             choices = c("None" = "none", "Trait 1" = "by_trait_1",
                                         "Trait 2" = "by_trait_2", "Trait 3" = "by_trait_3"),
                             selected = "none")
              ),
              br(),
              conditionalPanel("input.integration_group_by != 'none'", 
                               radioButtons("integration_group_level", "Trait Level Plotted:",
                                            choices = c("First" = 1, "Second" = 2, "Third" = 3)), br()),
              conditionalPanel(
                "output.file_phy_selected",
                radioButtons("integration_phylo_tf", "Evaluate Integration in a Phylogenetic Context:", choices = c("True" = T, "False" = F), selected = F), br()),
              numericInput(inputId = "integration_test_perm", label = "Number of Permutations:", 
                           value = 999, min = 0, max = 99999, step = 100)
            ))),
        tabPanel(
          "Symmetry",
          sidebarLayout(
            mainPanel(
              width = 9, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 130px); position:relative;", br(),
              wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                        fluidRow(column(9, offset = 1, h4("Symmetry of Shape Variation"))), hr(),
                        conditionalPanel(
                          "!output.symmetry_initiated", br(),
                          em(h5("Define symmetry analysis parameters in 'Settings', then press 'Calculate'."))
                        ),
                        conditionalPanel(
                          "output.symmetry_initiated",
                          fluidRow(
                            column(6, plotOutput("symmetry_plot", width = "100%", height = 700), br()),
                            column(6, verbatimTextOutput("symmetry_results"))),
                          fluidRow(
                            column(1),
                            column(3, align = 'center',
                                   downloadButton('export_symmetry_plot', 'Export Symmetry Plot', 
                                                  style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                            column(4, align = "center",
                                   downloadButton("export_symmetry_code", "Export Code", icon = shiny::icon("registered"),
                                                  style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                            column(3, align = 'center',
                                   downloadButton('export_symmetry_tables', 'Export Symmetry Table(s)', 
                                                  style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')))), 
                        br()), br(), 
              conditionalPanel("input.symmetry_obj_sym == 'TRUE'",
                               wellPanel(
                                 style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                                 fluidRow(column(9, offset = 1, h4("Landmark Side Assignment Visualization"))), hr(),
                                 plotOutput("symmetry_landpair_plot", width = "100%", height = 400), br(),
                                 fluidRow(
                                   column(6, align = "right", 
                                          downloadButton('export_symmetry_landpair_plot', 'Export Landmark Sides Plot', 
                                                         style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                                   column(6, align = "left", 
                                          downloadButton('export_symmetry_landpair_code', 'Export Code', icon = shiny::icon("registered"),
                                                         style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
                                 ))), 
              br()),
            sidebarPanel(
              width = 3, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 150px); position:relative;", 
              hr(style="border-color: purple;"),
              fluidRow(align = "center", h4(strong("Settings"))), 
              hr(style="border-color: purple;"),
              fluidRow(column(12, align = "center", 
                              radioButtons("symmetry_obj_sym", "Symmetry Type:", choices = c("Object" = T, "Matching" = F), inline = T))),
              h5("Define Specimen Assignments:"),
              fluidRow(column(6, align = "center", actionButton("go_symmetry_file", "Upload a File", width = "100%", style='padding:6px; margin:0px;font-size:80%')),
                       column(6, align = "center", actionButton("go_symmetry_manual", "Manual Entry", width = "100%", style='padding:6px;margin:0px; font-size:80%'))), 
              conditionalPanel(
                "input.symmetry_obj_sym == 'TRUE'", 
                br(),
                fluidRow(
                  column(6, align = "center", offset = 3, 
                         actionButton("go_symmetry_no_replicates", 
                                      "No Replicates", width = "100%", style='padding:6px;margin:0px; font-size:80%'))), br(),
                conditionalPanel(
                  "output.show_no_replicates_text", h6(em("The user has indicated that there are no replicates in the data."))
                )
              ),         
              conditionalPanel(
                "output.show_symmetry_file",
                br(),
                fileInput("symmetry_file_upload",label = "Choose File Defining Specimen Assignments:", 
                          multiple = FALSE, width = "auto", # input trait file. at the moment, only .csv files are allowed
                          accept = c("text/csv",".csv", "text/xls", ".xls", "text/xlsx",
                                     ".XLSX","text/CSV",".CSV", "text/XLS", ".XLS", "text/XLSX", ".XLSX"), 
                          placeholder = "No file selected")),
              conditionalPanel(
                "output.show_symmetry_definitions",
                wellPanel(
                  style = "font-size:80%; align: center; overflow: hidden; overflow-y: scroll; max-height: 400px; position:relative;",
                  matrixInput(
                    inputId = "symmetry_definitions", label = "Specimen Assignments",
                    value = symmetry_manual_matrix, class = "character",
                    cols = list(names = T), rows = list(names = T, extend = T, editableNames = T)))),
              conditionalPanel(
                "input.symmetry_obj_sym == 'TRUE'",
                hr(),
                h5("Assign Landmarks to Sides:"),
                fluidRow(column(6, align = "center", actionButton("go_symmetry_landpairs_file", "Upload a File", width = "100%", style='padding:6px; margin:0px;font-size:80%')),
                         column(6, align = "center", actionButton("go_symmetry_landpairs_manual", "Manual Entry", width = "100%", style='padding:6px;margin:0px; font-size:80%'))), br()),
              conditionalPanel(
                "output.show_symmetry_landpairs_file && input.symmetry_obj_sym == 'TRUE'",
                fileInput("symmetry_land_pairs_file_upload",label = "Choose File Defining Landmark Side Assignments:", 
                          multiple = FALSE, width = "auto", # input trait file. at the moment, only .csv files are allowed
                          accept = c("text/csv",".csv", "text/xls", ".xls", "text/xlsx", ".xlsx",
                                     ".XLSX","text/CSV",".CSV", "text/XLS", ".XLS", "text/XLSX", ".XLSX"), 
                          placeholder = "No file selected")
              ),
              conditionalPanel(
                "output.show_symmetry_landpairs_definitions", 
                wellPanel(
                  style = "font-size:80%; align: center; overflow: hidden; overflow-y: scroll; max-height: 400px; position:relative;",
                  matrixInput(
                    inputId = "symmetry_landpairs_definitions", 
                    label = "Landmark Side Assignments",
                    value = symmetry_landpairs_manual_matrix, 
                    class = "numeric",
                    cols = list(names = T), rows = list(names = F, extend = T)))),
              hr(),
              numericInput(inputId = "symmetry_perm", label = "Number of Permutations:", 
                           value = 999, min = 0, max = 99999, step = 100),
              fluidRow(column(12, align = "center", 
                              actionButton("run_symmetry_go", "Calculate",
                                           width = '100%', 
                                           style='padding:6px; font-size:100%; background-color: #003366; border-color: #003366;'))), 
              conditionalPanel("output.symmetry_initiated" , hr(),
                               fluidRow(column(12, align = "center", actionButton("go_symmetry_useoutput", "Use Symmetric Component of Shape Variation", style='width: 100%; font-size:80%;'))), br(),
                               fluidRow(column(12, align = "center", downloadButton("export_symmetry_useoutput", "Export Symmetric Component of Shape Variation", style='width: 100%; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))), br(),
                               fluidRow(column(12, align = "center", actionButton("go_asymmetry_useoutput", "Use Asymmetric Component of Shape Variation", style='width: 100%; font-size:80%;'))), br(),
                               fluidRow(column(12, align = "center", downloadButton("export_asymmetry_useoutput", "Export Asymmetric Component of Shape Variation", style='width: 100%; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')))),
              conditionalPanel("output.symmetry_pushed", hr(),
                               fluidRow(column(12, align = "center", actionButton("symmetry_reset", "Clear Symmetry Settings", style='width: 100%; padding:6px; font-size:80%;')))), 
              br(),
            ))),
        tabPanel(
          "Phylogenetic Signal",
          sidebarLayout(
            mainPanel(
              width = 9, br(),
              wellPanel(
                style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                fluidRow(
                  column(6, plotOutput(
                    "phy_signal_plot", width = "100%", height = 500)),
                  column(6, verbatimTextOutput(
                    "phy_signal_results"
                  ))),
                fluidRow(
                  column(6, align = 'right',
                         downloadButton('export_phy_signal_plot', 'Export Signal Plot', icon = shiny::icon("registered"),
                                        style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                  column(6, align = "left",
                         downloadButton('export_phy_signal_code', 'Export Code',icon = shiny::icon("registered"),
                                        style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
                ), br())),
            sidebarPanel(
              width = 3, 
              hr(style="border-color: purple;"),
              fluidRow(align = "center", h4(strong("Settings"))), 
              hr(style="border-color: purple;"),
              radioButtons("phy_signal_input", "Test Signal of:",
                           choices = c("Shape" = "shape")),
              numericInput(inputId = "signal_perm", label = "Number of Permutations:", 
                           value = 999, min = 0, max = 99999, step = 100)
            )))
      )),
    
    tabPanel(
      "Linear Models",
      add_busy_spinner(spin = "fading-circle",color = "#ceecf0", timeout = 300, height = '40px', width = '40px'),
      tabsetPanel(
        id = "tab_linearmodels",
        tabPanel(
          "Model Design",
          sidebarLayout(
            mainPanel(
              width = 9, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;",
              br(),
              fluidRow(
                column(12, offset = 1, textOutput("model_tested", container = h5))), hr(),
              wellPanel(
                style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                fluidRow(
                  column(12, h4("Results:"))), 
                fluidRow(
                  column(6, h5("Model Fit Output"))),
                fluidRow(
                  column(12, verbatimTextOutput("stats_shape_trait_overall_fit"))),
                fluidRow(
                  column(6, h5("ANOVA Table")), 
                  conditionalPanel(condition = "!(input.disparity_groups == 'no_run')", 
                                   column(3, h5("Procrustes Group Variances"))) , 
                  conditionalPanel(condition = "!(input.evol_rate_groups == 'no_run')", 
                                   column(3, h5("Evolutionary Rates (sigma_d)")))),
                fluidRow(
                  column(6, style = "border-right: 1px dashed white;", 
                         tableOutput("stats_shape_trait_overall")), 
                  conditionalPanel(condition = "!(input.disparity_groups == 'no_run')", 
                                   column(3, tableOutput("stats_morphol_disp_variances"))), 
                  conditionalPanel(condition = "!(input.evol_rate_groups == 'no_run')", 
                                   column(3, style = "border-left: 1px dashed white;", tableOutput("stats_evol_rates")))), 
                fluidRow(
                  column(3, align = "right",
                         downloadButton('export_aov_table', 'Export ANOVA Results', 
                                        style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                  column(3, align = "left",
                         downloadButton('export_aov_table_code', 'Export Code',icon = shiny::icon("registered"),
                                        style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                  conditionalPanel(
                    condition = "!(input.disparity_groups == 'no_run')", 
                    column(3, align = "center",downloadButton('export_proc_variances', 'Export Disparity Results', 
                                                              style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))),
                  conditionalPanel(
                    condition = "!(input.evol_rate_groups == 'no_run')", 
                    column(3, align = "center",downloadButton('export_evol_rates', 'Export Rates Results', 
                                                              style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))))), 
              conditionalPanel(
                condition = "!(input.trait_pairwise == 'no_run')",
                wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                          fluidRow(column(12, h4("Pairwise Results:"))),
                          fluidRow(
                            
                            column(6, h5("Pairwise ANOVA Table")), 
                            conditionalPanel(condition = "!(input.disparity_groups == 'no_run')", 
                                             column(3, h5("Pairwise Disparity Table"))), 
                            conditionalPanel(condition = "!(input.evol_rate_groups == 'no_run')", 
                                             column(3, h5("Pairwise Rates Table")))),
                          fluidRow(
                            column(6, 
                                   style = "border-right: 1px dashed white;",
                                   tableOutput("stats_shape_trait_pairwise")), 
                            conditionalPanel(condition = "!(input.disparity_groups == 'no_run')", 
                                             column(3, style = "border-right: 1px dashed white;", tableOutput("stats_morphol_disp_pairwise"))), 
                            conditionalPanel(condition = "!(input.evol_rate_groups == 'no_run')", 
                                             column(3, tableOutput("stats_evol_rates_pairwise")))),
                          fluidRow(
                            column(6, align = "center",
                                   downloadButton("export_pairwise_table", "Export Pairwise ANOVA Results", 
                                                  style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                            conditionalPanel(
                              condition = "!(input.disparity_groups == 'no_run')", 
                              column(3, align = "center",downloadButton("export_disparity_pairwise_table", "Export Pairwise Disparity Results", 
                                                                        style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))),
                            conditionalPanel(
                              condition = "!(input.evol_rate_groups == 'no_run')", 
                              column(3, align = "center",
                                     downloadButton("export_rates_pairwise_table", "Export Pairwise Rates Results", 
                                                    style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')))))), 
              br(), br()), 
            sidebarPanel(
              width = 3, # options for the stats
              tags$style(".well {border-color: gray; background-color: #f5feff}"), 
              style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;",
              hr(style="border-color: purple;"),
              fluidRow(align = "center", h4(strong("Settings"))), hr(style="border-color: purple;"),
              checkboxGroupInput(inputId = "independent_variables", "Independent Variables Tested:", inline = F,
                                 choices = c("Trait 1" = "init", "Trait 2" = "3", "Trait 3" = "4"), selected = "2"), 
              conditionalPanel(condition = "output.multiple_terms_selected", 
                               orderInput(inputId = 'independent_variables_order', label = "Trait Order:", items = c("Trait 1"), width = "300px"),
                               #                 checkboxInput(inputId = "interactions_included", "Include Interactions")
                               br()),
              conditionalPanel(condition = "output.file_phy_selected", 
                               radioButtons(inputId = "pgls_ols", "Analysis Type:", inline = T, choices = c("PGLS" = "pgls", "OLS" = "ols"), selected = "ols")),
              conditionalPanel(
                condition = "output.file_trait_selected",
                radioButtons(inputId = "trait_pairwise", "Trait Grouping for Pairwise Comparisons:", inline = F, choices = c("Not Run" = "no_run", "Trait 1" = 1, "Trait 2" = 2)),
                radioButtons(inputId = "disparity_groups", "Morphological Disparity Groups Tested:", inline = F,
                             choices = c("Not Run" = "no_run", "Trait 1" = "1", "Trait 2" = "2", "Trait 3" = "3")),
                conditionalPanel(
                  "output.file_phy_selected",
                  radioButtons(inputId = "evol_rate_groups", "Evolutionary Rate Groups Tested:", inline = F,
                               choices = c("Not Run" = "no_run", "Trait 1" = "1", "Trait 2" = "2", "Trait 3" = "3")))),
              numericInput(inputId = "anova_perm", label = "Number of Permutations:", value = 999, min = 0, max = 99999, step = 100), br(),
              selectInput(inputId = "ss_type", "Sums of Squares Calculation Method:", 
                          choices = c("Type 1" = "I", "Type 2" = "II", "Type 3" = "III"),
                          selected = "I"),
              actionButton("go_run_anova", "Calculate", width = '100%', style='padding:6px; font-size:85%; background-color: #003366; border-color: #003366;'), br(), br()
            ))),
        tabPanel(
          "Allometry",
          sidebarLayout(
            mainPanel(
              width = 9, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;",
              br(),
              fluidRow(column(9, offset = 1, textOutput("model_tested_2", container = h5))),
              hr(),
              plotOutput("allometry_plot", width = "100%", height = 600), br(),
              fluidRow(
                column(6, align = 'right',
                       downloadButton('export_allometry_plot', 'Export Allometry Plot', 
                                      style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                column(6, align = 'left',
                       downloadButton('export_allometry_code', "Export Code", icon = shiny::icon("registered"),
                                      style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
              ), br(), br()),
            sidebarPanel(
              width = 3, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;",
              hr(style="border-color: purple;"),
              fluidRow(align = "center", h4(strong("Settings"))), 
              hr(style="border-color: purple;"),
              radioButtons("allometry_type", "Allometry Type:", choices = c("Regression" = "regression")),
              radioButtons("allometry_reg_type", "Regression Type:", 
                           choices = c("Prediction Line" = "PredLine", "Regression Score" = "RegScore")),
              radioButtons("allometry_predictor", "Predictor Variable:", choices = c("Upload Trait Data" = "none")),
              conditionalPanel(
                "input.allometry_predictor == 'csize'",
                radioButtons("allometry_log_csize", "Log Transform Centroid Size:", choices = c("True" = TRUE, "False" = FALSE))),
              checkboxInput("show_allometry_tip_label", strong("Show Tip Labels")),
              selectInput("allometry_color", "Color Points:", choices = c("All One Color" = "all_1_col")),
              conditionalPanel(
                condition = "(input.trait_1_treatment == 'disc' || input.trait_2_treatment == 'disc' || input.trait_3_treatment == 'disc') && output.file_trait_selected", 
                checkboxInput("show_allometry_convex_hull_1", "Show Convex Hulls Around Trait 1")),
              conditionalPanel(
                condition = "output.two_disc_traits_selected && output.file_trait_selected",
                checkboxInput("show_allometry_convex_hull_2", "Show Convex Hulls Around Trait 2")),
              conditionalPanel(
                condition = "output.three_disc_traits_selected && output.file_trait_selected",
                checkboxInput("show_allometry_convex_hull_3", "Show Convex Hulls Around Trait 3")),
              conditionalPanel(
                "output.allometry_color_nlev > 1",
                fluidRow(
                  column(2, align = "center", textOutput("allometry_lev_1", container = h6)),
                  column(2, align = "center", textOutput("allometry_lev_2", container = h6)),
                  conditionalPanel(
                    "output.allometry_color_nlev > 2",
                    column(2, align = "center", textOutput("allometry_lev_3", container = h6)),
                    conditionalPanel(
                      "output.allometry_color_nlev > 3",
                      column(2, align = "center", textOutput("allometry_lev_4", container = h6)),
                      conditionalPanel(
                        "output.allometry_color_nlev > 4",
                        column(2, align = "center", textOutput("allometry_lev_5", container = h6)),
                        conditionalPanel(
                          "output.allometry_color_nlev > 5",
                          column(2, align = "center", textOutput("allometry_lev_6", container = h6))))))),
                fluidRow(
                  column(2, align = "center", colorSelectorDrop.ekb("allom_color_1", "Lev 1", selected = all_color_options[1])),
                  column(2, align = "center", colorSelectorDrop.ekb("allom_color_2", "Lev 2", selected = all_color_options[5])),
                  conditionalPanel(
                    "output.allometry_color_nlev > 2",
                    column(2, align = "center",colorSelectorDrop.ekb("allom_color_3", "Lev 3", selected = all_color_options[6])),
                    conditionalPanel(
                      "output.allometry_color_nlev > 3",
                      column(2, align = "center",colorSelectorDrop.ekb("allom_color_4", "Lev 4", selected = all_color_options[8], dropdownside = "right")),
                      conditionalPanel(
                        "output.allometry_color_nlev > 4",
                        column(2, align = "center",colorSelectorDrop.ekb("allom_color_5", "Lev 5", selected = all_color_options[52], dropdownside = "right")),
                        conditionalPanel(
                          "output.allometry_color_nlev > 5",
                          column(2, align = "center",colorSelectorDrop.ekb("allom_color_6", "Lev 6", selected = all_color_options[9], dropdownside = "right")))
                      )
                    ))
                ), br()
              ),
              sliderInput("allometry_pt_size", "Point Size:", min = 0.1, max = 5, value = 1), br()
            ))),
        tabPanel(
          "Model Comparison",
          sidebarLayout(
            mainPanel(
              width = 9,style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;",
              br(),
              fluidRow(column(11, offset = 1, textOutput("model_comparison_model_1", container = h5))), 
              fluidRow(column(11, offset = 1, textOutput("model_comparison_model_2", container = h5))),
              conditionalPanel(
                "input.add_third_model",
                fluidRow(column(11, offset = 1, textOutput("model_comparison_model_3", container = h5)))), hr(),
              wellPanel(
                style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                fluidRow(
                  column(12, h4("Results:"))), 
                fluidRow(
                  column(6, h5("Model Fit Output"))),
                fluidRow(
                  column(12, verbatimTextOutput("model_comparison_fit"))),
                fluidRow(
                  column(6, h5("ANOVA Table"))), 
                fluidRow(column(9, tableOutput("model_comparison"))),
                fluidRow(column(6, align = "right", 
                                downloadButton('export_model_comparison', 'Export Results', 
                                               style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                         column(6, align = "left",
                                downloadButton("export_model_comparison_code", "Export Code", icon = shiny::icon("registered"),
                                               style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')))
              )),
            sidebarPanel(
              width = 3, 
              tags$style(".well {border-color: gray; background-color: #f5feff}"), 
              style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;",
              hr(style="border-color: purple;"),
              fluidRow(align = "center", h4(strong("Settings"))), hr(style="border-color: purple;"),
              checkboxGroupInput(inputId = "independent_variables_model_1", "Independent Variables Tested in Model 1:", inline = F,
                                 choices = c("Trait 1" = "init", "Trait 2" = "3", "Trait 3" = "4"), selected = "2"), 
              conditionalPanel(condition = "output.multiple_terms_selected_model_1", 
                               orderInput(inputId = 'independent_variables_order_model_1', label = "Model 1 Trait Order:", 
                                          items = c("Trait 1"), width = "300px")), br(),
              checkboxGroupInput(inputId = "independent_variables_model_2", "Independent Variables Tested in Model 2:", inline = F,
                                 choices = c("Trait 1" = "init", "Trait 2" = "3", "Trait 3" = "4"), selected = "2"), 
              conditionalPanel(condition = "output.multiple_terms_selected_model_2", 
                               orderInput(inputId = 'independent_variables_order_model_2', label = "Model 2 Trait Order:", 
                                          items = c("Trait 1"), width = "300px")), 
              checkboxInput("add_third_model", em("Add a Third Model")),
              conditionalPanel("input.add_third_model", 
                               checkboxGroupInput(inputId = "independent_variables_model_3", "Independent Variables Tested in Model 3:", inline = F,
                                                  choices = c("Trait 1" = "init", "Trait 2" = "3", "Trait 3" = "4"), selected = "2"), 
                               conditionalPanel(condition = "output.multiple_terms_selected_model_3", 
                                                orderInput(inputId = "independent_variables_order_model_3", label = "Model 3 Trait Order:", 
                                                           items = c("Trait 1"), width = "300px"))),
              #checkboxInput(inputId = "interactions_included_model_comparison", "Include Interactions"),
              radioButtons(inputId = "pgls_ols_model_comparison", "Analysis Type:", inline = T, choices = c("PGLS" = "pgls", "OLS" = "ols"), selected = "pgls"),
              numericInput(inputId = "anova_perm_model_comparison", label = "Number of Permutations:", value = 999, min = 0, max = 99999, step = 100), br(),
              selectInput(inputId = "ss_type_model_comparison", "Sums of Squares Calculation Method:", 
                          choices = c("Type 1" = "I", "Type 2" = "II", "Type 3" = "III"), selected = "I"),
              
              fluidRow(column(12, align="center",actionButton("go_run_model_comparison", "Calculate", width = '100%', 
                                                              style='padding:6px; font-size:85%; background-color: #003366; border-color: #003366;'))), br()
            )
          )),
        
        tabPanel(
          "Trajectory Analysis", # 
          sidebarLayout(
            mainPanel(
              width = 9, br(),
              fluidRow(column(11, offset = 1, textOutput("model_trajectory", container = h5))), 
              hr(), 
              wellPanel(
                style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                conditionalPanel(
                  "output.traj_initiated",
                  fluidRow(
                    column(
                      6, align = "center",
                      plotOutput("trajectory_plot", height = "600px")), 
                    column(
                      6, div(style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 380px); 
                                position:relative;",
                             verbatimTextOutput("trajectory_results")))), br(),
                  fluidRow(
                    column(
                      3, align = "center", offset = 1, 
                      downloadButton('export_trajectory_plot', 'Export Trajectory Plot', 
                                     style='width: 200px; padding:6px; font-size:80%; 
                                               background-color: #337ab7; border-color: #337ab7;')),
                    column(
                      4, align = "center",
                      downloadButton('export_trajectory_code', "Export Code", icon = shiny::icon("registered"),
                                     style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                    column(
                      3, align = "center",
                      downloadButton('export_trajectory_results', 'Export Trajectory Statistics', 
                                     style='width: 200px; padding:6px; font-size:80%; 
                                                   background-color: #337ab7; border-color: #337ab7;'))))),
              br()),
            sidebarPanel(
              width = 3, 
              style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 160px); position:relative;",
              hr(style="border-color: purple;"),
              fluidRow(align = "center", h4(strong("Settings"))), 
              hr(style="border-color: purple;"),
              radioButtons("trajectory_group", "Group Trajectories By:", choices = c("Upload Group Data" = "none")),
              conditionalPanel(
                "output.traj_trait_selected",
                radioButtons("trajectory_trait", "Trajectory Across Trait:", choices = c("Upload Trait Data" = "none"))),
              radioButtons("trajectory_independent_var", "Additional Covariate:", choices = c("None" = "none")),
              radioButtons("trajectory_attribute", "Attributes:", choices = c("Magnitude Difference" = "MD", 
                                                                              "Trajectory Correlations" = "TC",
                                                                              "Trajectory Shape Differences" = "SD")),
              numericInput(inputId = "traj_perm", label = "Number of Permutations:", 
                           value = 999, min = 0, max = 99999, step = 100),
              actionButton("go_trajectory_run", "Calculate", width = '100%', 
                           style='padding:6px; font-size:85%; background-color: #003366; border-color: #003366;'),
              hr(),
              conditionalPanel(
                "output.traj_trait_nlevels > 0",
                fluidRow(column(12,h5(strong("Trait Level Colors:")))),
                conditionalPanel(
                  "output.traj_trait_selected",
                  fluidRow(div(style = "margin: 0px; padding: 0px; vertical-align: bottom;",
                               column(2, align = "center", textOutput("traj_trait_level_1", container = h6)),
                               column(2, align = "center", textOutput("traj_trait_level_2", container = h6)),
                               column(2, align = "center", textOutput("traj_trait_level_3", container = h6)),
                               column(2, align = "center", textOutput("traj_trait_level_4", container = h6)),
                               column(2, align = "center", textOutput("traj_trait_level_5", container = h6)),
                               column(2, align = "center", textOutput("traj_trait_level_6", container = h6)))
                  ),
                  fluidRow(div(style = "margin: 0px; padding: 0px; vertical-align: bottom;",
                               column(2, align = "center", colorSelectorDrop.ekb("traj_trait_1_col", "Level 1", selected = all_color_options[44])),
                               column(2, align = "center", colorSelectorDrop.ekb("traj_trait_2_col", "Level 2", selected = all_color_options[1])),
                               conditionalPanel(
                                 "output.traj_trait_nlevels > 2",
                                 column(2, align = "center", colorSelectorDrop.ekb("traj_trait_3_col", "Level 3", selected = all_color_options[3]))),
                               conditionalPanel(
                                 "output.traj_trait_nlevels > 3", 
                                 column(2, align = "center", colorSelectorDrop.ekb("traj_trait_4_col", "Level 4", selected = all_color_options[5], 
                                                                                   dropdownside = "right"))),
                               conditionalPanel(
                                 "output.traj_trait_nlevels > 4", 
                                 column(2, align = "center", colorSelectorDrop.ekb("traj_trait_5_col", "Level 5", selected = all_color_options[7], 
                                                                                   dropdownside = "right"))),
                               conditionalPanel(
                                 "output.traj_trait_nlevels > 5", 
                                 column(2, align = "center", colorSelectorDrop.ekb("traj_trait_6_col", "Level 6", selected = all_color_options[9], 
                                                                                   dropdownside = "right"))))
                  )), br()),
              fluidRow(
                column(12,
                       sliderInput("trajectory_specimen_cex", "Specimen Point Size:", min = 0, max = 5, value = 0.8, step = 0.1))),
              fluidRow(
                column(12,
                       sliderInput("trajectory_traj_cex", "Trajectory Mean Point Size:", min = 0, max = 5, value = 1.5, step = 0.1))),
              
              
              br(), br())
          ))
        
      )),
    tabPanel(
      "Extras",  
      add_busy_spinner(spin = "fading-circle",color = "#ceecf0", timeout = 300, height = '40px', width = '40px'),
      tabsetPanel(
        id = "tab_extras",
        tabPanel(
          "Tutorials", style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;", 
          br(),
          wellPanel(
            style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
            fluidRow(column(9, offset = 1, h4("Part 1: Data Input"))),hr(),
            fluidRow(
              column(
                11, 
                HTML('<iframe src="https://player.vimeo.com/video/530589702?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479" 
                     width="900" height="500" frameborder="0" allow="autoplay; fullscreen; picture-in-picture" allowfullscreen 
                     title="Data Input"></iframe>'))), br(),
            fluidRow(
              column(
                11, offset = 1,
                downloadButton("export_demo_tps", label = "Export Demo TPS File", 
                               style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'), 
                downloadButton("export_demo_phy", label = "Export Demo Phylogeny File", 
                               style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'),
                downloadButton("export_demo_trait", label = "Export Demo Trait File", 
                               style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')
              )),
            br(),
            fluidRow(
              column(9, offset = 1,
                     HTML('<h5>Updates since tutorial recording:</h5>
                   <li>Demo files available for all file uploads (see buttons above)</li>
                   <li>Download code buttons now pull all necessary info; no extra step needed to download data in current state</li>
                   <li>Instructions differ slightly, more information on various pages</li>
                   ')))
          ),
          wellPanel(
            style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
            fluidRow(column(9, offset = 1, h4("Part 2: Data Prep"))),hr(),
            fluidRow(
              column(
                11, 
                HTML('<iframe src="https://player.vimeo.com/video/530590526?title=0&amp;byline=0&amp;portrait=0&amp;speed=0&amp;badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479" 
                     width="900" height="500" frameborder="0" allow="autoplay; fullscreen; picture-in-picture" allowfullscreen 
                     title="Data Prep"></iframe>')))
          ),
          wellPanel(
            style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
            fluidRow(column(9, offset = 1, h4("Part 3: Morphospace and Warp Grids"))),hr(),
            fluidRow(
              column(
                11, 
                HTML('<iframe src="https://player.vimeo.com/video/530951457?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479" 
                             width="900" height="500" frameborder="0" allow="autoplay; fullscreen; picture-in-picture" allowfullscreen 
                             title="Morphospace_and_Warp_Grids.mp4"></iframe>')))
            
          ),
          wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                    fluidRow(column(9, offset = 1, h4("Part 4: Shape Patterns"))),hr(),
                    fluidRow(
                      column(
                        11, 
                        HTML('<iframe src="https://player.vimeo.com/video/530974819?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479" 
                             width="900" height="500" frameborder="0" allow="autoplay; fullscreen; picture-in-picture" allowfullscreen 
                             title="Shape_Patterns.mov"></iframe>'))), br(),
                    fluidRow(column(9, offset = 1, 
                                    downloadButton("export_demo_symmetry", label = "Export Demo Symmetry Files", 
                                                   style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))),
                    br(),
                    fluidRow(
                      column(9, offset = 1,
                             HTML('<h5>Updates since tutorial recording:</h5>
                   <li>Various bugs fixed</li>
                   <li>Certain options only available after Calculate is pressed</li>
                   <li>Clear Symmetry Settings button now available</li>')))
          ),
          wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                    fluidRow(column(9, offset = 1, h4("Part 5: Linear Models"))),hr(),
                    fluidRow(
                      column(
                        11,
                        HTML('<iframe src="https://player.vimeo.com/video/530980333?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479" 
                             width="900" height="500" frameborder="0" allow="autoplay; fullscreen; picture-in-picture" allowfullscreen 
                             title="Linear_Models.mov"></iframe>'))),
                    br(),
                    fluidRow(
                      column(9, offset = 1,
                             HTML('<h5>Updates since tutorial recording:</h5>
                   <li>Various bugs fixed</li>
                   <li>Convex Hulls available for Allometry Tab plot</li>')))
          ), 
          wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                    fluidRow(column(9, offset = 1, h4("Part 6: Extras"))),hr(),
                    fluidRow(
                      column(
                        11, 
                        HTML('<iframe src="https://player.vimeo.com/video/530960448?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479" 
                             width="900" height="500" frameborder="0" allow="autoplay; fullscreen; picture-in-picture" allowfullscreen 
                             title="Extras.mp4"></iframe>')))
          ), 
          br()
        ),
        tabPanel(
          "News", style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;", 
          br(), 
          wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                    fluidRow(column(9, offset = 1, h4("News"))), hr(),
                    fluidRow(column(11, uiOutput("news")))),
          wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                    fluidRow(column(9, offset = 1, h4("Upcoming Features"))),hr(),
                    fluidRow(column(11, uiOutput("upcoming_features")))),
          #wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
          #          fluidRow(column(9, offset = 1, h4("Server Capacity Limitations"))),hr(),
          #          fluidRow(column(11, uiOutput("server_capacity")))), # add this back in on next release
          wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                    fluidRow(column(9, offset = 1, h4("Contact the Developers"))),hr(),
                    fluidRow(column(11, 
                                    a("Report an issue with the App on Github", href = "https://github.com/geomorphR/gmShiny/issues/new", 
                                      style = "color:darkblue"))), br(),
                    fluidRow(column(11,
                                    a("Ask a question related to geomorph on the geomorph List Serve", href="https://groups.google.com/g/geomorph-r-package", 
                                      style = "color:darkblue"))), br(),
                    fluidRow(column(11, 
                                    a("Contact Dr. Erica Baken via email", href="mailto:gmShiny.baken@gmail.com",
                                      style = "color:darkblue")
                    )))
        ),
        tabPanel(
          "Citation Information",
          br(),
          wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                    fluidRow(column(11, offset = .5, textOutput("citation_info"))),
                    hr(),
                    fluidRow(column(11, offset = .5, textOutput("github_link"))),
          )
        )
      )), 
    tags$style(type = "text/css", "a{color: #ceecf0;}"),
    tags$head(tags$style(".toprow{height:28px; margin-top: -7px;}")),
    tags$script('Shiny.addCustomMessageHandler("scrollCallback_outlier",
                function(color) {
                   var objDiv = document.getElementById("scroll_outlier_selected");
                   $("#" + "scroll_outlier_selected").animate({scrollTop: objDiv.scrollHeight - objDiv.clientHeight}, 500);
                });'
    ),
    tags$script('Shiny.addCustomMessageHandler("scrollCallback_warp",
                function(color) {
                   var objDiv = document.getElementById("scroll_warp_initiated");
                   $("#" + "scroll_warp_initiated").animate({scrollTop: objDiv.scrollHeight - objDiv.clientHeight}, 500);
                });'
    ),
    useShinyjs(), 
    useShinyalert(), 
    setBackgroundColor(color = c("#ceecf0", "#3579b5"), gradient = "linear", direction = "top")
  )
  
)
