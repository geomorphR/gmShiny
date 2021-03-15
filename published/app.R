#remotes:::install_github("geomorphR/geomorph", ref = "Develop", build_vignettes =F)
library(shiny); library(shinyjs); library(shinyWidgets); library(shinydashboard); library(shinythemes) 
library(shinyalert); library(shinyMatrix); library(shinyjqui)
library(geomorph); library(ape); library(stringr); library(stringi); library(RColorBrewer); library(reactlog)

# elements with !!! in the comments indicate changes that should be made in the future
# elements with ??? in the comments indicate functionalities that have not been fully verified or need more rigorous testing

# use browser() like you would breakpoint but more targeted, can be hidden by a conditional
# runApp(...., display.mode="showcase")
# options(shiny.error = browser)
# or options(shiny.error = ___any function you want___)

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
colnames(symmetry_manual_matrix) <- c("Specimen", "Individual", "Replicate", "Side")
color.ramp.options <- rbind(c('yellow', 'red'), c('blue', 'green'), c('pink', 'purple'))
color.palette.options <- c("Dark2", "Accent", "PuBuGn")
newmat <- NULL
old_independent_vars <- NULL
drop_these <- NULL
options(shiny.maxRequestSize = 30*512^2)

options(shiny.reactlog = TRUE) # this allows for the reaction log to be generated (hit command + fn + f3 to see it)

ui <- function(request) {
  navbarPage(
    title = "Geomorph Shiny App",
    id = "navbar",
    theme = shinytheme("flatly"),
    footer = div(style = "position: fixed; bottom:0; padding: 12px; height: 50px;
                        width:100%; background-color: rgba(44, 62, 80, 0.75); color: #ceecf0;",
                 fluidRow(column(6, align = "left", 
                                 prettyToggle("alert_on_off", 
                                              label_on = "Instructions On", label_off = "Instructions Off",
                                              value = F, shape = "curve", icon_on = icon("check"),icon_off = icon("remove"),
                                              status_on = "info", status_off = "warning",  inline = T)),
                          #actionButton("alert_show_here", label = "Show Instructions for This Page", style='padding-top:3px; padding-bottom:3px; font-size:85%')),
                          column(6, align = "right", bookmarkButton(style='padding-top:3px; padding-bottom:3px; font-size:85%')))),
    tabPanel(
      "Data Input",
      fluidRow(class = "toprow",
               column(width = 6), 
               column(width = 2, 
                      align = "right", conditionalPanel(condition = "output.datasets_dont_match", 
                                                        actionButton(inputId = "go_pruning", label = "Prune Datasets To Match",  width = '100%', style='padding:6px; font-size:85%'))),  # this button shows up when the shape, phy, and/or trait files do not contain exactly the same species in the same order. Clicking the button trims all uploaded datasets to just the species/specimen names that match
               column(width = 2, 
                      align = "right", conditionalPanel(condition = "output.example_tps_selected || output.file_tps_selected || 
                                                 output.file_phy_selected || output.file_trait_selected", 
                                                        actionButton(inputId = "go_file_reset", label = "Clear All Inputs",  width = '100%', style='padding:6px; font-size:85%'))), # this button tries to reset all the inputs, basically resetting the app (??? not perfect) # icon = icon("cocktail"), class = "btn-warning")),
               column(width = 2, 
                      align = "right", actionButton(inputId = "go_example", label = "Use Example Plethodon Data", width = '100%', style='padding:6px; font-size:85%')) # this button activates the plethodon example dataset # icon = icon("dragon"), class = "btn-block btn-success"),
      ), 
      hr(),
      fluidRow(
        column(width = 4, 
               fileInput(inputId = "file_tps", label = "Choose Shape File", multiple = FALSE,  width = "auto",
                         accept = c("text/tps",".tps",  "text/shapes", ".shapes", "text/nts",".nts"), 
                         placeholder = "No shape file selected"), # upload tps files here
               conditionalPanel(
                 condition = "output.file_tps_selected", hr(), # this panel shows up once a tps file has been selected
                 div(style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 280px); position:relative;", 
                     fluidRow(
                       column(4, radioButtons(inputId = "raw_lms_already_aligned", "Specimens Are:",
                                              choices = c("Not Yet Aligned" = F, "Already Aligned" = T),
                                              selected = F, width = "100%")),
                       column(4,radioButtons(inputId = "spec_id", label = "Extract ID From:", # option to specify how the specimens are labeled
                                             choices = c("ID line" = "ID", "IMAGE line" = "imageID", "Assign New Names" = "None"),
                                             selected = "ID", width = "100%")),
                       column(4, radioButtons(inputId = "neg_lms", "Negative LMs Are:", # option to indicate whether negative LMs should be taken as missing data or as real negative values
                                              choices = c("True LMs" = F, "Missing Data" = T), 
                                              selected = F, width = "100%"))),  br(),
                     verbatimTextOutput("shape_file"), br(), br()))), # this displays a preview of the input tps file, mostly for spot checking and general confirmation that the intended file was uploaded
        column(width = 4, 
               fileInput(inputId = "file_phy", label = "Choose Phylogeny File", multiple = FALSE, width = "auto", # input phylogeny file. at the moment, only .tre files have been tested or coded for.
                         accept = c("text/tre",".tre", ".nexus", ".nex", "text/nex"), placeholder = "No tree file selected"),
               conditionalPanel(
                 condition = "output.file_phy_selected", hr(),
                 div(style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 280px); position:relative;",
                     checkboxInput(inputId = "show_tip_label_phy_preview", label = "Display Tip Labels", TRUE),
                     plotOutput(height = "auto", "phylogeny")))), #this displays the phylogeny uploaded, again for spot checking.
        column(width = 4,
               fileInput(inputId = "file_trait", label = "Choose Trait File", multiple = FALSE, width = "auto", # input trait file. at the moment, only .csv files are allowed
                         accept = c("text/csv",".csv", "text/xls", ".xls", "text/xlsx", ".xlsx"), placeholder = "No trait file selected"),
               conditionalPanel(
                 condition = "output.file_trait_selected", 
                 hr(style = "margin-top: 10px; margin-bottom: 10px; padding: 0px;"),
                 div(style = "overflow: hidden; overflow-y:scroll; max-height: calc(100vh - 280px); position:relative; overflow-x: hidden;", 
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
                        fluidRow(align= "center",
                                 downloadButton("export_plot_all_specimens", label = "Export Plot", 
                                                style='width: 200px; padding:6px; font-size:80%; 
                             background-color: #337ab7; border-color: #337ab7;'))), br(), br()), # clicks and double clicks are for drawing the links between landmarks. the brush is for selecting semilandmarks
            sidebarPanel(
              width = 3,  style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 150px); position:relative;",
              hr(style="border-color: purple;"),
              fluidRow(
                align = "center", h4(strong("Settings"))), 
              hr(style="border-color: purple;"),
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
              fluidRow(
                align = "center",
                column(12,
                       fileInput("semilms_upload_file_input", "Upload Semilandmark Matrix", accept = c("text/csv",".csv", "text/xls", ".xls", "text/xlsx", ".xlsx"),
                                 placeholder = "Select a CSV or excel file"))),
              fluidRow(column(12,h5(strong("Color Options:")))),
              fluidRow(div(style = "margin: 0px; padding: 0px; vertical-align: bottom;",
                           column(2, h6("SemiLMs")),
                           column(2, h6("Brackets")),
                           column(2, h6("Other LMS")),
                           column(2, h6("Individ")),
                           column(2, h6("Labels")),
                           column(2, h6("Links")))
              ),
              fluidRow(div(style = "margin: 0px; padding: 0px; vertical-align: bottom;",
                           column(2, colorSelectorDrop.ekb("semilms_color", "Semilandmarks", selected = all_color_options[1])),
                           column(2, colorSelectorDrop.ekb("semilms_color_brackets", "Bracketing Landmarks", selected = all_color_options[4])),
                           column(2, colorSelectorDrop.ekb("semilms_color_other", "Other Landmarks")),
                           column(2, colorSelectorDrop.ekb("semilms_color_individlms", "Individual Specimen Landmarks", selected = all_color_options[58], dropdownside = "right")),
                           column(2, colorSelectorDrop.ekb("semilms_color_labels", "Labels", selected = all_color_options[1], dropdownside = "right")),
                           column(2, colorSelectorDrop.ekb("semilms_color_links", "Links", dropdownside = "right"))
              )), br(), br(),
              conditionalPanel(
                condition = "output.semis_initiated",
                fluidRow(
                  align = "center",
                  actionButton(inputId = "semilms_reset", label = "Reset Semilandmark Selection",  
                               width = 200, style='padding:6px; font-size:80%'), br(), br())), # resetting the semi landmark selections ??? does this reset completely and rerun if we selected some sliders then reset them, and did it again
              conditionalPanel(
                condition = "output.links_dbclick_initiated", 
                fluidRow(
                  align = "center",
                  actionButton(inputId = "link_reset", label = "Reset Landmark Links", 
                               width = 200, style='padding:6px; font-size:80%')), br())))), # resetting the lines between the landmarks
        tabPanel(
          title = "Visualize Outliers and Individual Specimens", # plotoutiers 
          id = "tab_dataprep_visualizeoutliersandindividualspecimens",
          sidebarLayout(
            
            mainPanel(
              id = "scroll_outlier_selected",
              width = 9, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 130px); position:relative;", 
              br(), 
              wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;", 
                        plotOutput("outliers", height = 700, click = "outlier_selected"), # click is for selecting and visualizing each specimen
                        fluidRow(
                          align = "center", 
                          downloadButton("export_visualize_outliers_all", "Export Visualize Outliers Plot", 
                                         style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')), 
                        br()),
              
              conditionalPanel("output.outlier_selected", 
                               wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                                         
                                         plotOutput("outlier_selected_lms", height = 700), 
                                         fluidRow(
                                           align = "center",
                                           downloadButton("export_individual_specimen_shape", 
                                                          "Export Selected Individual Plot", 
                                                          style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'), br())
                               )), br(), br()),
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
                                            column(3, align = "center", h6("SemiLMs")),
                                            column(3, align = "center", h6("LMs")),
                                            column(3, align = "center", h6("Labels")),
                                            column(3, align = "center", h6("Links")))
                               ),
                               fluidRow(div(style = "margin: 0px; padding: 0px; vertical-align: bottom;",
                                            column(3, align = "center", colorSelectorDrop.ekb("vis_outliers_color", "Semilandmarks", selected = all_color_options[1])),
                                            column(3, align = "center", colorSelectorDrop.ekb("vis_outliers_color_other", "Other Landmarks")),
                                            column(3, align = "center", colorSelectorDrop.ekb("vis_outliers_color_labels", "Labels", selected = all_color_options[1], dropdownside = "right")),
                                            column(3, align = "center", colorSelectorDrop.ekb("vis_outliers_color_links", "Links", dropdownside = "right"))
                               ))), 
              br()
            ))),
        tabPanel(
          "Generalized Procrustes Alignment",
          id = "tab_dataprep_generalizedprocrustesalignment",
          br(), 
          fluidRow(
            column(4, wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250,.25);
                                overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 200px); position:relative;", 
                                h4("Raw Imported LMs"),
                                verbatimTextOutput("raw_lms_rx"))),
            column(4, align = "center",
                   wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250,.25);
                                overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 200px); position:relative;", 
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
                               tableOutput("semilandmark_matrix")))),
            column(4,
                   wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250,.25);
                                overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 200px); position:relative;", 
                             actionButton(inputId = "go_run_gpa", label = "Run GPA",  
                                          style='width: 100%; height: 42px; padding:6px; 
                                             font-size:120%; background-color: #003366; border-color: #003366;'),
                             conditionalPanel(
                               condition = "output.show_gpa", 
                               br(),
                               fluidRow(
                                 column(6, align = "left", h4("Aligned LMs")),
                                 column(6, align = "right", 
                                        downloadButton('export_aligned_lms', 'Export Aligned LM File', 
                                                       style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))), 
                               br(), 
                               verbatimTextOutput("gpa_aligned_rx")
                             )) # 
            )))
      )), 
    tabPanel(
      "Morphospace and Warp Grids", # why are the conditional panels not working on this tab panel? ???
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
                                                choices = c("PCA" = FALSE, "PACA" = TRUE), 
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
            condition = "input.tip_col_category == 'by_trait_1' || input.tip_col_category == 'by_trait_2' || input.tip_col_category == 'by_trait_3' || input.show_convex_hull_1 || input.show_convex_hull_2 || input.show_convex_hull_3", 
            fluidRow(
              column(1, colorSelectorDrop.ekb(inputId = "trait_colors_lev1", label = NULL, selected = all_color_options[1])),
              column(1, colorSelectorDrop.ekb(inputId = "trait_colors_lev2", label = NULL, selected = all_color_options[4])),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 2", colorSelectorDrop.ekb(inputId = "trait_colors_lev3", label = NULL, selected = all_color_options[7]))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 3", colorSelectorDrop.ekb(inputId = "trait_colors_lev4", label = NULL, selected = all_color_options[10]))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 4", colorSelectorDrop.ekb(inputId = "trait_colors_lev5", label = NULL, selected = all_color_options[13]))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 5", colorSelectorDrop.ekb(inputId = "trait_colors_lev6", label = NULL, selected = all_color_options[54]))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 6", colorSelectorDrop.ekb(inputId = "trait_colors_lev7", label = NULL, selected = all_color_options[50], dropdownside = "right"))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 7", colorSelectorDrop.ekb(inputId = "trait_colors_lev8", label = NULL, selected = all_color_options[44], dropdownside = "right"))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 8", colorSelectorDrop.ekb(inputId = "trait_colors_lev9", label = NULL, selected = all_color_options[63], dropdownside = "right"))),
              column(1, conditionalPanel(condition = "output.col_n_levels  > 9", colorSelectorDrop.ekb(inputId = "trait_colors_lev10", label = NULL, selected = all_color_options[39], dropdownside = "right"))), 
              column(1, conditionalPanel(condition = "output.col_n_levels  > 10", colorSelectorDrop.ekb(inputId = "trait_colors_lev11", label = NULL, selected = all_color_options[31], dropdownside = "right"))), br(), br())), 
          
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
                        column(width = 4, align = "center", style = "vertical-align: bottom;", p("Ref Links:"))),
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
                    fluidRow(align = "center", 
                             downloadButton("export_morphospace", "Export Morphospace Plot", 
                                            style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7;  border-color: #337ab7;')), br()), 
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
                        column(12, 
                               align = "center",
                               downloadButton("export_warp_grid", "Export Warp Grid", 
                                              style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7;  border-color: #337ab7;'))), 
                      br()), br())
        ))),
    tabPanel(
      "Shape Patterns",
      tabsetPanel(
        id = "tab_shapepatterns",
        tabPanel(
          "Phylogenetic Signal",
          sidebarLayout(
            mainPanel(
              width = 9, br(),
              wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                        fluidRow(
                          column(7, plotOutput(
                            "phy_signal_plot", width = "100%", height = 500)),
                          column(5, verbatimTextOutput(
                            "phy_signal_results"
                          ))),
                        fluidRow(
                          column(7, align = 'center',
                                 downloadButton('export_phy_signal_plot', 'Export Signal Plot', 
                                                style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))#,
                          #column(5, align = 'center',
                          #       downloadButton('export_phy_signal_results', 'Export Signal Results', 
                          #                      style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
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
            ))),
        tabPanel(
          "Modularity",
          sidebarLayout(
            mainPanel(width = 9, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;", 
                      br(),
                      wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                                fluidRow(column(9, offset = 1, h4("Module Visualization"))), hr(),
                                fluidRow(column(12, plotOutput("plot_modularity_visualization", width = "100%", height = 500))),
                                fluidRow(
                                  column(12, align = "center", 
                                         downloadButton(
                                           "export_modularity_visualization_plot", "Export Modularity Visualization", 
                                           style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')))),
                      br(),
                      wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                                fluidRow(column(9, offset = 1, h4("Degree of Modularity"))), hr(),
                                fluidRow(
                                  column(6, plotOutput("plot_modularity_test", width = "100%", height = 400)),
                                  column(6, verbatimTextOutput("stats_modularity_test"))),
                                fluidRow(
                                  column(6, align = "center", downloadButton("export_modularity_test_plot", "Export Modularity Plot", 
                                                                             style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))#,
                                  #column(6, downloadButton("export_modularity_test_table", "Export Modularity Table", 
                                  #                          style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
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
                                    column(6, align = "center", downloadButton("export_compare_multi_evol_rates_plot", "Export Rate Plot", 
                                                                               style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                                    #column(6, downloadButton("export_compare_multi_evol_rates_table", "Export Modular Evolutionary Rate Table", 
                                    #                          style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
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
              fluidRow(column(12, align = "center", actionButton("apply_modular_groups_go", label = "Assign LMs to Modules", width = "67%", style='padding:6px; font-size:85%'))),
              br(),
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
                        fluidRow(column(12, align = 'center',
                                        downloadButton('export_global_integration_plot', 'Export Global Integration Plot', 
                                                       style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')))),
              br(), 
              wellPanel(style = "align: center; border-color: white; background-color: rgba(255,250,250, .25) ;",
                        fluidRow(column(9, offset = 1, h4("Integration Between Modules (defined in 'Modularity' tab)"))), hr(),
                        fluidRow(
                          column(6, plotOutput("integration_test_plot", width = "100%", height = 400)),
                          column(6, fluidRow(verbatimTextOutput("integration_test_results")))),
                        fluidRow(
                          column(6, align = 'center',
                                 downloadButton('export_integration_test_plot', 'Export Integration Test Plot', 
                                                style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))), 
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
                          em(h5("Define symmetry analysis parameters in 'Settings', then press 'Apply Symmetry Analyses'."))
                        ),
                        conditionalPanel(
                          "output.symmetry_initiated",
                          fluidRow(
                            column(6, plotOutput("symmetry_plot", width = "100%", height = 700), br()),
                            column(6, verbatimTextOutput("symmetry_results"))),
                          fluidRow(
                            column(6, align = 'center',
                                   downloadButton('export_symmetry_plot', 'Export Symmetry Plot', 
                                                  style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')))), 
                        br()), br(), br()),
            sidebarPanel(
              width = 3, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 150px); position:relative;", 
              hr(style="border-color: purple;"),
              fluidRow(align = "center", h4(strong("Settings"))), 
              hr(style="border-color: purple;"),
              fluidRow(column(12, align = "center", 
                              radioButtons("symmetry_obj_sym", "Symmetry Type:", choices = c("Object" = T, "Matching" = F), inline = T))),
              h5("Assign Individuals, Replicates, and Sides Using:"),
              fluidRow(column(4, align = "center", actionButton("go_symmetry_file", "File Upload", width = "100%", style='padding:6px; margin:0px;font-size:80%')),
                       column(4, align = "center", actionButton("go_symmetry_pattern", "Patterning", width = "100%", style='padding:6px; margin:0px;font-size:80%')),
                       column(4, align = "center", actionButton("go_symmetry_manual", "Manual Entry", width = "100%", style='padding:6px;margin:0px; font-size:80%'))), br(),
              conditionalPanel(
                "output.symmetry_pattern",
                radioButtons('symmetry_ind_pattern', "Individuals Are:",  
                             choices = c("Not Repeated" = "no_repeats",
                                         "Grouped Every nth Specimen" = "nth", 
                                         "Grouped in Blocks" = "blocks"),
                             selected = "no_repeats"),
                #radioButtons('symmetry_replicate_pattern', "Group Replicates:",  
                #             choices = c("No Replicates" = "none",
                #                         "Every nth Specimen" = "nth", 
                #                         "In Blocks" = "blocks"),
                #             selected = character(0)),
                conditionalPanel(
                  "input.symmetry_obj_sym == 'TRUE'", 
                  radioButtons('symmetry_land_pairs_pattern', "Group Landmarks:",  
                               choices = c("Every Other" = "every_other",
                                           "In Blocks" = "blocks", 
                                           "With File Upload" = "file"),
                               selected = character(0))
                ),
                conditionalPanel(
                  "input.symmetry_obj_sym == 'FALSE'",
                  radioButtons('symmetry_side_pattern', "Group Sides:",  
                               choices = c("Every Other" = "every_other",
                                           "In Blocks" = "blocks"),
                               selected = character(0))
                )),
              conditionalPanel(
                "output.show_symmetry_file",
                fileInput("symmetry_file_upload",label = "Choose File Defining Individuals, Replicates, and Sides:", 
                          multiple = FALSE, width = "auto", # input trait file. at the moment, only .csv files are allowed
                          accept = c("text/csv",".csv", "text/xls", ".xls", "text/xlsx", ".xlsx"), 
                          placeholder = "No file selected")),
              conditionalPanel(
                "output.symmetry_land_pairs_file_input",
                fileInput("symmetry_land_pairs_file_upload",label = "Choose File Defining Landmark Side Assignment:", 
                          multiple = FALSE, width = "auto", # input trait file. at the moment, only .csv files are allowed
                          accept = c("text/csv",".csv", "text/xls", ".xls", "text/xlsx", ".xlsx"), 
                          placeholder = "No file selected")
              ),
              conditionalPanel(
                "output.show_symmetry_definitions",
                div(style = "font-size:80%; align: center;", #text-align: center;
                    matrixInput(
                      inputId = "symmetry_definitions", label = "Specimen Assignments",
                      value = symmetry_manual_matrix, class = "character",
                      cols = list(names = T), rows = list(names = T, extend = T)))),
              numericInput(inputId = "symmetry_perm", label = "Number of Permutations:", 
                           value = 999, min = 0, max = 99999, step = 100),
              fluidRow(column(12, align = "center", actionButton("run_symmetry_go", "Apply Symmetry Analyses" ))), br(),
              fluidRow(column(12, align = "center", actionButton("go_symmetry_useoutput", "Use Symmetric Component of Shape Variation", style='font-size:80%;'))), br(),
            )))
      )),
    tabPanel(
      "Linear Models",
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
                  column(6, downloadButton('export_aov_table', 'Export ANOVA Results', 
                                           style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                  conditionalPanel(
                    condition = "!(input.disparity_groups == 'no_run')", 
                    column(3, downloadButton('export_proc_variances', 'Export Disparity Results', 
                                             style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))),
                  conditionalPanel(
                    condition = "!(input.evol_rate_groups == 'no_run')", 
                    column(3, downloadButton('export_evol_rates', 'Export Rates Results', 
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
                            column(6, 
                                   downloadButton("export_pairwise_table", "Export Pairwise ANOVA Results", 
                                                  style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;')),
                            conditionalPanel(
                              condition = "!(input.disparity_groups == 'no_run')", 
                              column(3, downloadButton("export_disparity_pairwise_table", "Export Pairwise Disparity Results", 
                                                       style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))),
                            conditionalPanel(
                              condition = "!(input.evol_rate_groups == 'no_run')", 
                              column(3, 
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
              actionButton("go_run_anova", "Calculate", width = '100%', style='padding:6px; font-size:85%'), br(), br()
            ))),
        tabPanel(
          "Allometry",
          sidebarLayout(
            mainPanel(width = 9, style = "overflow: hidden; overflow-y: scroll; max-height: calc(100vh - 140px); position:relative;",
                      br(),
                      fluidRow(column(9, offset = 1, textOutput("model_tested_2", container = h5))),
                      hr(),
                      plotOutput("allometry_plot", width = "100%", height = 600), br(),
                      fluidRow(
                        column(12, align = 'center',
                               downloadButton('export_allometry_plot', 'Export Allometry Plot', 
                                              style='width: 200px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))
                      )),
            sidebarPanel(width = 3, 
                         hr(style="border-color: purple;"),
                         fluidRow(align = "center", h4(strong("Settings"))), 
                         hr(style="border-color: purple;"),
                         radioButtons("allometry_type", "Allometry Type:", choices = c("Regression" = "regression")),
                         radioButtons("allometry_reg_type", "Regression Type:", 
                                      choices = c("Prediction Line" = "PredLine", "Regression Score" = "RegScore")),
                         radioButtons("allometry_predictor", "Predictor Variable:", choices = c("Upload Trait Data" = "none")),
                         conditionalPanel(
                           "input.allometry_predictor == 'csize'",
                           radioButtons("allometry_log_csize", "Log Transform Centroid Size", choices = c(TRUE, FALSE))),
                         selectInput("allometry_color", "Color Points:", choices = c("Upload Trait Data" = "none")),
                         sliderInput("allometry_pt_size", "Point Size:", min = 0.1, max = 5, value = 1)
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
                fluidRow(column(9, align = "center", downloadButton('export_model_comparison', 'Export Model Comparison Results', 
                                                                    style='width: 300px; padding:6px; font-size:80%; background-color: #337ab7; border-color: #337ab7;'))),)),
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
              
              fluidRow(column(12, align="center",actionButton("go_run_model_comparison", "Calculate", width = '100%', style='padding:6px; font-size:85%'))), br()
            )
          )),
        
        tabPanel(
          "Trajectory",
          sidebarLayout(
            mainPanel(width = 9, 
                      br(),
                      fluidRow(column(9, offset = 1, h5("Model"))),
                      hr(), 
                      fluidRow(column(7, plotOutput("trajectory_plot", width = "100%", height = 600)),
                               column(5, verbatimTextOutput("trajectory_results")))
            ),
            sidebarPanel(width = 3, 
                         hr(style="border-color: purple;"),
                         fluidRow(align = "center", h4(strong("Settings"))), 
                         hr(style="border-color: purple;"),
                         radioButtons("trajectory_group", "Group", choices = c("Upload Trait Data" = "none")),
                         radioButtons("trajectory_trait", "Trait", choices = c("Upload Trait Data" = "none")))
          ))
        
      )),
    tabPanel("Example Tutorial",  fluidRow(h3(strong("Tutorial"))), 
             fluidRow(h4("Part 1: Data Input Tab")),
             #fluidRow(HTML('<iframe width="900" height="500" src="https://iastate.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=822b0e7b-cba8-4643-b407-ac3d01546f19" frameborder="0" allow="accelerometer; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
             fluidRow(h4("Part 2: Links and SemiLandmarks")), 
             #fluidRow(HTML('<iframe width="900" height="500" src="https://iastate.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=e78d9298-1ba8-416a-a2a1-ac3d0154855e" frameborder="0" allow="accelerometer; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
             fluidRow(h4("Part 3: Visualize Outliers")), 
             #fluidRow(HTML('<iframe width="900" height="500" src="https://iastate.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=aadff764-3960-4a35-86ad-ac3d01588c71" frameborder="0" allow="accelerometer; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
             fluidRow(h4("Part 4: Morphospace Tab")), 
             #fluidRow(HTML('<iframe width="900" height="500" src="https://iastate.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=c5ec117a-b048-4835-87fe-ac3d01595d05" frameborder="0" allow="accelerometer; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
             fluidRow(h4("Part 5: Morphospace Tab Continued")), 
             #fluidRow(HTML('<iframe width="900" height="500" src="https://iastate.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=f4556ad4-d5e0-4313-a06e-ac3d015c4db5" frameborder="0" allow="accelerometer; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
             fluidRow(h4("Part 6: Statistical Analyses Tab")), 
             #fluidRow(HTML('<iframe width="900" height="500" src="https://iastate.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=cc5483f4-2adb-4c3e-9a46-ac40016a62dd" frameborder="0" allow="accelerometer; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
             fluidRow(h4("Part 7: Real Data Example"))
             #fluidRow(HTML('<iframe width="900" height="500" src="https://iastate.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=3cd14240-ce64-4475-9b59-ac40016c1ae3" frameborder="0" allow="accelerometer; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
    ), 
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
} 

server <- function(input, output, session) {

  #### Instructions ####
  tryObserveEvent(input$navbar, {
    if(is.null(bookmark_vals$adjust_updates) & is.null(alert_vals$navbar_datainput_welcomealertdone)) { # silences this alert if coming from a bookmarked state or if this already ran once
      shinyalert(
        title = "Welcome!",
        inputId = "keep_alerts_on_tf",
        text = "Alerts like this are available throughout this App to help explain all the functionalities available to you. 
        
        Would you like to keep these Instructions on?",
        size = "s", 
        closeOnEsc = F,
        closeOnClickOutside = F,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "Yes",
        confirmButtonCol = "#AEDEF4",
        cancelButtonText = "No",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      alert_vals$navbar_datainput_welcomealertdone <- "yes"
    }
  }
  )
  
  tryObserveEvent(input$alert_on_off, ignoreInit = T, priority = -10,  {
    if(input$alert_on_off == F) { 
      shinyalert(
        title = "Instructions Have Been Turned Off",
        text = "If at any time you would like to turn the instructions back on, 
       or if you'd like to view the instructions for a particular page, you can do
        so using the 'Instructions' toggle in the bottom left-hand corner, currently 
       labeled 'Instructions Off'.",
        html = T,
        closeOnClickOutside = T,
        closeOnEsc = T,
        size = "s"
      )
    }
  })
  
  
  tryObserveEvent(eventExpr = input$keep_alerts_on_tf, ignoreInit = T, priority = 10,  {
    updatePrettyToggle(session, inputId = "alert_on_off", 
                       value = as.logical(input$keep_alerts_on_tf)) # doesnt need a bookmarking workaround
    
    if(input$keep_alerts_on_tf == F) { 
      shinyalert(
        title = "Instructions Have Been Turned Off",
        text = "If at any time you would like to turn the instructions back on, 
        or if you'd like to view the instructions for a particular page, you can do
         so using the 'Instructions' toggle in the bottom left-hand corner, currently 
        labeled 'Instructions Off'. 
        <br><br>
        If you'd like to save your progress in the App at any point,
                you can do so using the 'Bookmark...' button in the bottom right-hand corner. 
                This will provide you a link, with which you can return to that point in the App in a new session.
                This link can also be shared among users.",
        html = T,
        closeOnClickOutside = T,
        closeOnEsc = T,
        size = "m"
      )
    }
    
  })
  
  alert_vals <- reactiveValues() 
  bookmark_vals <- reactiveValues()
  
  
  tryObserveEvent(eventExpr = input$alert_on_off, {
    if(input$alert_on_off == FALSE) { # IF YOU ADD ANY HERE, ADD THEM TO BOOKMARKING THING
      alert_vals$navbar_datainput <- NULL
      alert_vals$navbar_datainput_examplealertdone <- NULL
      alert_vals$navbar_datainput_filetpsalertdone <- NULL
      alert_vals$navbar_datainput_filetraitalertdone <- NULL
      alert_vals$navbar_datainput_dontmatchalertdone <- NULL
      alert_vals$navbar_dataprep <- NULL
      alert_vals$navbar_dataprep_links <- NULL
      alert_vals$navbar_dataprep_outliers <- NULL
      alert_vals$navbar_dataprep_gpa <- NULL
      alert_vals$navbar_dataprep_links_semialertdone <- NULL
      alert_vals$navbar_dataprep_links_linkalertdone <- NULL
      alert_vals$navbar_shapepatterns <- NULL
      alert_vals$navbar_shapepatterns_signal <- NULL
      alert_vals$navbar_shapepatterns_modularity <- NULL
      alert_vals$navbar_shapepatterns_integration <- NULL
      alert_vals$navbar_morphospaceplot <- NULL
      alert_vals$navbar_morphospaceplot_tipcol <- NULL
      alert_vals$navbar_morphospaceplot_tipcolalertdone <- NULL
      alert_vals$navbar_morphospaceplot_tippch <- NULL
      alert_vals$navbar_morphospaceplot_tippchalertdone <- NULL
      alert_vals$navbar_morphospaceplot_phylo <- NULL
      alert_vals$navbar_morphospaceplot_warp <- NULL
      alert_vals$navbar_morphospaceplot_warpalertdone <- NULL
      alert_vals$navbar_linearmodels <- NULL
      alert_vals$navbar_linearmodels_modeldesign <- NULL
      alert_vals$navbar_linearmodels_modeldesignalertdone <- NULL
      alert_vals$navbar_linearmodels_allometry <- NULL
      alert_vals$navbar_linearmodels_allometryalertdone <- NULL
      alert_vals$navbar_linearmodels_modelcomparison <- NULL
      alert_vals$navbar_linearmodels_modelcomparisonalertdone <- NULL
      
    }
  }) # update to include all relevant alert_vals 
  
  tryObserveEvent(eventExpr = input$alert_on_off, ignoreInit = T, {
    if(input$alert_on_off == TRUE) {
      if(req(input$navbar) == "Data Input") {
        shinyalert(
          title = "Getting Started",
          text = "To practice using this App, you can load the 
        salamander head example dataset from geomorph (<span style='font-family: Courier New'>plethspecies</span>) by
        pressing the 'Use Example Plethodon Data' button in the top right corner.
          <br></br>
          Otherwise, upload your own shape, phylogenetic, and/or trait data using the appropriate 'Browse' buttons.
          <br><br>
          If at any point you'd like to save your progress, you can do so by pressing the 'Bookmark...' button in the bottom
          right-hand corner. This will provide you a link, with which you can return to that point in the App in a new session.
                This link can also be shared among users.",
          size = "m",
          html = T
        )
      }
    }
  })
  
  tryObserveEvent(eventExpr = vals$go_example_1, ignoreInit = T, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_datainput_examplealertdone)) {
      delay(1000,
            if(!is.null(vals$go_example_1)){
              shinyalert(
                title = "Data Input",
                text = "Now that you've loaded the example dataset, you can see some of the options available on this page.
          <br></br>
          Previews of each dataset are made visible to confirm the accuracy of the upload. 
          A button appears in the top right corner ('Clear All Inputs') that can clear the uploaded data if you'd like to choose different datasets.
           <br></br>
           In the left-most column, you will see options for how to upload, in this case, a TPS shape file. This is where 
           you can specify whether the shape data have already been aligned or not, from where to extract the specimen IDs, and whether negative landmarks 
           should be read in as missing data. 
           <br><br>
           The center column displays the uploaded phylogeny.<br><br>
           In the right-most column, you can select which columns of your uploaded trait file you wish to use downstream. 
          <em>This functionality is limited to 3 columns of data at a time</em>. 
          
          As you select the columns of interest,
          you can also specify whether each trait is discrete or continuous. If you select a continuous trait, you can apply a transformation 
          to the data before moving forward. 
          <br></br>
          When you're happy with your data input, move on to the Data Prep page.",
                size = "m",
                html = T
              )
            }
      )
      alert_vals$navbar_datainput_examplealertdone <- "yes"
    }
  })
  
  tryObserveEvent(eventExpr = input$file_tps, ignoreInit = T, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_datainput_filetpsalertdone)) {
      delay(1000,
            if(is.null(vals$go_example_1)){
              shinyalert(
                title = "You've uploaded shape data!",
                text = "In the left-most column, you can specify the state of your shape data and options for 
                how the file should be read. These correspond
          to the arguments in the <span style='font-family: Courier New'>readland</span> functions 
          (e.g., <span style='font-family: Courier New'>readland.tps</span>). 
          <br></br>
                The preview of the shape data allows you to double check that the landmarks loaded appropriately. 
                Semilandmarks can be defined and the Generalized Procrustes Alignment can
                be run on the Data Prep page, but we recommend uploading any phylogenetic and/or trait data before moving forward.
                ",
                size = "m",
                type = "success",
                html = T
              )
            }
      )
      alert_vals$navbar_datainput_filetpsalertdone <- "yes"
    }
  })
  
  tryObserveEvent(eventExpr = input$file_trait, ignoreInit = T, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_datainput_filetraitalertdone)) {
      delay(1000,
            if(!is.null(vals$go_example_1)){
              shinyalert(
                title = "You've uploaded trait data!",
                text = "The format of this file must have the names of the specimens/species as column 1 and 
          the data must be in wide-format (1 trait per column).
          Make sure your file follows this format before moving forward.
          <br></br>
          In the right-most section, under the Choose Trait File option,
          you can select which columns of your uploaded trait file you wish to use downstream. 
          <em>This functionality is limited to the use of up to 3 columns at a time</em>. 
          <br></br>
          As you select the columns of interest,
          you can also specify whether each trait is discrete or continuous. If you select a continuous trait, you can apply a transformation 
          to the data before moving forward. 
          <br></br>
          Upload all shape and phylogenetic data before moving on to the Data Prep page.
                ",
                size = "m",
                type = "success",
                html = T
              )
              alert_vals$navbar_datainput_filetraitalertdone <- "yes"
            }
      )
      
    }
  })
  
  tryObserve({ # navigation reactive values
    if(input$alert_on_off == TRUE) {
      if(req(input$navbar) == "Data Input"){
        alert_vals$navbar_datainput <- "Here"
      }
      if(req(input$navbar) == "Data Prep"){
        alert_vals$navbar_dataprep <- "Here"
        if(req(input$tab_dataprep) == "Define Links and Semi-Landmarks"){
          alert_vals$navbar_dataprep_links <- "Here"
        }
        if(req(input$tab_dataprep) == "Visualize Outliers and Individual Specimens"){
          alert_vals$navbar_dataprep_outliers  <- "Here"
        }
        if(req(input$tab_dataprep) == "Generalized Procrustes Alignment"){
          alert_vals$navbar_dataprep_gpa  <- "Here" 
        }
      }
      
      if(req(input$navbar) == "Shape Patterns"){
        alert_vals$navbar_shapepatterns <- "Here" 
        if(req(input$tab_shapepatterns) == "Phylogenetic Signal"){
          alert_vals$navbar_shapepatterns_signal  <- "Here" 
        }
        if(req(input$tab_shapepatterns) == "Modularity"){
          alert_vals$navbar_shapepatterns_modularity  <- "Here" 
        }
        if(req(input$tab_shapepatterns) == "Integration"){
          alert_vals$navbar_shapepatterns_integration  <- "Here"
        }
      }
      
      if(req(input$navbar) == "Morphospace and Warp Grids"){
        alert_vals$navbar_morphospaceplot <- "Here"
        if(req(input$tip_col_category) != "all_1_col"){
          alert_vals$navbar_morphospaceplot_tipcol <- "Here"
        }
        if(input$include_phylo){
          alert_vals$navbar_morphospaceplot_phylo <- "Here"
        }
        if(!(req(input$tip_pch) %in% c("19", "1", "18"))){
          alert_vals$navbar_morphospaceplot_tippch <- "Here"
        }
        if(!is.null(vals$warp_initiated)){
          alert_vals$navbar_morphospaceplot_warp <- "Here"
          
        }
      }
      
      if(req(input$navbar) == "Linear Models"){
        alert_vals$navbar_linearmodels <- "Here"
        if(req(input$tab_linearmodels) == "Model Design"){
          alert_vals$navbar_linearmodels_modeldesign <- "Here" 
        }
        if(req(input$tab_linearmodels) == "Allometry"){
          alert_vals$navbar_linearmodels_allometry <- "Here" 
        }
        if(req(input$tab_linearmodels) == "Model Comparison"){
          alert_vals$navbar_linearmodels_modelcomparison <- "Here"  
        }
      }
      
      if(req(input$navbar) == "Example Tutorial"){
        alert_vals$navbar_exampletutorial <- "Here"
      }}
  })
  
  tryObserveEvent(alert_vals$navbar_dataprep, {
    if(!is.null(alert_vals$navbar_dataprep)){
      shinyalert(
        title = "Data Prep",
        text = "This page has tabs for: <br> (1) defining landmark links and semilandmarks <br>
      (2) visualizing outliers or particular specimens of interest <br>
      & (3) performing the Generalized Procrustes Alignment. <br></br>
      If on the previous page you indicated that your landmarks are not yet aligned, 
      you are required to run the GPA before moving forward.",
        size = "s",
        html = T
      )
    }
  })
  
  tryObserveEvent(alert_vals$navbar_dataprep_links, {
    if(!is.null(alert_vals$navbar_dataprep_links)){
      shinyalert(
        title = "Define Links and Semi-Landmarks",
        text = "On this tab, you will see a plot similar to one generated from the function 
        <span style='font-family: Courier New'>plotAllSpecimens</span>.  
      <br></br>
      To define links between landmarks, <strong style='color:#08a89e'>single-click</strong> 
      the landmarks you wish to connect in order, 
      ending the string with a <strong style='color:#3691d1'>double-click</strong>.
      <br></br>
      To assign semilandmarks, <strong style=color:#9560d1>click-and-drag</strong> to highlight the desired semilandmark(s). 
      If the selected semilandmark is linked to other landmarks, these linked landmarks will be automatically assigned as the 
      bracketing landmarks between which the semilandmark will slide during the GPA. 
       <br></br>
       The semilandmark matrix in the Settings panel is auto-filled by defining links and semilandmarks on the plot.
       However, you can also define or edit them directly in the matrix or by uploading a file ('Upload Semilandmark Matrix'). 
       Duplicate semilandmark rows will be automatically deleted, prioritizing the most recently defined brackets.
      ",
        html = T,
        size = "m"
      )
    }
  })
  
  tryObserveEvent(vals$links_dbclicked, ignoreInit = T, {
    if(input$alert_on_off == TRUE & !is.null(vals$links_dbclicked)) {
      if(is.null(alert_vals$navbar_dataprep_links_linkalertdone)) {
        delay(300,{
          shinyalert(
            title = "You've defined links!",
            text = "
      These links are applied to specimen visualizations throughout the App and 
      can be reset at any time using the 'Reset Landmark Links' button in the Settings panel on this tab. 
      <br></br>
      Links are also used to define the bracketing landmarks around semilandmarks. 
          More related Instructions appear if you select a semilandmark (<strong style=color:#9560d1>click-and-drag</strong>).",
            html = T,
            type = "success",
            size = "s"
          )
          alert_vals$navbar_dataprep_links_linkalertdone <- "yes"
        })
      }
    }
  })
  
  tryObserveEvent(vals$semis_brushed, ignoreInit = T, {
    if(input$alert_on_off == TRUE & !is.null(vals$semis_brushed)) {
      if(is.null(alert_vals$navbar_dataprep_links_semialertdone)) {
        shinyalert(
          title = "You've selected semilandmarks!",
          text = "The semilandmark selection has been recorded in the Semilandmark Matrix in the Settings panel.<br></br> 
      Before performing the GPA, you will need to assign the bracketing landmarks along which each semilandmark 
      will slide. This may have been filled in automatically if you have already defined links through the selected semilandmark(s). 
      <br><br>
      These 'Before' and 'After' landmarks can be overwritten manually or by defining new links through the desired semilandmark(s).
          
          The Semilandmark Matrix can be reset using the 'Reset Semilandmark Selection' button.",
          html = T,
          type = "success",
          size = "m"
        )
        alert_vals$navbar_dataprep_links_semialertdone <- "yes"
      }
    }
  })
  
  tryObserveEvent(vals$alert_semilmmat_wrong_format, {
    shinyalert(
      title = "Selected File in Wrong Format",
      text = "Something about the file you selected does not match the requirements for this input. 
      Files must have 3 columns, one for the beginning landmark, one for the semilandmark, and one for the ending landmark between which the semilandmark slides.
      <br></br>
      As a reminder, you can enter your semilandmarks in three ways: <br>
      (1) manually entering landmark values into the matrix in the Settings panel, <br>
      (2) <strong style=color:#9560d1>click-and-dragging</strong> over the desired semilandmarks on the plot, <br> then defining
      the bracket landmarks via manual entry or using the links (<strong style='color:#08a89e'>clicks</strong> and <strong style='color:#3691d1'>double-clicks</strong>), or <br>
      (3) uploading an appropriately formatted file. Such a file can be generated 
      in geomorph using the <span style='font-family: Courier New'>define.sliders</span> function.
      ",
      size = "m",
      html = T
    )
  })
  
  tryObserveEvent(alert_vals$navbar_dataprep_outliers, {
    if(input$alert_on_off == TRUE){
      shinyalert(
        title = "Visualize Outliers and Individual Specimens",
        text = "Here you will see the plot generated from the function <span style='font-family: Courier New'>plotOutliers</span>. 
      <br></br>
      To inspect a single specimen of interest, 
      <strong style='color:#08a89e'>single-click</strong> the desired point on the plot, and it's landmarks will appear below the outliers plot.
       <br></br>
      If any of the traits uploaded on the Data Input page is discrete, 
      an option to 'Visualize Outliers by Trait Group' will appear in the Settings panel, 
      with options to toggle between traits and levels used to generate the plot. This option is only available for 
      levels with more than 1 specimen in it.
      <br></br>
        This tab also allows you to remove specimens from all your datasets. If one of the specimens in your sample appears to be an outlier, 
        you can select said specimen from the 'Exclude Specimen from Dataset' dropdown menu and press the 'Remove' button. 
        All corresponding data will be excluded from subsequent analyses. 
        This action can be undone by pressing the 'Undo Removal' button that appears after removal.",
        size = "m",
        html = T
      )
    }
  })
  
  tryObserveEvent(alert_vals$navbar_dataprep_gpa, {
    if(input$alert_on_off == TRUE) {
      shinyalert(
        title = "Generalized Procrustes Alignment",
        text = "This tab serves as a checkpoint to verify all the pre-alignment data prep done so far.
        Before pressing the 'Run GPA' button in the right-most column to perform the GPA, make sure that the adjustments from
        raw LMs to the desired dataset is correct (middle column). <br><br>
        This step must be completed before moving forward unless the uploaded shape data were previously aligned and 
        the corresponding option was selected on the Data Input page. <br><br>
        Once the GPA is performed (function <span style='font-family: Courier New'>gpagen</span>), 
        the specimens' centroid sizes will be passed on as a trait and can be used in various parts of the App (e.g., measuring phylogenetic
        signal on the Shape Patterns page, coloring data points on the Morphospace and Warp Grids page, etc.). ",
        size  = "m",
        html = T
      )
    }
  })
  
  tryObserveEvent(alert_vals$gpa_not_ready, ignoreInit = T, {
    if(alert_vals$gpa_not_ready) {
      shinyalert(
        title = "Error in the Semilandmark Matrix",
        text = "Possible errors:
        (1) the semilandmark matrix is incomplete,
      (2) one of the semilandmarks is repeated (i.e., a semilandmark appears twice or more in the 'Slide' column), 
      (3) one of the landmarks is invalid (e.g., LM 12 is assigned to the semilandmark matrix 
      when only 11 total landmarks exist in the shape dataset),
      (4) one of the semilandmarks is bracketed by itself (e.g., semilandmark 5 is set to slide between LMs 3 and 5), or
      (5) one of the semilandmarks is bracketed between 1 landmark on both sides (e.g., semilandmark 5 is set to slide between LMs 3 and 3). 
      <br></br>
      Return to the 'Define Links and Semi-Landmarks' tab to fix the error before aligning the specimens.",
        type = "error",
        inputId = "alert_gpa_not_ready",
        html = T,
        size = "m"
      )
    }
  })
  
  tryObserveEvent(input$alert_gpa_not_ready, {
    if(input$alert_gpa_not_ready){
      updateTabsetPanel(session, "tab_dataprep", selected = "Define Links and Semi-Landmarks") # doesnt need a bookmarking workaround
    }
  })
  
  tryObserveEvent(alert_vals$navbar_shapepatterns, {
    if(input$alert_on_off == TRUE){
      shinyalert(
        title = "Shape Patterns",
        text = "This page allows you to explore various shape patterns, including Modularity and Integration. 
        If a phylogeny has been uploaded, Phylogenetic Signal can also be quantified.",
        size = "s",
        html = T
      )
    }
  })
  
  
  tryObserveEvent(alert_vals$navbar_shapepatterns_signal, {
    if(input$alert_on_off == TRUE) {
      shinyalert(
        title = "Phylogenetic Signal",
        text = "Here you will find the output from the function <span style='font-family: Courier New'>physignal</span>. 
      <br></br>
      The plot displays the null K distribution, with which the p value and effect size of the observed value are determined.
      <br></br>
      You can choose to investigate the degree of phylogenetic signal in the shape data or in any uploaded continuous trait 
        in the Settings panel.",
        size  = "s",
        html = T
      )
    }
  })
  
  tryObserveEvent(alert_vals$navbar_shapepatterns_modularity, {
    if(input$alert_on_off == TRUE) {
      shinyalert(
        title = "Modularity",
        text = "Here you will find options to test degree of modularity and, if a phylogeny is uploaded, evolutionary rate
        variation between modules. 
      <br></br>
      The first step is to define the number of modules and to specify which landmarks belong
      to each module by dragging the landmark number to the appropriate heading in the Settings panel. Once all landmarks are assigned, 
      press the 'Assign LMs to Modules' button to finalize the module definitions. Double check the assignments with the Module Visualization plot
        in the main panel.",
        size  = "m",
        html = T
      )
    }
  })
  
  tryObserveEvent(input$apply_modular_groups_go, {
    if(input$alert_on_off == TRUE & input$apply_modular_groups_go > 0& is.null(alert_vals$navbar_shapepatterns_modularitygoalertdone)) {
      shinyalert(
        title = "Modules Assigned!",
        text = "Now that your modules have been assigned, scroll down on the main panel to see output from the function 
        <span style='font-family: Courier New'>modularity.test</span> or <span style='font-family: Courier New'>phylo.modularity</span>,
        depending on the option selected for 'Evaluate Modularity in a Phylogenetic Context' in the Settings Panel 
        (only available if a phylogeny is uploaded). This plot displays the null CR Coefficient distribution, with which the 
        p value and effect size of the observed value are determined.
      <br></br>
      The bottom plot is generated using the function <span style='font-family: Courier New'>compare.multi.evol.rates</span>. 
        Similar to the CR plot, this displays the null distribution of the rate ratio between the modules. If more than
        2 modules have been defined, the observed rate ratio corresponds to the rate ratio between the most disparate group rates 
        (i.e., fastest evolving module:slowest evolving module).",
        size  = "m",
        html = T,
        type = "success"
      )
      alert_vals$navbar_shapepatterns_modularitygoalertdone <- 'yes' ##dd newww
    }
  })
  
  tryObserveEvent(alert_vals$navbar_shapepatterns_integration, {
    if(input$alert_on_off == TRUE) {
      shinyalert(
        title = "Integration",
        text = "Here you can calculate various measures of integration. The first plot in this tab is of the global integration 
        generated from the function <span style='font-family: Courier New'>globalIntegration</span>. 
      <br></br>
      The below plot displays the level of integration between modules, which must be defined in the 'Modularity' tab.
      The plot is generated using the <span style='font-family: Courier New'>integration.test</span> or 
      <span style='font-family: Courier New'>phylo.integration</span> function, 
      depending on the option selected for 'Evaluate Integration in a Phylogenetic Context' in the Settings Panel 
        (only available if a phylogeny is uploaded). The plot visualizing PLS Block 1 vs Block 2 can only be generated
        when there are only 2 modules.
        <br><br>
         Integration between modules can also be calculated on a subset of specimens as defined by any uploaded discrete trait 
       data and is specified in the Settings Panel. Only traits for which 3 or more specimens belong to each level can be visualized.",
        size  = "m",
        html = T
      )
    }
  })
  
  tryObserveEvent(alert_vals$navbar_morphospaceplot, {  
    if(input$alert_on_off == TRUE){
      shinyalert(
        title = "Morphospace and Warp Grids Page",
        text = "This page shows the shape data PCA plot generated from the functions 
      <span style='font-family: Courier New'>gm.prcomp</span> and <span style='font-family: Courier New'>plot</span>.
    <br></br>
    There are many ways to modify this plot in the Settings panel, including aspects of the alignment,
    which PC axes are displayed,
    the color, size, shape, and labels of the points, convex hulls,
    and whether to calculate and plot ancestral states along the phylogeny (if uploaded).
    <br></br> 
    If 'Point Color' is set to 'All One Color' (default), you may select from one of the displayed color options below that drop down menu,
    or check the 'Other' box, which allows you to select an R-defined color name. Alternatively, the points could be
    colored by any uploaded trait data or, if applicable, centroid size. 
    <br></br>
    <strong>Warp grids</strong> can also be displayed on this page. <br>
    Initiate these options by <strong style='color:#08a89e'>single-clicking</strong> any point or <strong style='color:#3691d1'>double-clicking</strong> any region on the morphospace plot.",
        size = "m",
        html = T
      )
    }
  })
  
  tryObserveEvent(alert_vals$navbar_morphospaceplot_tipcol, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_morphospaceplot_tipcolalertdone)) {
      shinyalert(
        title = "You've selected to color points by one of your traits!",
        text = " 
      Below the 'Point Color' drop down menu has appeared several colored boxes that allow you to define the group color of each level. 
      If the trait selected is continuous, the two boxes represent the start and end of a color spectrum, 
      along which the points are colored according to their value. If the trait is discrete, the boxes correspond to the levels of said trait.
      <br></br>
      A legend appeared on the left side of the morphospace plot, indicating  which colors 
      belong to which trait levels.
      
      This legend can be repositioned with a <strong style=color:#9560d1>click-and-drag</strong>.",
        html = T,
        type = "success",
        size = "s"
      )
      alert_vals$navbar_morphospaceplot_tipcolalertdone <- "yes"
    }
  })
  
  tryObserveEvent(alert_vals$navbar_morphospaceplot_phylo, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_morphospaceplot_phyloalertdone)) {
      shinyalert(
        title = "You've opted to display a phylomorphospace!",
        text = "Additional options for adjusting visual components of the phylogeny are available
        by checking the 'More Phylogeny Style Options' box that has appeared in the Settings panel.",
        html = T,
        type = "success",
        size = "s"
      )
      alert_vals$navbar_morphospaceplot_phyloalertdone <- "yes"
    }
  })
  
  tryObserveEvent(alert_vals$navbar_morphospaceplot_tippch, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_morphospaceplot_tippchalertdone)) {
      shinyalert(
        title = "You've selected to define point shapes by one of your traits!",
        text = "This is only an option for discrete traits.
      <br></br>
      A legend appeared on the left side of the morphospace plot, indicating which shapes 
      belong to which trait levels. This legend can be repositioned with a <strong style=color:#9560d1>click-and-drag</strong>.",
        html = T,
        type = "success",
        size = "s"
      )
      alert_vals$navbar_morphospaceplot_tippchalertdone <- "yes"
    }
  })
  
  tryObserveEvent(alert_vals$navbar_morphospaceplot_warp, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_morphospaceplot_warpalertdone)) {
      shinyalert(
        title = "You've initiated a warp grid!",
        text = "This warp grid was produced with the function 
        <span style='font-family: Courier New'>plotRefToTarget</span>, and 
        the page has been scrolled down to view the plot. The associated settings are now available at the bottom of the
        Settings panel.
        <br></br>
        Options for Warp Grid Magnitude and Warp Type correspond to the arguments in the <span style='font-family: Courier New'>plotRefToTarget</span> function,
        'mag' and 'method'.
        You can also choose to display the variation along particular PC axes (Variation Displayed).
        <br><br>
        The <strong>Warp Comparison Start (Reference)</strong> and <strong>Warp Comparison End (Target)</strong> options correspond to the 
        two points of comparison for the warp grid. An Observed Point (Clicked) for either 
        the Reference or Target points can be selected by <strong style='color:#08a89e'>single-clicking</strong> 
        a point on the morphospace above. 
        Alternatively, a particular specimen can be selected using the Observed Point (By Name) option 
        for which a drop down menu will appear. 
        <br></br>
        You can also select a theoretical part of morphospace by <strong style='color:#3691d1'>double-clicking</strong>, 
        which can be assigned to either the Reference or Target points of the comparison. This option 
        will appear in the Warp Comparison options once said theoretical space is selected. 
        <br><br>
        To clarify exactly 
        which shapes, observed or projected, are involved in the warp grid, an arrow has been added to the morphospace above
        showing the directionality between the selected Reference and Target points.
        This can be removed by unchecking the box 'Show Comparison Trajectory',
        <br></br>
        Even more stylistic options for the warp grid are available by checking 'Show More Warp Grid Style Options.'",
        html = T,
        type = "success",
        size = "m"
      )
      alert_vals$navbar_morphospaceplot_warpalertdone <- "yes"
    }
  })
  
  tryObserveEvent(alert_vals$navbar_linearmodels, {  
    if(input$alert_on_off == TRUE){
      shinyalert(
        title = "Linear Models",
        text = "This page shows a select few ways to evaluate linear models with geomorph. For greater flexibility
        with these (and all) App operations, we advise learning to use geomorph in R.",
        size = "s",
        html = T
      )
    }
  })
  
  tryObserveEvent(alert_vals$navbar_linearmodels_modeldesign, {
    if(input$alert_on_off == T & is.null(alert_vals$navbar_linearmodels_modeldesignalertdone)) {
      shinyalert(
        title = "Model Design",
        text = "With this tab, you can investigate how shape varies with respect to 
        your uploaded trait and/or centroid size (if applicable). Tests can also be run 
        to evaluate pairwise comparisons of means, morphological disparity, and evolutionary rates.
        All analyses are generated from the functions <span style='font-family: Courier New'>procD.pgls</span> or 
      <span style='font-family: Courier New'>procD.lm</span> corresponding to the 'Analysis Type' option in the Settings
        panel.
        <br><br>
        The first step is defining which independent variables you would like to include in the model ('Independent Variables Tested'). 
        If more than one trait is selected, an option will appear to change the order of the traits by dragging and dropping. 
        As you select traits and change their order, the defined model will be explicated at the top of the main panel ('Model Tested').
        <strong>Once you are happy with your model and all remaining tests have been specified, press the 'Calculate' button at the bottom
        of the Settings Panel.</strong> If any Settings are subsequently changed, you will need to press 'Calculate' again to get the updated results.
        <br><br>If the model is complex, the number of permutations is high, or you have a lot of specimens, the model 
        fit might take a few seconds to generate. Once this model is defined and calculated, you can visualize the allometry plot in the 
        'Allometry' tab. The 'Model Comparison' tab, on the other hand, does not require this model to be run.",
        html = T,
        size = "m"
      )
      alert_vals$navbar_linearmodels_modeldesignalertdone <- "yes" 
    }
  })
  
  tryObserveEvent(alert_vals$navbar_linearmodels_allometry, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_linearmodels_allometryalertdone)) {
      shinyalert(
        title = "Allometry",
        text = "Using the model defined in the 'Model Design' tab, here you will find the associated Allometry plot.
        <br><br> The settings for 'Allometry Type',
        'Regression Type', 'Predictor Variable', and 'Color Points' correspond to the arguments 
        'type', 'reg.type', 'predictor', and 'col' in the <span style='font-family: Courier New'>procD.lm</span>
        function, respectively. The Predictor Variable can be any uploaded continuous variable or centroid size (if applicable).
        If 'Centroid Size' is selected, you can choose whether or not to log transform the centroid sizes in the Settings panel.",
        html = T,
        size = "s"
      )
      alert_vals$navbar_linearmodels_allometryalertdone <- "yes"
    }
  })
  
  tryObserveEvent(alert_vals$navbar_linearmodels_modelcomparison, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_linearmodels_modelcomparisonalertdone)) {
      shinyalert(
        title = "Model Comparison",
        text = "This tab is similar to the 'Model Design' tab in how each model is defined. 
        Up to three models can be compared.",
        html = T,
        size = "s"
      )
      alert_vals$navbar_linearmodels_modelcomparisonalertdone <- "yes"
    }
  })
  
  
  ## Alerts that stop misuse of App (always on, even if Instructions are off)
  
  
  tryObserveEvent(datasets_dont_match(), ignoreInit = T, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_datainput_dontmatchalertdone)) { #  
      delay(1000,
            if(datasets_dont_match()){
              shinyalert(
                title = "Data Input Page",
                text = "Some or all of the datasets you've uploaded have mismatching specimen/species names. 
          <br></br>
          <em>This must be addressed before moving forward</em> 
          by pressing the 'Prune Datasets to Match' button
          that has appeared near the top of the page. 
          Once pressed, any new uploaded datasets might also need to be pruned. 
          <br></br>
          Note: this mismatch might simply be the result of inconsistent ordering of the specimens/species. 
          Even if this is the case, you must press the Prune button, which will reorder the data to match.
                ",
                size = "m",
                type = "warning",
                html = T
              )
              alert_vals$navbar_datainput_dontmatchalertdone <- "yes"
            }
      )
    }
  })
  
  tryObserveEvent(alert_vals$prune_warning, ignoreInit = T, {
    req(alert_vals$prune_warning)
    if(alert_vals$prune_warning == "remove_all_species") {
      shinyalert(
        title = "Prune Stopped",
        text = "It appears that your datasets do not have common specimen names 
        across all data files. 
        <br></br>
        Check that the correct data files were selected for upload 
        and that the specimen names are consistent in format. Inconsistent capitalizations,
        periods, dashes, spaces, or other formatting differences should be rectified before attempting 
        to prune the datasets to match.",
        type = "error",
        html = T,
        size = "s"
      )
      vals$prune_datasets <- NULL # unclick that button
    }
  })
  
  mismatch_alert_listen <- reactive({list(input$navbar, datasets_dont_match())})
  
  tryObserveEvent(mismatch_alert_listen(), {
    req(datasets_dont_match())
    if(datasets_dont_match() & input$navbar != "Data Input"){
      shinyalert(
        title = "STOP",
        text = "It appears that your datasets have not been pruned to match eachother. 
        <br></br>
        Mismatched datasets will cause errors in downstream plots and analyses. 
        Return to the Data Input page and
        press the 'Prune Datasets to Match' button. <br></br>
        If you believe you've reached this message in error, check the styles of 
        the specimen names across data files. 
        Underscores, capitalizations, or extra spaces may be causing the mismatch.",
        type = "error",
        html = T,
        size = "s",
        inputId = "alert_datasets_dont_match"
      )
    }
  })
  
  
  tryObserveEvent(input$alert_datasets_dont_match, {
    if(input$alert_datasets_dont_match){
      updateNavbarPage(session, "navbar", selected = "Data Input") # doesnt need a bookmarking workaround
    }
  })
  
  
  tryObserveEvent(input$navbar, ignoreInit = T, {
    if(input$navbar != "Data Prep" & input$navbar != "Data Input") {
      if(is.null(vals$go_run_gpa) & input$raw_lms_already_aligned == F) {
        shinyalert(
          title = "STOP",
          text = "The shape data have not yet been aligned. 
        <br></br>
        You indicated on the Data Input page that your uploaded shape data were not yet aligned.
        In this case, you must finalize all the alignment details on the Data Prep page under the Generalized Procrustes Alignment tab.
        Return to that tab to confirm the details of your data prep, then press the 'Run GPA' button before proceeding.",
          type = "error",
          html = T,
          size = "s",
          inputId = "gpa_not_run_alert"
        )
      }
    }
  })
  
  tryObserveEvent(input$gpa_not_run_alert, {
    if(input$gpa_not_run_alert){
      updateNavbarPage(session, "navbar", selected = "Data Prep") # doesnt need a bookmarking workaround
      updateTabsetPanel(session, "tab_dataprep", selected = "Generalized Procrustes Alignment")
    }
  })
  
  # symmetry individuals  
  
  tryObserveEvent(input$go_symmetry_file, ignoreInit = T,  priority = 10, {
    vals$go_symmetry_pattern <- NULL
    vals$go_symmetry_file <- input$go_symmetry_file
  })
  
  tryObserveEvent(input$go_symmetry_pattern, ignoreInit = T, priority = 10, {
    vals$go_symmetry_file <- NULL
    vals$go_symmetry_pattern <- input$go_symmetry_pattern
  })
  
  tryObserveEvent(input$go_symmetry_manual, ignoreInit = T, priority = 10, {
    vals$go_symmetry_file <- NULL
    vals$go_symmetry_pattern <- NULL
    vals$go_show_specimen_assignments <- "go"
    updateMatrixInput(session, "symmetry_definitions", 
                      value = symmetry_manual_matrix)
    updateRadioButtons(session, "symmetry_ind_pattern",  
                       choices = c("Not Repeated" = "no_repeats",
                                   "Grouped Every nth Specimen" = "nth", 
                                   "Grouped in Blocks" = "blocks"),
                       selected = "no_repeats")
    
  })
  
  tryObserveEvent(input$symmetry_definitions, ignoreInit = T, priority= 10, {
    updateRadioButtons(session, "symmetry_land_pairs_pattern", label = "Group Landmarks:", 
                       choices = c("Every Other" = "every_other",
                                   "In Blocks" = "blocks", 
                                   "With File Upload" = "file"),
                       selected = character(0))
  })
  
  symmetry_ind_listen <- reactive({list(input$symmetry_ind_pattern, vals$go_symmetry_pattern)})
  tryObserveEvent(symmetry_ind_listen(), ignoreInit = F,  priority = -10, {
    req(input$symmetry_ind_pattern)
    if(!is.null(vals$go_symmetry_pattern)) {
      if(input$symmetry_ind_pattern == "no_repeats"){
        shinyalert(
          title = "Specimen Individuals Assigned",
          text = "Each specimen has been assigned a unique identity, and thus the data do not include replicates.",
          inputId = "symmetry_ind_pattern_no"
        )
      }
      if(input$symmetry_ind_pattern == "nth") {
        shinyalert(
          title = "Define Individuals as Every Nth Specimen",
          text = "Enter the integer that defines n. For example, if Individual A is represented by every 5th specimen, n = 5.",
          type = "input",
          inputType = "number",
          inputId = "symmetry_ind_pattern_nth"
        )
      }
      if(input$symmetry_ind_pattern == "blocks"){
        shinyalert(
          title = "Define Individuals in Blocks",
          text = "Enter the integer that defines the size of each block. 
        For example, if each individual is represented by 4 specimens in a row, enter '4'.",
          type = "input",
          inputType = "number",
          inputId = "symmetry_ind_pattern_block"
        )
      }
    }
  })
  
  symmetry_ind_pattern_listen <- reactive({list(input$symmetry_ind_pattern_no,
                                                input$symmetry_ind_pattern_nth, 
                                                input$symmetry_ind_pattern_block)})
  tryObserveEvent(symmetry_ind_pattern_listen(), ignoreInit = T, {
    if(input$symmetry_ind_pattern == "no_repeats") {
      symmetry_ind <- 1:dim(gpa_coords_rx())[3]
      symmetry_replicate <- rep(1, dim(gpa_coords_rx())[3])
      vals$symmetry_ind_no_repeats <- T
    }
    
    if(input$symmetry_ind_pattern == "nth") {
      n <- as.numeric(input$symmetry_ind_pattern_nth)
      dimbyn <- dim(gpa_coords_rx())[3]/n
      if(dimbyn == round(dimbyn)) { # if the specimen number splits evenly
        symmetry_ind <- rep(1:n,dimbyn)
        symmetry_replicate <- rep(1:dimbyn, each = n)
      } else {
        symmetry_ind <- rep(1:n,floor(dimbyn))
        symmetry_replicate <- rep(1:floor(dimbyn), each = n)
        nas_left <- dim(gpa_coords_rx())[3] - length(symmetry_ind)
        symmetry_ind <- c(symmetry_ind, 1:nas_left)
        symmetry_replicate <- c(symmetry_replicate, rep(ceiling(dimbyn), nas_left))
      }
    }
    
    if(input$symmetry_ind_pattern == "blocks") {
      n <- as.numeric(input$symmetry_ind_pattern_block)
      dimbyn <- dim(gpa_coords_rx())[3]/n
      if(dimbyn == round(dimbyn)) { # if the specimen number splits evenly
        symmetry_ind <- rep(1:dimbyn, each = n)
        symmetry_replicate <- rep(1:n, dimbyn)
      } else {
        symmetry_ind <- rep(1:floor(dimbyn), each = n)
        symmetry_replicate <- rep(1:n, floor(dimbyn))
        nas_left <- dim(gpa_coords_rx())[3] - length(symmetry_ind)
        symmetry_ind <- c(symmetry_ind, rep(ceiling(dimbyn), nas_left))
        symmetry_replicate <- c(symmetry_replicate, 1:nas_left)
      }
    }
    
    vals$symmetry_ind <- symmetry_ind
    vals$symmetry_replicate <- symmetry_replicate
    
    if(is.null(vals$symmetry_side)){
      symmetry_side <- rep(NA, dim(gpa_coords_rx())[3])
    } else {symmetry_side <-vals$symmetry_side }
    
    if(input$symmetry_obj_sym) {
      symmetry_definitions <- cbind(vals$symmetry_ind, vals$symmetry_replicate)
      colnames(symmetry_definitions)<- c("Individual", "Replicate") #  
      row.names(symmetry_definitions) <- dimnames(gpa_coords_rx())[[3]] #  
    } else {
      symmetry_definitions <- cbind(vals$symmetry_ind, vals$symmetry_replicate, symmetry_side)
      colnames(symmetry_definitions)<- c("Individual", "Replicate", "Side") #  
      row.names(symmetry_definitions) <- dimnames(gpa_coords_rx())[[3]] #  
    }
    
    updateMatrixInput(session, "symmetry_definitions", value = symmetry_definitions)
  })
  
  
  # symmetry land pairs  
  tryObserveEvent(input$symmetry_land_pairs_pattern, ignoreInit = T, {
    if(input$symmetry_land_pairs_pattern != "file"){
      if(ceiling(dim(gpa_coords_rx())[1]/2) == floor(dim(gpa_coords_rx())[1]/2)) { # checking that things are even
        
        if(input$symmetry_land_pairs_pattern == "every_other") {
          symmetry_land_pairs <- matrix(1:dim(lizards$coords)[1], ncol = 2, byrow = T)
          shinyalert(
            title = "Landmarks Assigned Every Other",
            text = "Even landmarks have been assigned to one side, and odd landmarks have been assigned to the other."
          )
        }
        if(input$symmetry_land_pairs_pattern == "blocks") {
          symmetry_land_pairs <- matrix(1:dim(lizards$coords)[1], ncol = 2, byrow = F)
          shinyalert(
            title = "Landmarks Assigned in Blocks",
            text = "The first half of the landmarks have been assigned to one side, and the second half of the landmarks have been assigned to the other."
          )
        }
        vals$symmetry_land_pairs <- symmetry_land_pairs
        
      } else {
        shinyalert(
          title = "Odd Number of Landmarks",
          text = "Landmark pairs cannot be defined automatically. Select Manual Entry for Grouping Landmarks."
        )
      } 
    } else {
      shinyalert(
        title = "Upload a Landmark Side File",
        text = "A button to upload your Landmark Side File should now be visible in the Settings.
        
        This can be either a CSV or an Excel file, and the data must be organized into two columns (one for each side). Not all landmarks must be assigned a side, but none can be repeated. Do not include column headers.
        
        See geomorph help file for the function 'bilat.symmetry' for examples of these matrices.",
        inputId = "symmetry_land_pairs_file_input"
      )
    }
  })
  
  tryObserveEvent(input$symmetry_land_pairs_file_upload, ignoreInit  = T, {
    inFile <- input$symmetry_land_pairs_file_upload
    if (endsWith(inFile$name, '.xlsx') | endsWith(inFile$name, '.xls')| 
        endsWith(inFile$name, '.XLSX') | endsWith(inFile$name, '.XLS')){
      symmetry_land_pairs_file <- readxl::read_excel(inFile$datapath)
    } else if (endsWith(inFile$name, '.csv') | endsWith(inFile$name, '.CSV')){
      symmetry_land_pairs_file <- read.csv(inFile$datapath) # read in trait file (this cant be from the trait_rx or else the reactives become circular)
    } 
    vals$symmetry_land_pairs <- symmetry_land_pairs_file
  })
  
  
  # symmetry sides
  
  symmetry_side_pattern_listen <- reactive({list(input$symmetry_side_pattern, input$symmetry_side)})
  tryObserveEvent(symmetry_side_pattern_listen(), ignoreInit = T, {
    req(input$symmetry_side_pattern)
    if(input$symmetry_side_pattern == "every_other") {
      
      dimbyn <- dim(gpa_coords_rx())[3]/2
      if(dimbyn == round(dimbyn)) { # if the specimen number splits evenly
        symmetry_side <- rep(1:2,dimbyn)
      } else {
        symmetry_side <- rep(1:2,floor(dimbyn))
        symmetry_side <- c(symmetry_side, 1)
      } 
      vals$symmetry_side <- symmetry_side
      
      symmetry_replicate <- rep(1, length(symmetry_side))
      symmetry_replicate_loops <- unique(paste(vals$symmetry_ind, symmetry_side))
      for(i in 1:length(symmetry_replicate_loops)){
        all_reps_tf <- paste(vals$symmetry_ind, symmetry_side) %in% symmetry_replicate_loops[i]
        symmetry_replicate[all_reps_tf] <- 1:length(which(all_reps_tf == T))
      }
      vals$symmetry_replicate <- symmetry_replicate
      
      
      # if this happens, vals$symmetry_replicate should be adjusted !!!!
    } 
    if(input$symmetry_side_pattern == "blocks"){
      
      
      symmetry_side <- rep(1, dim(gpa_coords_rx())[3])
      ind_groups <- levels(as.factor(vals$symmetry_ind))
      for(i in 1:length(ind_groups)) {
        inds <- which(vals$symmetry_ind == ind_groups[i])
        if(ceiling(length(inds)/2) == floor(length(inds)/2)) {
          inds_second_half <- inds[(length(inds)/2 + 1):length(inds)]
        } else { inds_second_half <- inds[ceiling(length(inds)/2):length(inds)]}
        symmetry_side[inds_second_half] <- 2
      }
      vals$symmetry_side <- symmetry_side
      
      symmetry_replicate <- rep(1, length(symmetry_side))
      symmetry_replicate_loops <- unique(paste(vals$symmetry_ind, symmetry_side))
      for(i in 1:length(symmetry_replicate_loops)){
        all_reps_tf <- paste(vals$symmetry_ind, symmetry_side) %in% symmetry_replicate_loops[i]
        symmetry_replicate[all_reps_tf] <- 1:length(which(all_reps_tf == T))
      }
      vals$symmetry_replicate <- symmetry_replicate
    }
    
    if(is.null(vals$symmetry_ind)){
      symmetry_ind <- rep(NA, dim(gpa_coords_rx())[3])
    } else {symmetry_ind <-vals$symmetry_ind }
    if(is.null(vals$symmetry_replicate)){
      symmetry_replicate <- rep(NA, dim(gpa_coords_rx())[3])
    } else {symmetry_replicate <-vals$symmetry_replicate }
    
    
    symmetry_definitions <- cbind(symmetry_ind, symmetry_replicate, vals$symmetry_side)
    colnames(symmetry_definitions)<- c( "Individual", "Replicate", "Side")
    row.names(symmetry_definitions) <- dimnames(gpa_coords_rx())[[3]]
    
    updateMatrixInput(session, "symmetry_definitions", value = symmetry_definitions)
    
  })
  
  tryObserveEvent(input$symmetry_file_upload, ignoreInit = T, {
    req(gpa_coords_rx())
    inFile <- input$symmetry_file_upload
    if (endsWith(inFile$name, '.xlsx') | endsWith(inFile$name, '.xls')| 
        endsWith(inFile$name, '.XLSX') | endsWith(inFile$name, '.XLS')){
      symmetry_file <- readxl::read_excel(inFile$datapath)
    } else if (endsWith(inFile$name, '.csv') | endsWith(inFile$name, '.CSV')){
      symmetry_file <- read.csv(inFile$datapath) # read in trait file (this cant be from the trait_rx or else the reactives become circular)
    } 
    symmetry_file <- as.data.frame(symmetry_file)
    if(ncol(symmetry_file) != 4) {
      shinyalert(
        title = "Incorrect File Format",
        text = "The file you have chosen is not in the correct format. 
          This file must have 4 columns with columns for the specimen ID 
          and numerical assignments for Individual, Replicate, and Side for each specimen (in that order). ",
        type = "error"
      )
    } else {
      if(anyNA(match(dimnames(gpa_coords_rx())[[3]], symmetry_file[,1]))) {
        shinyalert(
          title = "Mismatch in Specimen Names",
          text = "The first column in the uploaded file does not match the names of the specimens in the shape dataset.",
          type = "error"
        )
      } else {
        symmetry_file <- symmetry_file[match(dimnames(gpa_coords_rx())[[3]], symmetry_file[,1]), ]
        vals$symmetry_ind <- symmetry_file[,2]
        vals$symmetry_replicate <- symmetry_file[,3]
        vals$symmetry_side <- symmetry_file[,4]
        updateMatrixInput(session, "symmetry_definitions", value = symmetry_file)
      }
    }
  })
  
  #tryObserveEvent(input$go_symmetry_useoutput, ignoreInit = T, {
  # vals$lms_rx <- vals$bilat_symmetry$symm.shape # not this
  #})
  
  
  #### Input Data Reactives ####
  
  vals <- reactiveValues()
  update_vals <- reactiveValues()
  
  lms_rx <- reactive({ # landmark reactive element
    dd <- input$go_remove_outlier_specimen_reset #trigger dddd
    if(is.null(vals$go_example_1)){ # if the user is NOT using the sample pleth data
      
      req(input$file_tps) # stop running this reactive if there is no input tps file selected
      inFile <- input$file_tps
      if (endsWith(inFile$name, '.nts')){
        temp_LMs <- readland.nts(inFile$datapath)
      } else if (endsWith(inFile$name, '.shapes')){
        temp_LMs <- readland.shapes(inFile$datapath)
      } else if (endsWith(inFile$name, '.tps') | endsWith(inFile$name, '.TPS')){
        temp_LMs <- readland.tps(inFile$datapath, specID = input$spec_id, negNA = input$neg_lms) # read in the tps data with options
      }}
    if(!is.null(vals$go_example_1)){ 
      
      vals$tps_rx_upload_state <- 'example'
      temp_LMs <- plethspecies$land 
      if(input$neg_lms) {
        temp_LMs[which(temp_LMs < 0)] <- NA
      }
      if(input$spec_id == "imageID") {
        fake_names_image <- paste(rep("Image", dim(temp_LMs)[3]), 1:(dim(temp_LMs)[3]), sep = "_")
        dimnames(temp_LMs)[[3]] <- fake_names_image
      } else {
        if(input$spec_id == "None") {
          dimnames(temp_LMs)[[3]] <- 1:(dim(temp_LMs)[3])
        } else { # if input$spec_id == "ID"
          temp_LMs <- temp_LMs[,,match(plethspecies$phy$tip.label, dimnames(temp_LMs)[[3]])]
          
        }
      }
    }
    
    dimnames(temp_LMs)[[1]] <- 1:(dim(temp_LMs)[1]) # adding the appropriate label so that the point_clicks work to define the links
    dimnames(temp_LMs)[[2]] <- c("X", "Y", "Z")[1:(dim(temp_LMs)[2])] # labeling the dimensions in both scenarios of lms being 2 or 3 dimensions
    dimnames(temp_LMs)[[3]] <- str_replace_all(dimnames(temp_LMs)[[3]], " ", "_")  # make all names separated by underscores (matching most phylogeny inputs). 
    
    if(!is.null(isolate(update_vals$remove_outlier_specimen_unselected))){
      temp_LMs <- temp_LMs[,,-which(dimnames(temp_LMs)[[3]] %in% isolate(update_vals$remove_outlier_specimen_unselected))]
    }
    
    return(temp_LMs) # this is when the button for example data
  }) 
  
  tryObserveEvent(input$file_tps, {
    if(!is.null(input$file_tps)) {
      vals$tps_rx_upload_state <- 'uploaded'
    }
  })
  
  tryObserveEvent(lms_rx(), ignoreInit = T, {
    req(vals$tps_rx_upload_state)
    
    if(vals$tps_rx_upload_state == "reset") { 
      vals$lms_rx <- NULL } else { 
        vals$lms_rx <- lms_rx()
      }
  })
  
  phy_rx <- reactive({ # phylogeny reactive element
    temp_phy <- NULL 
    dd <- input$go_remove_outlier_specimen_reset #trigger dddd
    if(is.null(vals$go_example_1)){ # if the example button has not been pressed
      if(!is.null(input$file_phy)) {
        inFile <- input$file_phy
        if (endsWith(inFile$name, '.nexus') | endsWith(inFile$name, '.nex')){
          temp_phy <- read.nexus(inFile$datapath)
        } else if (endsWith(inFile$name, '.tre')){
          temp_phy <- read.tree(inFile$datapath) # read in the tree
        }
      } else { return(isolate(vals$phy_rx))}
    }
    if(!is.null(vals$go_example_1)){ 
      vals$phy_rx_upload_state <- "example"
      temp_phy <- plethspecies$phy 
    }
    if(!is.null(temp_phy)) {
      if(!is.null(isolate(update_vals$remove_outlier_specimen_unselected))){
        temp_phy <- drop.tip(temp_phy, isolate(update_vals$remove_outlier_specimen_unselected))
      }
      showTab(inputId = "tab_shapepatterns", "Phylogenetic Signal")
      return(temp_phy)
    } else { 
      hideTab(inputId = "tab_shapepatterns", "Phylogenetic Signal")
      return(NULL)    }
  }) 
  
  tryObserveEvent(input$file_phy, {
    vals$phy_rx_upload_state <- 'uploaded'
  })
  
  
  tryObserveEvent(phy_rx(), ignoreInit = T, {
    req(vals$phy_rx_upload_state)
    if(vals$phy_rx_upload_state == "reset") { vals$phy_rx <- NULL } else {
      vals$phy_rx <- phy_rx() 
    }
  })
  
  
  trait_rx <- reactive({
    trait_table <- NULL
    dd <- input$go_remove_outlier_specimen_reset #trigger dddd
    if(is.null(isolate(update_vals$trait_column_selected))) { trait_column <- as.numeric(input$trait_column) } else {
      trait_column <- as.numeric(isolate(update_vals$trait_column_selected)) }
    
    if(is.null(vals$go_example_1)){ # if the example button has not been pressed
      if(!is.null(input$file_trait)) {
        inFile <- input$file_trait
        if (endsWith(inFile$name, '.xlsx') | endsWith(inFile$name, '.xls')| 
            endsWith(inFile$name, '.XLSX') | endsWith(inFile$name, '.XLS')){
          trait_raw <- readxl::read_excel(inFile$datapath)
          trait_raw <- as.data.frame(trait_raw)
        } else if (endsWith(inFile$name, '.csv') | endsWith(inFile$name, '.CSV')){
          trait_raw <- read.csv(inFile$datapath) # read in trait file (this cant be from the trait_rx or else the reactives become circular)
        }  # read in trait file
        
        trait_raw[,1] <- str_replace_all(trait_raw[,1], " ", "_") # make the labels spaced with _ to match other data types
        trait_table <- as.matrix(trait_raw[,c(1,trait_column)]) # include the selected trait columns 
        
        #if(!is.null(isolate(vals$outlier_removed_names))){ # necessary for bookmarking if more than one trait_column selected and something was dropped
        #  trait_table <- trait_table[-match(isolate(vals$outlier_removed_names), trait_table[,1]),]
        #}
      }
    }
    if(!is.null(vals$go_example_1)){
      vals$trait_rx_upload_state <- 'example'
      trait_table <- as.matrix(example_mat[,c(1,trait_column)])
      #if(!is.null(isolate(vals$outlier_removed_names))){ # necessary for bookmarking if more than one trait_column selected and something was dropped
      #  trait_table <- trait_table[-match(isolate(vals$outlier_removed_names), trait_table[,1]),]
      #}
    }
    if(!is.null(trait_table)) {
      if(!is.null(isolate(update_vals$remove_outlier_specimen_unselected))){
        trait_table <- trait_table[-which(trait_table[,1] %in% isolate(update_vals$remove_outlier_specimen_unselected)),]
      }
      
      if(ncol(trait_table) > 1) {
        if(ncol(trait_table) > 4) { trait_table <- trait_table[,1:4] }
        if(input$trait_1_transformation != "raw" |
           input$trait_2_transformation != "raw"  | 
           input$trait_3_transformation != "raw" ) {
          trait_table_transformed <- trait_table
          
          if(input$trait_1_transformation == "log") { trait_table_transformed[,2] <- log(as.numeric(trait_table[,2])) }
          if(input$trait_1_transformation == "sqrt") { trait_table_transformed[,2] <- sqrt(as.numeric(trait_table[,2])) }
          if(ncol(trait_table)>2){
            if(input$trait_2_transformation == "log") { trait_table_transformed[,3] <- log(as.numeric(trait_table[,3])) }
            if(input$trait_2_transformation == "sqrt") { trait_table_transformed[,3] <- sqrt(as.numeric(trait_table[,3])) }
            if(ncol(trait_table)>3) {
              if(input$trait_3_transformation == "log") { trait_table_transformed[,4] <- log(as.numeric(trait_table[,4])) }
              if(input$trait_3_transformation == "sqrt") { trait_table_transformed[,4] <- sqrt(as.numeric(trait_table[,4])) }
            }
          }
          
          return(trait_table_transformed)
        } else { return(trait_table) }
      } else { return(trait_table)  }
    } else { return(NULL) }
  })
  
  tryObserveEvent(input$file_trait, {
    vals$trait_rx_upload_state <- 'uploaded'
  })
  
  tryObserve({
    req(vals$trait_rx_upload_state)
    if(vals$trait_rx_upload_state == "reset") { vals$trait_rx <- NULL } else {
      vals$trait_rx <- trait_rx() 
    }
  })
  
  prune_both_ways <- reactive({list(vals$outlier_removed_names, vals$go_pruning,
                                    vals$unprune)})
  
  tryObserveEvent(eventExpr = prune_both_ways(), ignoreInit = T, {
    
    req(vals$lms_rx)
    drop_these <- NULL
    
    if(!is.null(vals$outlier_removed_names)) { 
      
      drop_these <- c(drop_these, vals$outlier_removed_names) 
      
      if(!is.null(vals$phy_rx)) {
        vals$phy_rx <- drop.tip(vals$phy_rx, tip = drop_these)
        if(!is.null(vals$trait_rx)) {
          vals$trait_rx <- vals$trait_rx[match(vals$phy_rx$tip.label, vals$trait_rx[,1]),]
        }
        if(!is.null(vals$lms_rx)) {
          vals$lms_rx <- vals$lms_rx[,,match(vals$phy_rx$tip.label,dimnames(vals$lms_rx)[[3]])]
        }
      } else {
        if(!is.null(vals$lms_rx)) {
          vals$lms_rx <- vals$lms_rx[,,-match(drop_these,dimnames(vals$lms_rx)[[3]])] 
          if(!is.null(vals$trait_rx)) {
            vals$trait_rx <- vals$trait_rx[match(dimnames(vals$lms_rx)[[3]], vals$trait_rx[,1]),] 
          }
          
        } else {
          if(!is.null(vals$trait_rx)) {
            vals$trait_rx <- vals$trait_rx[-match(drop_these, vals$trait_rx[,1]),]
          }
        } 
      }
      
    } else {
      #if(!is.null(trait_rx())) { vals$trait_rx <- trait_rx() }
      #if(!is.null(phy_rx())) { vals$phy_rx <- phy_rx() }
      #if(!is.null(lms_rx())) { vals$lms_rx <- lms_rx() }
      
    }
    
    datasets <- c(!is.null(vals$trait_rx), !is.null(vals$phy_rx), !is.null(vals$lms_rx))
    
    if(length(which(datasets == T)) > 1 & !is.null(vals$go_pruning)) { # if there is more than one trait file
      if(is.null(vals$trait_rx)) { # if no trait file
        keep_these <- intersect(dimnames(vals$lms_rx)[[3]], vals$phy_rx$tip.label) #
        if(!is.null(vals$outlier_removed_names)) {
          keep_these <- keep_these[-match(vals$outlier_removed_names,keep_these)]
        }
        
        if(length(keep_these) == 0) {
          alert_vals$prune_warning <- "remove_all_species"
        } else {
          vals$phy_rx <- keep.tip(vals$phy_rx, tip = keep_these)
          vals$lms_rx <- vals$lms_rx[,,match(vals$phy_rx$tip.label, dimnames(vals$lms_rx)[[3]])] #  
        }
      }
      if(is.null(vals$phy_rx)) { # if no phy file
        keep_these <- intersect(vals$trait_rx[,1], dimnames(vals$lms_rx)[[3]]) #  
        
        if(length(keep_these) == 0) {
          alert_vals$prune_warning <- "remove_all_species"
        } else {
          vals$trait_rx <- vals$trait_rx[match(keep_these, vals$trait_rx[,1]),]
          vals$lms_rx <- vals$lms_rx[,,match(keep_these, dimnames(vals$lms_rx)[[3]])] #  
        } 
        
      }
      if(is.null(vals$lms_rx)) { # if no lm file
        keep_these <- intersect(vals$trait_rx[,1], vals$phy_rx$tip.label)
        
        if(length(keep_these) == 0) {
          alert_vals$prune_warning <- "remove_all_species"
        } else {
          vals$phy_rx <- keep.tip(vals$phy_rx, tip = keep_these)
          vals$trait_rx <- vals$trait_rx[match(vals$phy_rx$tip.label, vals$trait_rx[,1]),]
        } 
        
      }
      
      if(!is.null(vals$trait_rx) & !is.null(vals$phy_rx) & !is.null(vals$lms_rx)) {
        keep_these <- Reduce(intersect, list(dimnames(vals$lms_rx)[[3]], vals$phy_rx$tip.label, vals$trait_rx[,1])) #  
        
        if(length(keep_these) == 0) {
          alert_vals$prune_warning <- "remove_all_species"
        } else {
          vals$phy_rx <- keep.tip(vals$phy_rx, tip = keep_these)
          vals$trait_rx <- vals$trait_rx[match(vals$phy_rx$tip.label, vals$trait_rx[,1]),]
          vals$lms_rx <- vals$lms_rx[,,match(vals$phy_rx$tip.label, dimnames(vals$lms_rx)[[3]])] #  
          
        }
      }
    }
  })
  
  tryObserveEvent(eventExpr = vals$go_run_gpa, ignoreInit = T, {
    if(!is.null(vals$go_run_gpa)){
      req(vals$curves_final_anyNAs)
      if(vals$curves_final_anyNAs) {
        alert_vals$gpa_not_ready <- T
      } else {
        alert_vals$gpa_not_ready <- F
      }
    }
  })
  
  
  gpa_coords_rx <- reactive({ # this reactive helps with whether or not the data should be aligned 
    req(vals$lms_rx)
    
    vals$estimates_cancelled <- NULL
    if(input$raw_lms_already_aligned == F) {
      if (!is.null(vals$curves)) {
        curves <- as.matrix(vals$curves)
        curves.long <- as.integer(curves)
        curve.mat <- matrix(curves.long, ncol = 3, byrow = F)
        colnames(curve.mat) <- c("before", "slide", "after")
      } else { curve.mat <- NULL}
      
      if(anyNA(vals$lms_rx)){ # if any need estimating because they're nas or marked as nas because neg_lm option
        
        # Testing whether NAs are in the correct placement for estimating missing landmarks
        
        these <- which(is.na(vals$lms_rx) == TRUE)
        even_numb_nas <- (length(these) %% 2 == 0)
        
        if(even_numb_nas) {
          these_mat <- matrix(these, ncol = 2, byrow = T)
          spec_numb <- ceiling(these/(dim(vals$lms_rx)[1]*dim(vals$lms_rx)[2]))
          mat <- matrix(spec_numb, ncol = 2, byrow = T)
          spec_numb_each <- spec_numb[seq(1, length(spec_numb), 2)]
          spec_numb_unique <- unique(spec_numb_each)
          
          if(length(spec_numb_unique) < length(spec_numb_each)) { # if there are specimens with multiple missing landmarks
            for (i in 1:length(spec_numb_unique)) {
              if(length(which(spec_numb_each %in% spec_numb_unique[i]))>1) {
                these_vec <- these[which(spec_numb %in% spec_numb_unique[i])]
                these_rematted <- matrix(these_vec, ncol = 2, byrow = F)
                these_mat[which(spec_numb_each %in% spec_numb_unique[i]),] <- these_rematted
              } # if there are repeats for ith spec_numb_unique
            }} 
          
          lm_numb_x <- these_mat[,1] -(mat[,1]-1)*dim(vals$lms_rx)[2]*dim(vals$lms_rx)[1] 
          expected_ys <- dim(vals$lms_rx)[1]*dim(vals$lms_rx)[2]*(mat[,1]-1) + (lm_numb_x + dim(vals$lms_rx)[1])
          incorrect_lm <- any(these_mat[,2] != expected_ys)
          
          if(!incorrect_lm) {
            lms_estimated <- estimate.missing(vals$lms_rx, method = "TPS") # change this to be an option
          } else {
            vals$estimates_cancelled <- T
          } 
        } else { 
          vals$estimates_cancelled <- T
        }
      }  else {
        lms_estimated <- vals$lms_rx
      }
      
      if(!is.null(vals$estimates_cancelled)) {
        
        shinyalert(
          title = "Landmark Estimation Error",
          text = "The placement of the negative landmarks 
          is not in the appropriate format. Both the x and y dimensions of each missing landmark
          must be marked as negative or NA for the 
          function <span style='font-family: Courier New'>estimate.missing</span> to work.",
          type = "warning",
          html = T,
          size = "s")
        
        lms_final <- vals$lms_rx
        updateRadioButtons(session, inputId = "neg_lms", "Negative LMs Indicate:", # option to indicate whether negative LMs should be taken as missing data or as real negative values
                           choices = c("True LMs" = FALSE, "Missing Data" = TRUE), 
                           selected = FALSE) # doesnt need a bookmarking workaround
        
        
      } else {
        vals$Data_gpa <- gpagen(lms_estimated, curves = curve.mat)
        lms_final <- vals$Data_gpa$coords
        vals$csize <- vals$Data_gpa$Csize 
        names(vals$csize) <- dimnames(lms_final)[[3]]
      } 
    } else {
      lms_final <- vals$lms_rx # pass lms straight through
      vals$csize <- NULL
    }
    if(!anyNA(lms_final)) { 
      vals$mshape_gpacoords_rx <- mshape(lms_final) # also making a mean shape item so modularity doesnt take so long
      return(lms_final) 
    } else { return(NULL) } 
  }) 
  
  run_gpa_reset_listen <- reactive({ list(gpa_coords_rx(), vals$curves_final_anyNAs)})
  tryObserveEvent(eventExpr =run_gpa_reset_listen(), ignoreInit = T, { # making the Run GPA button reset if you change anything that might affect it
    updateActionButton(session, "go_run_gpa")
  })
  
  #### Processing data in various ways for analyses and visualization ####
  
  pca_nonphylo_rx <- reactive({
    req(gpa_coords_rx()) # dont run until gpa_coords defined
    prcomp(two.d.array(gpa_coords_rx())) # run a prcomp for when phylogeny is not being shown on morphospace
  })
  
  pca_rx <- reactive({
    req(gpa_coords_rx()) # dont run until gpa_coords defined
    if(datasets_dont_match() == FALSE) {
      geomorph:::gm.prcomp(gpa_coords_rx(), vals$phy_rx, GLS = input$gls_center_tf,
                           align.to.phy = input$align_to_phy_tf, 
                           transform = as.logical(input$transform_resid_tf)) # gm.prcomp for phylomorphospace
    }
  })
  
  x_lab_rx <- reactive({ paste("PC", input$pca_x_axis, sep="") }) # making a reactive for which pc axes to display
  
  y_lab_rx <- reactive({ paste("PC", input$pca_y_axis, sep="") }) # making a reactive for which pc axes to display
  
  # Trait Treatment for Input Settings
  
  tryObserve(priority = -100, {
    req(input$trait_column)
    
    if(is.null(isolate(update_vals$trait_1_treatment_selected))){
      x <- c(input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment)
    } else {
      x <- c(isolate(update_vals$trait_1_treatment_selected), 
             isolate(update_vals$trait_2_treatment_selected), 
             isolate(update_vals$trait_3_treatment_selected))
    }
    
    xx <- x[1:length(input$trait_column)]
    
    if(length(which(xx=="disc"))>0) {vals$one_disc_traits <- "one_trait"} else {vals$one_disc_traits <- NULL}
    if(length(which(xx=="disc"))>1) {vals$two_disc_traits <- "two_traits"} else {vals$two_disc_traits <- NULL}
    if(length(which(xx=="disc"))>2) {vals$three_disc_traits <- "three_traits"} else {vals$three_disc_traits <- NULL}
    if(length(input$trait_column) == 1) {vals$two_disc_traits <- vals$three_disc_traits <- NULL}
  })
  
  
  # Morphospace Tip Color
  tryObserve({
    if (input$tip_col_category == "by_trait_1" | input$tip_col_category == "by_trait_2" | input$tip_col_category == "by_trait_3" | input$tip_col_category == "csize") {
      if (input$tip_col_category == "by_trait_1" & input$trait_1_treatment == "disc") {i <- 1} 
      if (input$tip_col_category == "by_trait_2" & input$trait_2_treatment == "disc") {i <- 2}
      if (input$tip_col_category == "by_trait_3" & input$trait_3_treatment == "disc") {i <- 3}
      if ((input$tip_col_category == "by_trait_1" & input$trait_1_treatment == "cont") |
          (input$tip_col_category == "by_trait_2" & input$trait_2_treatment == "cont") |
          (input$tip_col_category == "by_trait_3" & input$trait_3_treatment == "cont") |
          input$tip_col_category == "csize") {i <- "continuous_color"}
      if (i == 1 | i == 2 | i == 3) { # if the selected trait is discrete
        tip_col_fac <- as.character(vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),i+1]) 
        lev <- length(unique(tip_col_fac))
        lev_options <- unique(tip_col_fac)
        if(lev > 10) {showNotification("You are attempting to color points by a trait with more than 10 levels, 
                                        which is beyond the functionality of this app. If this is a continuous trait, 
                                        adjust the trait treatment in the Data Input tab accordingly.", 
                                       duration = 30, type = "warning")} 
        col_options <- c(input$trait_colors_lev1, input$trait_colors_lev2, input$trait_colors_lev3,
                         input$trait_colors_lev4, input$trait_colors_lev5, input$trait_colors_lev6,
                         input$trait_colors_lev7, input$trait_colors_lev8, input$trait_colors_lev9,
                         input$trait_colors_lev10, input$trait_colors_lev11)[1:lev]
        for (j in 1:lev){ tip_col_fac[which(tip_col_fac == lev_options[j])] <- col_options[j] }
        vals$tip_col <- tip_col_fac
      } else {
        if(input$tip_col_category == "csize") {
          tip_col_cont <- as.numeric(vals$csize)
        }
        if (input$tip_col_category == "by_trait_1") {
          tip_col_cont <- as.numeric(vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),2])
        }
        if (input$tip_col_category == "by_trait_2") {
          tip_col_cont <- as.numeric(vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),3])
        }
        if (input$tip_col_category == "by_trait_3") {
          tip_col_cont <- as.numeric(vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),4])
        }
        palette <- colorRampPalette(colors = c(input$trait_colors_lev1, input$trait_colors_lev2))
        ordered_col <- findInterval(tip_col_cont, sort(tip_col_cont))
        vals$tip_col <- palette(length(tip_col_cont))[ordered_col]
      }  
    }
    if(input$tip_col_category != "by_trait_1" & input$tip_col_category != "by_trait_2" & input$tip_col_category != "by_trait_3" & input$tip_col_category != "csize" & input$tip_col_other_tf == "FALSE")  { vals$tip_col <- (as.character(input$tip_col)) }
    if(input$tip_col_other_tf == "TRUE")  { vals$tip_col <- (as.character(input$tip_col_other)) }
  })
  
  # Morphospace Tip Shape
  tryObserve({
    if (input$tip_pch == "by_trait_1"| input$tip_pch == "by_trait_2" | input$tip_pch == "by_trait_3") {
      if (input$tip_pch == "by_trait_1") {i <- 2} 
      if (input$tip_pch == "by_trait_2") {i <- 3}
      if (input$tip_pch == "by_trait_3") {i <- 4}
      tip_pch_fac <- as.character(vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),i]) # reordering necessary for correct grouping
      lev <- length(unique(tip_pch_fac))
      if (lev < 20) { # if there are few enough factors to shape by trait
        lev_options <- unique(tip_pch_fac)
        pch_options <- c(19,1,15,0,8,3,2,4,5,6,7,9,10,11,12,13,14,16,17,18)[1:lev]
        for (j in 1:lev){ tip_pch_fac[which(tip_pch_fac == lev_options[j])] <- pch_options[j] }
        vals$tip_pch <- tip_pch_fac
      } else { vals$tip_pch <- 4 } # if not, use Xs (pch = 4) as an error indicator that you're trying to use point shape on a variable with too many levels
    }
    if(input$tip_pch == "19" | input$tip_pch == "1" | input$tip_pch == "18") { vals$tip_pch <- input$tip_pch }
  })
  
  
  #### Adjusting Reactive Values for Conditional Paneling (T/F) ####
  
  gridPar_vals <- reactive({
    x <- gridPar(pt.bg = input$warp_pt_bg, pt.size = as.numeric(input$warp_pt_size), 
                 link.col = input$warp_link_col, link.lwd = as.numeric(input$warp_link_lwd), link.lty = as.numeric(input$warp_link_lty), 
                 #out.col = input$warp_out_col, out.cex = as.numeric(input$warp_out_cex),  # Needs shape outline, functionality to come
                 tar.pt.bg = input$warp_tar_pt_bg,  tar.pt.size = as.numeric(input$warp_tar_pt_size), 
                 tar.link.col = input$warp_tar_link_col, tar.link.lwd = as.numeric(input$warp_tar_link_lwd), tar.link.lty = as.numeric(input$warp_tar_link_lty), 
                 #tar.out.col = input$warp_tar_out_col, tar.out.cex = as.numeric(input$warp_tar_out_cex), # Needs shape outline, functionality to come 
                 n.col.cell = as.numeric(input$warp_n_col_cell), 
                 grid.col = input$warp_grid_col, grid.lwd = as.numeric(input$warp_grid_lwd), grid.lty = as.numeric(input$warp_grid_lty), 
                 #txt.adj = as.numeric(input$warp_txt_adj), # non-functional
                 txt.pos = as.numeric(input$warp_txt_pos), txt.cex = input$warp_txt_cex, txt.col = input$warp_txt_col)
    return(x)
  })
  
  output$example_tps_selected <- reactive({return(!is.null(vals$go_example_1))}) # this output allows for appropriate options to be available when using example pleth data
  outputOptions(output, 'example_tps_selected', suspendWhenHidden=FALSE)
  
  output$file_tps_selected <- reactive({return(!is.null(vals$lms_rx))}) # output for conditional paneling
  outputOptions(output, 'file_tps_selected', suspendWhenHidden=FALSE) # ??? consolidate all these outputOptions? necessary for suspendWhenHidden?
  
  output$file_phy_selected <- reactive({return(!is.null(vals$phy_rx))})
  outputOptions(output, 'file_phy_selected', suspendWhenHidden=FALSE)
  
  output$file_trait_selected <- reactive({return(!is.null(vals$trait_rx))})
  outputOptions(output, 'file_trait_selected', suspendWhenHidden=FALSE)
  
  output$any_traits_selected <- reactive({return(!is.null(trait_rx()))})
  outputOptions(output, 'any_traits_selected', suspendWhenHidden=FALSE)
  
  output$two_traits_selected <- reactive({return(length(input$trait_column) > 1)})
  outputOptions(output, 'two_traits_selected', suspendWhenHidden=FALSE)
  
  output$three_traits_selected <- reactive({return(length(input$trait_column) > 2)})
  outputOptions(output, 'three_traits_selected', suspendWhenHidden=FALSE)
  
  output$one_disc_traits_selected <- reactive({return(!is.null(vals$one_disc_traits) & !is.null(vals$trait_rx))})
  outputOptions(output, 'one_disc_traits_selected', suspendWhenHidden=FALSE)
  
  output$two_disc_traits_selected <- reactive({return(!is.null(vals$two_disc_traits))})
  outputOptions(output, 'two_disc_traits_selected', suspendWhenHidden=FALSE)
  
  output$three_disc_traits_selected <- reactive({return(!is.null(vals$three_disc_traits))})
  outputOptions(output, 'three_disc_traits_selected', suspendWhenHidden=FALSE)
  
  output$outlier_selected <- reactive({return(!is.null(vals$outlier_row))})
  outputOptions(output, 'outlier_selected', suspendWhenHidden=FALSE)
  
  output$outlier_removed <- reactive({return(!is.null(vals$outlier_removed_names) | !is.null(isolate(update_vals$remove_outlier_specimen_unselected)))})
  outputOptions(output, "outlier_removed", suspendWhenHidden = F)
  
  output$show_symmetry_file <- reactive({return(!is.null(vals$go_symmetry_file))})
  outputOptions(output, "show_symmetry_file", suspendWhenHidden = F)
  
  output$symmetry_pattern <- reactive({return(!is.null(vals$go_symmetry_pattern))})
  outputOptions(output, 'symmetry_pattern', suspendWhenHidden=FALSE)
  
  output$symmetry_land_pairs_file_input <- reactive({return(!is.null(input$symmetry_land_pairs_file_input))})
  outputOptions(output, "symmetry_land_pairs_file_input", suspendWhenHidden = F)
  
  output$show_symmetry_definitions <- reactive({return(!is.null(vals$go_symmetry_file) | 
                                                         !is.null(vals$go_symmetry_pattern) | 
                                                         !is.null(vals$go_show_specimen_assignments))})
  outputOptions(output, "show_symmetry_definitions", suspendWhenHidden = F)
  
  output$symmetry_initiated <- reactive({return(input$run_symmetry_go > 0)})
  outputOptions(output, "symmetry_initiated", suspendWhenHidden = F)
  
  
  output$symmetry_replicate <- reactive({return(input$symmetry_replicate != 0)})
  outputOptions(output, 'symmetry_replicate', suspendWhenHidden=FALSE)
  
  output$symmetry_side <- reactive({return(input$symmetry_side != 0)})
  outputOptions(output, 'symmetry_side', suspendWhenHidden=FALSE)
  
  output$col_n_levels <- reactive({
    if(!is.null(vals$color_options_list)){
      return(length(unlist(vals$color_options_list)))
    } else {return(0)}
  })
  outputOptions(output, 'col_n_levels', suspendWhenHidden=FALSE)
  
  output$morpho_click_initiated <- reactive({return(!is.null(vals$warp_initiated))}) # for conditional paneling when morphospace clicked (for warp grid)
  outputOptions(output, 'morpho_click_initiated', suspendWhenHidden=FALSE)
  
  output$morpho_dbclick_initiated <- reactive({return(!is.null(vals$morpho_dbclicked))}) # for conditional paneling for warp grid options
  outputOptions(output, 'morpho_dbclick_initiated', suspendWhenHidden=FALSE)
  
  output$links_dbclick_initiated <- reactive({return(!is.null(vals$links_dbclicked))}) # for conditional paneling when plotAllSpecimens is double clicked to show the link reset button
  outputOptions(output, 'links_dbclick_initiated', suspendWhenHidden=FALSE)
  
  output$semis_initiated <- reactive({return(nrow(vals$curves_final) > 0)}) # for conditional paneling to display semis reset button 
  outputOptions(output, 'semis_initiated', suspendWhenHidden=FALSE)
  
  datasets_dont_match <- reactive({
    if(is.null(vals$lms_rx)) {  return(FALSE) } else { # making the button stay hidden if we've used the example dataset and then hit clear
      if(!is.null(vals$phy_rx)){ # if there is a phylogeny
        if(!is.null(vals$trait_rx)) { # if there is also a trait file
          !(identical(vals$trait_rx[,1], dimnames(vals$lms_rx)[[3]]) & 
              identical(dimnames(vals$lms_rx)[[3]], vals$phy_rx$tip.label)) # return T/F of whether the data match
        } else { # if only the phy file is uploaded
          !identical(dimnames(vals$lms_rx)[[3]], vals$phy_rx$tip.label) # return T/F of whether the data match
        }
      } else {
        if(!is.null(vals$trait_rx)) { # if only the trait file is uploaded
          
          !identical(vals$trait_rx[,1], dimnames(vals$lms_rx)[[3]]) # return T/F of whether the data match
        } else {
          return(FALSE) } # if neither file is uploaded, return a FALSE ??? unsure
      }
    }
  })
  
  output$datasets_dont_match <- reactive({ datasets_dont_match() })
  outputOptions(output, 'datasets_dont_match', suspendWhenHidden=FALSE)
  
  output$show_gpa <- reactive({
    if(!is.null(vals$curves_final_anyNAs)) {
      show <- (vals$curves_final_anyNAs == F)
    } else { show <- T }
    return(!is.null(vals$go_run_gpa) & show)
  })
  outputOptions(output, 'show_gpa', suspendWhenHidden=FALSE)
  
  output$multiple_terms_selected <- reactive({
    req(input$independent_variables)
    if(!is.null(vals$trigger_trait_order_hide)) {
      if(vals$trigger_trait_order_hide) {
        return(F)
      } else { return(length(input$independent_variables) > 1) }
    } else { return(length(input$independent_variables) > 1) }
  })
  outputOptions(output, 'multiple_terms_selected', suspendWhenHidden=FALSE)
  
  output$multiple_terms_selected_model_1 <- reactive({
    req(input$independent_variables_model_1)
    return(length(input$independent_variables_model_1) > 1)
  })
  outputOptions(output, 'multiple_terms_selected_model_1', suspendWhenHidden=FALSE)
  
  output$multiple_terms_selected_model_2 <- reactive({
    req(input$independent_variables_model_2)
    return(length(input$independent_variables_model_2) > 1)
  })
  outputOptions(output, 'multiple_terms_selected_model_2', suspendWhenHidden=FALSE)
  
  output$multiple_terms_selected_model_3 <- reactive({
    req(input$independent_variables_model_3)
    return(length(input$independent_variables_model_3) > 1)
  })
  outputOptions(output, 'multiple_terms_selected_model_3', suspendWhenHidden=FALSE)
  
  #### tryObserveEvents for Example Data, Prune Datasets, and Clear Inputs Buttons #### 
  
  tryObserveEvent(eventExpr = input$go_example, ignoreInit = F, {
    if(input$go_example > 0 ) {
      vals$go_example_1 <- c(vals$go_example_1, "go")      
    } else { vals$go_example_1 <- NULL}
  })
  
  tryObserveEvent(eventExpr =input$go_pruning, ignoreInit = F, priority = 10, {
    if(input$go_pruning > 0) {
      vals$go_pruning <- c(vals$go_pruning, "go")      
    } else { vals$go_pruning <- NULL }
  })
  
  tryObserveEvent(eventExpr = input$go_run_anova, ignoreInit = F, {
    if(input$go_run_anova > 0) {
      vals$go_run_anova <- c(vals$go_run_anova, "go")
    } else { vals$go_run_anova <- NULL }
  })
  
  tryObserveEvent(eventExpr = input$go_run_model_comparison, ignoreInit = F, {
    if(input$go_run_model_comparison > 0) {
      vals$go_run_model_comparison <- c(vals$go_run_model_comparison, "go")
    } else { vals$go_run_model_comparison <- NULL }
  })
  
  go_run_gpa_listen <- reactive({list(update_vals$go_run_gpa_selected, input$go_run_gpa)})
  tryObserveEvent(eventExpr = go_run_gpa_listen(), ignoreInit = F, {
    if(input$go_run_gpa>0) {
      vals$go_run_gpa <- c(vals$go_run_gpa, "go")
    } else {
      if(!is.null(update_vals$go_run_gpa_selected)){
        if(update_vals$go_run_gpa_selected > 0) {
          vals$go_run_gpa <- c(vals$go_run_gpa, "go")
        }
      }
    }
  })
  
  tryObserveEvent(eventExpr = vals$go_example_1, priority = 10000, ignoreInit = F, { # example dataset button
    if(!is.null(vals$go_example_1)) {
      names_reduced <- names(vals)
      names_reduced <- names_reduced[-match("go_example_1", names_reduced)]
      for(i in names_reduced) {
        vals[[i]] <- NULL 
      }
      vals$tip_col <- "black"
    }
  })
  
  tryObserveEvent(eventExpr = input$go_file_reset, ignoreInit = T, {   
    req(vals)
    for(i in names(vals)) vals[[i]] <- NULL  
    
    vals$tip_pch <- 19
    vals$tip_col <- "black"
    
    reset("trait_column")
    reset("file_tps")
    reset("file_phy")
    reset("file_trait")
    reset("go_run_gpa")
    reset("spec_id")
    reset("neg_lms")
    
    reset("pca_x_axis")
    reset("pca_y_axis")
    reset("tip_col")
    reset("tip_pch")
    reset("tip_cex")
    reset("tip_txt_cex")
    reset("include_phylo")
    reset("show_node_label")
    reset("show_tip_label")
    reset("show_convex_hull_1")
    reset("show_convex_hull_2") 
    reset("show_convex_hull_3")
    reset("warp_mag")
    reset("warp_comparison_start")
    reset("warp_comparison_end")
    reset("warp_comparison_show_arrow")
    reset("warp_var_displayed")
    reset("warp_axes_selected")
    
    reset("pgls_ols")
    reset("trait_pairwise")
    reset("independent_variables")
    reset("interactions_included")
    reset("disparity_groups")
    reset("evol_rate_groups")
    reset("anova_perm")
    reset("ss_type")
    
    
    reset("go_example")
    reset("go_pruning")
    
    reset("show_tip_label_phy_preview")
    reset("raw_lms_already_aligned")
    reset("trait_1_treatment")
    reset("trait_2_treatment")
    reset("trait_3_treatment")
    reset("trait_1_transformation")
    reset("trait_2_transformation")
    reset("trait_3_transformation")
    reset("apply_modular_groups_go")
    
    reset("semilms_apply")
    reset("semilms_manual")
    reset("semilms_color_other")
    reset("semilms_color_brackets")
    reset("semilms_color")
    reset("semilms_color_labels")
    reset("semilms_color_individlms")
    reset("semilms_color_links")
    reset("link_click_initiated")
    reset("link_click_end")
    reset("link_reset")
    
    reset("semilms_reset")
    reset("semis_selected")
    reset("semilms_manual_input")
    reset("semilms_upload_file_input")
    reset("outlier_selected")
    reset("outlier_group")
    reset("outlier_group_tf")
    reset("outlier_group_level_plotted")
    reset("outlier_plot_pt_cex")
    reset("outlier_plot_txt_cex")
    reset("outlier_plot_show_point_names_tf")
    reset("vis_outliers_color")
    reset("vis_outliers_color_links")
    reset("vis_outliers_color_labels")
    reset("vis_outliers_color_other")
    reset("go_remove_outlier_specimen_reset")
    reset("go_remove_outlier_specimen")
    #reset("remove_outlier_specimen")
    
    reset("phy_signal_input")
    reset("signal_perm")
    reset("modularity_group_1")
    reset("modularity_group_2")
    reset("modularity_group_3")
    reset("modularity_group_4")
    reset("modularity_group_5")
    reset("modularity_group_6")
    reset("modularity_group_7")
    reset("modularity_group_8")
    reset("modularity_n_groups")
    reset("modularity_phylo_tf")
    reset("modularity_perm")
    reset("module_1_col")
    reset("module_2_col")
    reset("module_3_col")
    reset("module_4_col")
    reset("module_5_col")
    reset("module_6_col")
    reset("module_7_col")
    reset("module_8_col")
    
    reset("plot_modularity_visualization_cex")
    reset("integration_group_by")
    reset("integration_phylo_tf")
    reset("integration_test_perm")
    reset("integration_group_level")
    reset("run_symmetry_go")
    reset("symmetry_definitions")
    reset("symmetry_ind_pattern")
    reset("symmetry_ind_pattern_no")
    reset("symmetry_ind_pattern_nth")
    reset("symmetry_ind_pattern_block")
    reset("symmetry_obj_sym")
    reset("symmetry_land_pairs_pattern")
    reset("symmetry_land_pairs_file_upload")
    reset("symmetry_land_pairs_file_input")
    reset("symmetry_side_pattern")
    reset("symmetry_side")
    reset("symmetry_file_upload")
    reset("symmetry_perm")
    
    reset("go_symmetry_useoutput")
    reset("go_symmetry_file")
    reset("go_symmetry_pattern")
    reset("go_symmetry_manual")
    reset("run_symmetry_go")
    reset("symmetry_replicate")
    
    reset("gls_center_tf")
    reset("align_to_phy_tf")
    reset("transform_resid_tf")
    reset("tip_col_category")
    reset("tip_col_other_tf")
    reset("tip_col_other")
    reset("trait_colors_lev1")
    reset("trait_colors_lev2")
    reset("trait_colors_lev3")
    reset("trait_colors_lev4")
    reset("trait_colors_lev5")
    reset("trait_colors_lev6")    
    reset("trait_colors_lev7")
    reset("trait_colors_lev8")
    reset("trait_colors_lev9")
    reset("trait_colors_lev10")
    reset("trait_colors_lev11")
    
    reset("edge_col")
    reset("edge_width")
    reset("node_pch")
    reset("node_cex")
    reset("node_col")
    reset("node_txt_cex")
    reset("warp_labels")
    reset("warp_type")
    
    
    reset("warp_specific_ref_specimen")
    reset("warp_specific_targ_specimen")
    reset("more_warp_options")
    reset("warp_pt_bg")
    reset("warp_pt_size")
    reset("warp_link_col")
    reset("warp_link_lwd")
    reset("warp_link_lty")
    reset("warp_tar_pt_bg")
    reset("warp_tar_pt_size")
    reset("warp_tar_link_col")
    
    reset("warp_tar_link_lwd")
    reset("warp_tar_link_lty")
    reset("warp_n_col_cell")
    reset("warp_grid_col")
    reset("warp_grid_lwd")
    reset("warp_grid_lty")
    #reset("warp_txt_adj")
    reset("warp_txt_pos")
    reset("warp_txt_cex")
    reset("warp_txt_col")
    
    #reset("independent_variables_order")
    #reset("go_run_model_comparison")
    reset("independent_variables_model_1")
    reset("independent_variables_order_model_1")
    reset("independent_variables_model_2")
    reset("independent_variables_order_model_2")
    reset("independent_variables_model_3")
    reset("independent_variables_order_model_3")
    reset("pgls_ols_model_comparison")
    reset("anova_perm_model_comparison")
    reset("ss_type_model_comparison")
    reset("interactions_included_model_comparison")
    reset("add_third_model")
    
    reset("allometry_predictor")
    reset("allometry_color")
    reset("allometry_type")
    reset("allometry_reg_type")
    reset("allometry_pt_size")
    
    reset("gpa_not_run_alert")
    
    vals$trait_rx_upload_state <- "reset"
    vals$phy_rx_upload_state <- "reset"
    vals$tps_rx_upload_state <- "reset"
    
    hideTab(inputId = "tab_shapepatterns", "Phylogenetic Signal")
    hideTab(inputId = "navbar", target = "Linear Models")
    
  }) # delete replicates !!!!
  
  tryObserveEvent(eventExpr =vals$go_pruning, ignoreInit = F, { 
    req(gpa_coords_rx()) # this only runs if shape data exist
    if(!is.null(vals$go_pruning)) {
      vals$prune_datasets <- "pruned" # initiating lms_rx, phy_rx, and trait_rx to be rerun
    } else {
      vals$prune_datasets <- NULL
    }
  })
  
  tryObserveEvent(eventExpr =input$warp_reset, ignoreInit = T,{ # reset button resets a lot of things including the placement 
    if(input$warp_reset > 0) {
      vals$morpho_clicked <- NULL
      vals$morpho_dbclicked <- NULL
      vals$specimen_row_ref <- NULL
      vals$specimen_row_targ <- NULL
      vals$projection_row <- NULL
      vals$warp_initiated <- NULL
      
      reset("warp_comparison_start")
      reset("warp_comparison_end")
      reset("warp_comparison_show_arrow")
      reset("warp_var_displayed") 
      reset("warp_axes_selected")
      reset("warp_specific_ref_specimen")
      reset("warp_specific_targ_specimen")
      reset("warp_mag")
    }
  })  
  
  #### Dealing with Plot Clicks  #####
  tryObserveEvent(eventExpr = input$semilms_apply, ignoreInit = T, {
    if(is.null(vals$go_example_1)) { # ignore if using example pleth data
      vals$curves <- vals$curves_final # triggers a rerun of gpa_coords_rx()
    }
  })
  
  morphospace_clicked_listen <- reactive({list(input$morphospace_specimen_click, input$warp_comparison_start, input$warp_comparison_end,
                                               update_vals$morphospace_specimen_click_selected)})
  tryObserveEvent(eventExpr = morphospace_clicked_listen(), ignoreInit = F, {
    vals$morpho_clicked <- 1 # initiate the vals for conditional paneling
    rotation_x_df <- as.data.frame(pca_nonphylo_rx()$x) # pca rotation converted into a dataframe for nearPoints function below
    if(is.null(update_vals$morphospace_specimen_click_selected)) { 
      clicked_point <- nearPoints(rotation_x_df, input$morphospace_specimen_click, 
                                  xvar = x_lab_rx(), yvar = y_lab_rx(),
                                  threshold = 20, maxpoints = 1)  # this prints the x and y values of the nearest point (within 20 pixels) to the click on the morphospace
      
    } else { clicked_point <- update_vals$morphospace_specimen_click_selected }
    
    if(length(unlist(clicked_point))>1){
      
      vals$warp_initiated <- "Yes"
      if(input$warp_comparison_end == "selected_obs") { 
        vals$specimen_row_targ <- clicked_point 
      }
      if(input$warp_comparison_start == "selected_obs") {  vals$specimen_row_ref <- clicked_point }
      
    } 
  })
  
  tryObserveEvent(eventExpr =input$morphospace_projection_dbclick, ignoreInit = T, {
    vals$morpho_dbclicked <- 1 # initiate the vals for conditional paneling
    vals$projection_row <- c(input$morphospace_projection_dbclick$x, input$morphospace_projection_dbclick$y) # this prints the exact x and y values where double clicked on the morphospace
    vals$warp_initiated <- "Yes"
  })
  
  tryObserveEvent(vals$warp_initiated, ignoreInit = F, {
    if(!is.null(vals$warp_initiated)) {
      delay(300, session$sendCustomMessage(type = "scrollCallback_warp", 1))     
    }
  })
  
  warp_ref_listen <- reactive({list(input$warp_comparison_start, input$warp_specific_ref_specimen)})
  tryObserveEvent(warp_ref_listen(), ignoreInit = F, {
    if(input$warp_comparison_start == "selected_obs_byname" & !is.null(input$warp_specific_ref_specimen)) {
      rotation_x_df <- as.data.frame(pca_nonphylo_rx()$x)
      coords <- rotation_x_df[match(input$warp_specific_ref_specimen, row.names(rotation_x_df)),]
      names(coords) <- input$warp_specific_ref_specimen 
      vals$specimen_row_ref <- coords
    }
  })
  
  warp_targ_listen <- reactive({list(input$warp_comparison_end, input$warp_specific_targ_specimen)})
  tryObserveEvent(warp_targ_listen(), ignoreInit = T, {
    req(input$warp_comparison_end)
    req(input$warp_specific_targ_specimen)
    if(input$warp_comparison_end == "selected_obs_byname" & !is.null(input$warp_specific_targ_specimen)) {
      rotation_x_df <- as.data.frame(pca_nonphylo_rx()$x)
      coords <- rotation_x_df[match(input$warp_specific_targ_specimen, row.names(rotation_x_df)),]
      names(coords) <- input$warp_specific_targ_specimen
      vals$specimen_row_targ <- coords
    }
  })
  
  # Selecting the reference and target shapes for warp grid (??? this appears to work, but has yes to be tested exactly)
  ref_rx <- reactive({
    req(gpa_coords_rx())
    if(input$warp_var_displayed == "full_morphospace"){ # comparison is whole morphospace
      if(input$warp_comparison_start == "mean") { 
        return(mshape(gpa_coords_rx()))
      } # if the warp comparison ref is the full morphospace mean
      if(input$warp_comparison_start == "selected_obs" | input$warp_comparison_start == "selected_obs_byname" ) {
        req(vals$specimen_row_ref)
        temp_pc <- pca_rx()$x
        which_row <- match(row.names(vals$specimen_row_ref), row.names(temp_pc)) # which specimen was clicked matches the pca
        temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, ref = temp_pc[which_row,]) 
        return(temp_pc_pred$ref)
      }
      if(input$warp_comparison_start == "selected_proj") {
        temp_pc <- pca_rx()$x[,c(as.numeric(input$pca_x_axis), as.numeric(input$pca_y_axis))] # this is limited to the 2 pc axes visualized because thats all thats possible with the theoretical part of morphospace
        temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, ref = vals$projection_row) # which x and y values were double clicked as projection
        return(temp_pc_pred$ref)
      }
    }
    if(input$warp_var_displayed == "selected_axes"){ # comparison is just across specific axes
      if(input$warp_comparison_start == "mean") {
        temp_pc <- pca_rx()$x[,as.numeric(input$warp_axes_selected)]
        if(is.null(dim(temp_pc))) { 
          temp_pc_pred <- shape.predictor(gpa_coords_rx(), x = temp_pc, Intercept = FALSE, ref = mean(temp_pc))
        } else { temp_pc_pred <- shape.predictor(gpa_coords_rx(), x = temp_pc, Intercept = FALSE, ref = colMeans(temp_pc)) }
      }
      if(input$warp_comparison_start == "selected_obs"| input$warp_comparison_start == "selected_obs_byname" ) {
        req(vals$specimen_row_ref)
        temp_pc <- pca_rx()$x[,as.numeric(input$warp_axes_selected)] 
        if(is.null(dim(temp_pc))) { # this if else is about whether the selected axes form a vector or a matrix
          which_row <- match(row.names(vals$specimen_row_ref), names(temp_pc))
          temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, ref = temp_pc[which_row])
        } else {
          which_row <- match(row.names(vals$specimen_row_ref), row.names(temp_pc))
          temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, ref = temp_pc[which_row,])
        }
      }
      if(input$warp_comparison_start == "selected_proj") {
        included_axes <- intersect(c(as.numeric(input$pca_x_axis), as.numeric(input$pca_y_axis)), as.numeric(input$warp_axes_selected)) # this identifies the shared axes between what pc axes are visible from which the projection was selected and which axes were selected for what variation to show
        temp_pc <- pca_rx()$x[,included_axes] 
        temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, 
                                        ref = vals$projection_row[na.omit(match(c(as.numeric(input$pca_x_axis), as.numeric(input$pca_y_axis)), included_axes))]) # again selecting the axes that are both visualized and selected
      }
      return(temp_pc_pred$ref)
    }
  })
  
  targ_rx <- reactive({
    if(input$warp_var_displayed == "full_morphospace"){
      req(input$warp_comparison_end)
      if(input$warp_comparison_end == "mean") { 
        preds <- shape.predictor(gpa_coords_rx(), x = pca_rx()$x, meanshape = colMeans(pca_rx()$x))
        out <- preds$meanshape
        names(out)<- "Mean Shape"
        return(out)
      }
      if(input$warp_comparison_end == "selected_obs"| input$warp_comparison_end == "selected_obs_byname") {
        req(vals$specimen_row_targ)
        temp_pc <- pca_rx()$x
        which_row <- match(row.names(vals$specimen_row_targ), row.names(temp_pc))
        
        temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, tar = temp_pc[which_row,])
        out <- temp_pc_pred$tar
        names(out)<- row.names(vals$specimen_row_targ)
        return(out)
      }
      if(input$warp_comparison_end == "selected_proj") {
        temp_pc <- pca_rx()$x[,c(as.numeric(input$pca_x_axis), as.numeric(input$pca_y_axis))] 
        temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, tar = vals$projection_row)
        out <- temp_pc_pred$tar
        names(out)<- paste("(", round(vals$projection_row[1], 4), ", ", round(vals$projection_row[2], 4), ")", sep = "")
        return(out)
      }
    }
    if(input$warp_var_displayed == "selected_axes"){
      if(input$warp_comparison_end == "mean") {
        temp_pc <- pca_rx()$x[,as.numeric(input$warp_axes_selected)]
        if(is.null(dim(temp_pc))) { 
          temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, tar = mean(temp_pc)) # dimensionality issue as described in ref reactive above
        } else { temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, tar = colMeans(temp_pc)) }
      }
      if(input$warp_comparison_end == "selected_obs"| input$warp_comparison_end == "selected_obs_byname") {
        req(vals$specimen_row_targ)
        temp_pc <- pca_rx()$x[,as.numeric(input$warp_axes_selected)] 
        if(is.null(dim(temp_pc))) { # same as above
          which_row <- match(row.names(vals$specimen_row_targ), names(temp_pc)) 
          temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, tar = temp_pc[which_row])
          out <- temp_pc_pred$tar
          names(out) <- row.names(vals$specimen_row_targ)
        } else {
          which_row <- match(row.names(vals$specimen_row_targ), row.names(temp_pc))
          temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, tar = temp_pc[which_row,])
          out <- temp_pc_pred$tar
          names(out) <- row.names(vals$specimen_row_targ)
        }
      }
      if(input$warp_comparison_end == "selected_proj") {
        included_axes <- intersect(c(as.numeric(input$pca_x_axis), as.numeric(input$pca_y_axis)), as.numeric(input$warp_axes_selected))
        temp_pc <- pca_rx()$x[,included_axes] 
        temp_pc_pred <- shape.predictor(gpa_coords_rx(), x= temp_pc, Intercept = FALSE, 
                                        tar = vals$projection_row[na.omit(match(c(as.numeric(input$pca_x_axis), as.numeric(input$pca_y_axis)), included_axes))])
        out <- temp_pc_pred$tar
        
        names(out) <- paste("(", round(vals$projection_row[1], 4), ", ", round(vals$projection_row[2], 4), ")", sep = "")
      }
      return(out)
    }
  })
  
  tryObserveEvent(input$link_click_initiated, {
    req(input$link_click_initiated)
    req(gpa_coords_rx())
    link_mean_df <- as.data.frame(vals$mshape_gpacoords_rx) # these are the mean placements the landmarks that match up with what is displayed from the plotAllSpecimens plot
    colnames(link_mean_df) <- c("X", "Y")  #  
    new_link <- nearPoints(link_mean_df, input$link_click_initiated, 
                           xvar = "X", yvar = "Y",
                           threshold = 10, maxpoints = 1) # which mean landmark was closest to the click within 10 pixels
    vals$links <- c(vals$links, row.names(new_link)) # add on a new landmark to include in the link chain
    if(length(vals$links) > 1){ # this loop helps construct the df in the correct format for plotting
      for(i in 1:(length(vals$links)-1)){
        vals$links_df <- rbind(vals$links_df, c(vals$links[i], vals$links[i+1])) # all the rows except the last
      }}
  })
  
  tryObserveEvent(input$link_click_end, ignoreInit = T, { # this double click makes the links_df form in correct format for plotting
    req(vals$links)
    vals$links_dbclicked <- 1 # this is for conditional paneling of the reset button
    link_mean_df <- as.data.frame(vals$mshape_gpacoords_rx) # these are the mean placements the landmarks that match up with what is displayed from the plotAllSpecimens plot
    colnames(link_mean_df) <- c("X", "Y")
    last_link <- nearPoints(link_mean_df, input$link_click_end, 
                            xvar = "X", yvar = "Y",
                            threshold = 10, maxpoints = 1)
    if(length(vals$links) > 0){ # this loop helps construct the df in the correct format for plotting
      vals$links_df <- rbind(vals$links_df, c(vals$links[length(vals$links)], row.names(last_link))) # final row
    }
    vals$links <- NULL # resetting the vals$links so that if the user starts clicking again, the line doesn't connect to the last click before the double click
  })
  
  tryObserveEvent(input$link_reset, ignoreInit = T, {
    vals$links_df <- NULL
    vals$links <- NULL
    vals$links_dbclicked <- NULL
  })
  
  tryObserveEvent(input$semilms_reset, {
    vals$semis_brushed <- NULL
    vals$curves_brushed <- NULL
    
    vals$curves_final_anyNAs <- FALSE
    vals$curves_final <- NULL
    
    session$resetBrush("semis_selected")
    reset("semilms_upload_file")
    
    newmat1 <- matrix(NA, ncol = 3)
    colnames(newmat1) <- c("Before", "Slide", "After")
    updateMatrixInput(session, "semilms_manual_input", value = newmat1)   
    
  })
  
  tryObserveEvent(input$semis_selected, ignoreInit = T, { 
    req(gpa_coords_rx())
    link_mean_df <- as.data.frame(vals$mshape_gpacoords_rx) # these are the mean placements the landmarks that match up with what is displayed from the plotAllSpecimens plot
    colnames(link_mean_df) <- c("X", "Y")
    selected <- brushedPoints(link_mean_df, input$semis_selected, xvar = "X", yvar = "Y")
    if(length(unlist(selected))>1) { # if any landmarks were actually selected
      
      if(is.null(vals$semis_brushed)) {
        vals$semis_brushed <- as.character(row.names(selected)) # 
        
      } else {
        vals$semis_brushed <- unique(as.character(c(row.names(selected), vals$semis_brushed))) # 
      }
    }
  })
  
  brushes_links_listen <- reactive({list(vals$semis_brushed, vals$links_df)})
  tryObserveEvent(brushes_links_listen(), ignoreInit = T, {
    
    req(vals$semis_brushed)
    curves_rough <- data.frame("Before" = rep(NA, length(vals$semis_brushed)), 
                               "Slide" = unlist(vals$semis_brushed), 
                               "After" = rep(NA, length(vals$semis_brushed)))
    befores_last <- NULL
    afters_last <- NULL
    if(!is.null(vals$links_df)) {
      for (i in 1:length(vals$semis_brushed)) {
        if(any(vals$links_df[,2] %in% vals$semis_brushed[i])) {
          befores_i <- vals$links_df[which(vals$links_df[,2] == vals$semis_brushed[i]),1]
          befores_last <- c(befores_last, befores_i[length(befores_i)])
        } else {befores_last <- c(befores_last, NA)}
        if(any(vals$links_df[,1] %in% vals$semis_brushed[i])){
          afters_i <- vals$links_df[which(vals$links_df[,1] == vals$semis_brushed[i]),2]
          afters_last <- c(afters_last, afters_i[length(afters_i)])
        } else {afters_last <- c(afters_last, NA)}
      }
      
      curves_rough$Before <- befores_last
      curves_rough$After <- afters_last
    }
    
    temp_mat <- input$semilms_manual_input
    if(!identical(temp_mat, semilms_manual_matrix)) { # if there are already manual entries, add the curves to the bottom. labeled at beginning of script
      curves_rough <- rbind(temp_mat, curves_rough)
    }
    
    delete <- which(curves_rough[,2] == "")
    if(length(delete) > 0) {
      curves_rough <- curves_rough[-delete,]
    }
    
    keep <- NULL
    for(i in 1:nlevels(as.factor(curves_rough[,2]))){
      a <- levels(as.factor(curves_rough[,2]))[i]
      keep_new <- curves_rough[tail(which(curves_rough[,2] == a),1),]
      keep <- rbind(keep, keep_new)
    }
    vals$curves_final <- keep
    
    mat <- matrix(unlist(keep), ncol = 3)
    colnames(mat) <- c("Before", "Slide", "After")
    updateMatrixInput(session, "semilms_manual_input", value = mat) 
  })
  
  tryObserveEvent(input$semilms_manual_input, ignoreInit = T, {
    temp_mat <- input$semilms_manual_input
    for(i in 1:nrow(temp_mat)) { temp_mat[i,] <- as.numeric(temp_mat[i,])}
    
    if(!is.null(vals$curves_final)) { temp_vals_mat <- matrix(unlist(vals$curves_final), ncol = 3) } else { temp_vals_mat <- NULL }
    
    if(!identical(temp_mat, temp_vals_mat)) { # if there are already manual entries, add the curves to the bottom. labeled at beginning of script
      curves_rough <- rbind(temp_vals_mat, temp_mat)
      delete <- which(curves_rough[,2] == "")
      if(length(delete) > 0) {
        curves_rough <- curves_rough[-delete,]
      }
      
      keep <- NULL
      if(length(curves_rough) == 3) { curves_rough <- matrix(curves_rough, ncol = 3)}
      for(i in 1:nlevels(as.factor(curves_rough[,2]))){
        a <- levels(as.factor(curves_rough[,2]))[i]
        keep_new <- curves_rough[tail(which(curves_rough[,2] == a),1),]
        keep <- rbind(keep, keep_new)
      }
      vals$curves_final <- keep
      
      mat <- matrix(unlist(keep), ncol = 3)
      colnames(mat) <- c("Before", "Slide", "After")
      updateMatrixInput(session, "semilms_manual_input", value = mat) 
      
    }
  })
  
  
  tryObserveEvent(input$semilms_upload_file_input, ignoreInit=T, {
    inFile <- input$semilms_upload_file_input
    if (endsWith(inFile$name, '.xlsx') | endsWith(inFile$name, '.xls')| 
        endsWith(inFile$name, '.XLSX') | endsWith(inFile$name, '.XLS')){
      trait_raw_temp <- readxl::read_excel(inFile$datapath)
      trait_raw_temp <- as.data.frame(trait_raw_temp)
    } else if (endsWith(inFile$name, '.csv')| endsWith(inFile$name, '.CSV')){
      trait_raw_temp <- read.csv(inFile$datapath) # read in trait file (this cant be from the trait_rx or else the reactives become circular)
    } 
    
    if(ncol(trait_raw_temp) != 3) {
      vals$alert_semilmmat_wrong_format <- rnorm(1) # reinitiate the warning message every time
    } else {
      
      vals$curves_final <- trait_raw_temp
      
      trait_raw_temp <- matrix(unlist(trait_raw_temp), ncol = 3)
      colnames(trait_raw_temp) <- c("Before", "Slide", "After")
      updateMatrixInput(session, "semilms_manual_input", value = trait_raw_temp) 
      
    }
  })
  
  tryObserveEvent(vals$curves_final, ignoreInit = T, {
    # series of checks to make sure that vals$curves_final is valid
    req(vals$lms_rx)
    lms_out_of_bounds_tf <- any(!(unlist((as.numeric(unlist(vals$curves_final))) %in% 1:dim(vals$lms_rx)[1])))
    duplicate_brackets_tf <- any(vals$curves_final[,1] == vals$curves_final[,3])
    duplicate_semis_to_before_tf <- any(vals$curves_final[,1] == vals$curves_final[,2])
    duplicate_semis_to_after_tf <- any(vals$curves_final[,3] == vals$curves_final[,2])
    repeated_semis_tf <- (length(vals$curves_final[,2]) != length(unique(vals$curves_final[,2])))
    anynas_tf <-  anyNA(vals$curves_final)
    
    vals$curves_final_anyNAs <- any(c(lms_out_of_bounds_tf, duplicate_brackets_tf, 
                                      duplicate_semis_to_before_tf, duplicate_semis_to_after_tf,
                                      repeated_semis_tf,
                                      anynas_tf))
    
  })
  
  tryObserveEvent(update_vals$semilms_manual_input_selected, {
    if(!is.null(update_vals$semilms_manual_input_selected)) {
      updateMatrixInput(session, "semilms_manual_input", value = update_vals$semilms_manual_input_selected)
      update_vals$semilms_manual_input_selected <- NULL # resetting this so that next edits after returning to a bookmark don't revert to this
    }
  })
  
  outlier_selected_listen <- reactive({list(input$outlier_selected, update_vals$outlier_selected_selected)})
  tryObserveEvent(outlier_selected_listen(), ignoreInit = F, { 
    if(!is.null(input$outlier_selected) | !is.null(update_vals$outlier_selected_selected)) {
      grouping <- NULL
      req(gpa_coords_rx())
      
      column <- (2:4)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% input$outlier_group]
      if(length(column) > 0 & input$outlier_group_tf) { 
        grouping <- as.factor(vals$trait_rx[,column])
        grouping <- grouping[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1])]
      } else {grouping <- NULL}
      if(input$outlier_group_tf) {
        which_group_new <- as.numeric(input$outlier_group_level_plotted)
      } else { which_group_new <- 1 }
      
      x1 <- plotOutliers.ekb1(gpa_coords_rx(), groups = grouping)
      x <- plotOutliers.ekb2(x1, gpa_coords_rx(), groups = grouping, which_group = which_group_new, pt_cex = as.numeric(input$outlier_plot_pt_cex),
                             txt_cex = as.numeric(input$outlier_plot_txt_cex), show_point_names = input$outlier_plot_show_point_names_tf,
                             produce_plot = F) 
      y <- x[order(x, decreasing = TRUE)]
      outlier_df <- data.frame("x" = order(y, decreasing = TRUE), "y" = y) # defining the right x and y values for the plotOutliers
      if(is.null(update_vals$outlier_selected_selected)){
        vals$outlier_row <- nearPoints(outlier_df, input$outlier_selected, xvar = "x", yvar = "y",
                                       threshold = 15, maxpoints = 1) 
      } else {
        vals$outlier_row <- update_vals$outlier_selected_selected
      }
      
      
      delay(300, session$sendCustomMessage(type = "scrollCallback_outlier", 1))
    }
    
  })
  
  tryObserveEvent(input$find_mean_spec, ignoreInit = T, {
    req(gpa_coords_rx())
    mean_spec <- findMeanSpec(gpa_coords_rx())
    grouping <- NULL
    column <- (2:4)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% input$outlier_group]
    if(length(column) > 0 & input$outlier_group_tf) { 
      grouping <- as.factor(vals$trait_rx[,column]) 
      selected_level <- levels(grouping)[as.numeric(input$outlier_group_level_plotted)]
      grouping <- as.factor(grouping[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1])])
      coords_subset <- gpa_coords_rx()[,,which(grouping %in% selected_level)]
      mean_spec <- findMeanSpec(coords_subset)
    } else {grouping <- NULL}
    if(input$outlier_group_tf) {
      which_group_new <- as.numeric(input$outlier_group_level_plotted)
    } else { which_group_new <- 1 }
    
    x1 <- plotOutliers.ekb1(gpa_coords_rx(), groups = grouping)
    x <- plotOutliers.ekb2(x1, gpa_coords_rx(), groups = grouping, which_group = which_group_new, pt_cex = as.numeric(input$outlier_plot_pt_cex),
                           txt_cex = as.numeric(input$outlier_plot_txt_cex), show_point_names = input$outlier_plot_show_point_names_tf) 
    y <- x[order(x, decreasing = TRUE)]
    outlier_df <- data.frame("x" = order(y, decreasing = TRUE), "y" = y) # defining the right x and y values for the plotOutliers
    
    
    vals$outlier_row <- outlier_df[match(names(mean_spec), row.names(outlier_df)),]
    
  })
  
  tryObserveEvent(input$outlier_group_tf, priority = 100, ignoreInit = T,{
    if(!input$outlier_group_tf) {
      vals$outlier_row <- NULL
      reset("outlier_group")
      reset("outlier_group_level_plotted")
    }
  })
  
  
  
  tryObserveEvent(input$trait_column, priority = 100, {
    reset("outlier_group_tf")
  })
  
  tryObserveEvent(input$outlier_group, ignoreInit = T, {
    vals$outlier_row <- NULL
  })
  
  tryObserveEvent(input$outlier_group_level_plotted, ignoreInit = T, {
    vals$outlier_row <- NULL
  })
  
  #### Updating Input Options #####
  
  tryObserveEvent(input$raw_lms_already_aligned, {
    if(input$raw_lms_already_aligned){
      hideTab(inputId = "tab_dataprep", target = "Generalized Procrustes Alignment")
      if(is.null(alert_vals$already_aligned_csize_done)) {
        shinyalert(
          "Pre-Aligned LMs",
          "You've indicated that the uploaded shape file contains pre-aligned landmarks. This means that the App can not calculate centroid size. <br><br>
        If you'd like to include centroid size in any subsequent analyses, you can do 
        so by including it in your trait file upload.",
          size = "s",
          html = T
        )
        alert_vals$already_aligned_csize_done <- 1
      }
    } else {
      showTab(inputId = "tab_dataprep", target = "Generalized Procrustes Alignment")
    }
  })
  
  tryObserve({ # only showing the Linear Models page if there is a trait or csize to analyze
    if(!is.null(vals$trait_rx) | !is.null(vals$csize)){
      showTab(inputId = "navbar", target = "Linear Models")
    }
  })
  
  tryObserve({ # this updates the field where you select which trait columns to use in plotting and analyses ???
    
    if(is.null(vals$go_example_1)) { # if example button is not pushed
      if(!is.null(input$file_trait)) {
        # only run if trait file is uploaded
        inFile <- input$file_trait
        if (endsWith(inFile$name, '.xlsx') | endsWith(inFile$name, '.xls')| 
            endsWith(inFile$name, '.XLSX') | endsWith(inFile$name, '.XLS')){
          trait_raw_temp <- readxl::read_excel(inFile$datapath)
          trait_raw_temp <- as.data.frame(trait_raw_temp)
        } else if (endsWith(inFile$name, '.csv')| endsWith(inFile$name, '.CSV')){
          trait_raw_temp <- read.csv(inFile$datapath) # read in trait file (this cant be from the trait_rx or else the reactives become circular)
        }
        vals$trait_column_max_value <- ncol(trait_raw_temp) # count the number of available columns in the dataset
        vals$col_names_temp <- colnames(trait_raw_temp)
        if (is.null(inFile)) { vals$trait_column_max_value <- 2 } # if there is no file, ??? does this need adjusting? not sure why this is here if req(input$file_trait) is above
        if (vals$trait_column_max_value > 15) {vals$trait_column_max_value <- 16} # cutoff the number of columns available for selection at 15. No solid reason why, could be adjusted, just dont want to go crazy with it
      } 
    } else {  # example button pushed
      vals$trait_column_max_value <- 4
      vals$col_names_temp <- c("Species", "Discrete Example Data A", "Discrete Example Data B", "Continuous Example Data") 
    }
    
    if(!is.null(vals$col_names_temp)) {
      
      if(is.null(isolate(update_vals$trait_column_selected))) {
        updateCheckboxGroupInput(session, "trait_column", label = NULL,
                                 choiceNames = vals$col_names_temp[2:vals$trait_column_max_value], 
                                 choiceValues = 2:vals$trait_column_max_value, # updating options for all columns available
                                 selected = 2, inline = F)
      } else {
        updateCheckboxGroupInput(session, "trait_column", label = NULL,
                                 choiceNames = vals$col_names_temp[2:vals$trait_column_max_value], 
                                 choiceValues = 2:vals$trait_column_max_value, # updating options for all columns available
                                 selected = isolate(update_vals$trait_column_selected), inline = F)
        # update_vals$trait_column_selected <- NULL # resetting the bookmarked vals
      }
    }
    
  })
  
  tryObserveEvent(input$semilms_manual, {
    vals$semilms_manual <- "initiated"
  })
  
  tryObserveEvent(input$go_remove_outlier_specimen_reset, ignoreInit = T, { 
    vals$outlier_removed_names <- NULL
    reset("go_remove_outlier_specimen")
    reset("remove_outlier_specimen") 
    vals$unprune <- rnorm(1)
    
  })
  
  tryObserveEvent(input$go_remove_outlier_specimen, ignoreInit = T, {
    reset("outlier_group_tf")
  })
  
  remove_specimens_listen <- reactive({list(input$go_remove_outlier_specimen, input$go_remove_outlier_specimen_reset)})
  
  tryObserveEvent(remove_specimens_listen(), ignoreInit = F, { 
    req(gpa_coords_rx())
    vals$outlier_row <- NULL
    if(input$remove_outlier_specimen %in% dimnames(gpa_coords_rx())[[3]]) {
      vals$outlier_removed_names <- unique(c(vals$outlier_removed_names, input$remove_outlier_specimen, update_vals$remove_outlier_specimen_unselected)) 
    }
  })
  
  tryObserve({
    if(!is.null(vals$phy_rx) | !is.null(vals$go_example_1)) { pgls_ols <- "pgls" } else { pgls_ols <- "ols" }
    if(is.null(isolate(update_vals$pgls_ols_selected))) {
      updateRadioButtons(session, "pgls_ols", label = "Analysis Type:", 
                         choices = c("PGLS" = "pgls", "OLS" = "ols"), selected = pgls_ols)
    } else {
      updateRadioButtons(session, "pgls_ols", label = "Analysis Type:", 
                         choices = c("PGLS" = "pgls", "OLS" = "ols"), selected = isolate(update_vals$pgls_ols_selected)) # the isolates for these update_vals makes it so that pressing the bookmark button does not trigger a reset of inputs
      #update_vals$pgls_ols_selected <- NULL
    }
    
  })
  
  tryObserve({
    req(vals$trait_rx)
    if(input$outlier_group_tf){
      column <- (2:4)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% input$outlier_group]
      grouping <- as.factor(vals$trait_rx[,column])
      if(length(grouping)>0){
        
        choice_names <- levels(grouping)
        choice_vals <- 1:nlevels(grouping)
        if(min(table(grouping))<2) { # if any level has only 1 specimen
          
          for(i in 1:nlevels(grouping)){ # go through each level
            
            if(length(which(grouping == levels(grouping)[i])) < 2) { # if that level has only 1 specimen
              choice_names[i] <- NA # replace values with nas
              choice_vals[i] <- NA
            }
          }
          choice_names <- na.omit(choice_names) # omit all the nas
          choice_vals <- na.omit(choice_vals)
        }
        
        if(is.null(isolate(update_vals$outlier_group_level_plotted_selected))){
          updateRadioButtons(session, "outlier_group_level_plotted", label = NULL, 
                             choiceNames = choice_names, choiceValues = choice_vals)
        } else {
          updateRadioButtons(session, "outlier_group_level_plotted", label = NULL, 
                             choiceNames = choice_names, choiceValues = choice_vals,
                             selected = isolate(update_vals$outlier_group_level_plotted_selected))
          
        }
        
      }
    }
  })
  
  independent_variables_listen <- reactive({list(vals$trait_rx, gpa_coords_rx(), vals$csize,
                                               input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment)})
  tryObserveEvent(independent_variables_listen() , ignoreInit = F, { # this updates the field where you select the color and shape of the tip labels, along with other input items that require the precise name and treatment of the trait data
    
    req(gpa_coords_rx())
    
    if(is.null(vals$trait_rx)) { all_selected_trait_names <- NULL } else {
      all_selected_trait_names <- colnames(vals$trait_rx)[-1] # what are the selected column names from trait file
    }
    if(!is.null(vals$csize)) { all_selected_trait_names <- c(all_selected_trait_names, "Centroid Size")}
    
    
    if(length(all_selected_trait_names) > 0) {
      
      if(is.null(isolate(vals$csize))) {
        choice_list <- c("by_trait_1", "by_trait_2", "by_trait_3")[1:length(all_selected_trait_names)]
      } else { if(length(colnames(isolate(vals$trait_rx))[-1]) > 0) {
        choice_list <- c(c("by_trait_1", "by_trait_2", "by_trait_3")[1:length(colnames(isolate(vals$trait_rx))[-1])], "csize")
      } else { choice_list <- "csize"}
      }
      replacement_chull_name <- NULL
      
      for(i in 1:length(all_selected_trait_names)){
        names(choice_list)[i] <- paste("By", all_selected_trait_names[i], sep = " ") # labeling the choices
        replacement_chull_name <- c(replacement_chull_name, paste("Convex Hulls Around", all_selected_trait_names[i], "Groups", sep = " ")) # adding names to the convex hull list of options
      }
      vals$trait_names <- choice_list
      
      independent_variable_named <- choice_list
      names(independent_variable_named) <- all_selected_trait_names
      
      if(is.null(isolate(update_vals$independent_variables_selected))) { 
        selected_item <- independent_variable_named[1]
      } else {
        selected_item <- isolate(update_vals$independent_variables_selected)
      }
      
      updateCheckboxGroupInput(session, "independent_variables" , 
                               "Independent Variables Tested:",
                               choices = independent_variable_named, # updating options for all columns available
                               selected = selected_item, inline = F) 
      
      if(is.null(isolate(update_vals$independent_variables_model_1_selected))) { 
        selected_item <- independent_variable_named[1]
      } else { selected_item <- isolate(update_vals$independent_variables_model_1_selected) }
      
      updateCheckboxGroupInput(session, "independent_variables_model_1" , 
                               "Independent Variables Tested in Model 1:",
                               choices = independent_variable_named, # updating options for all columns available
                               selected = selected_item, inline = F) 
      
      if(is.null(isolate(update_vals$independent_variables_model_2_selected))) { 
        selected_item <- independent_variable_named[1]
      } else {
        selected_item <- isolate(update_vals$independent_variables_model_2_selected)
      }
      updateCheckboxGroupInput(session, "independent_variables_model_2" , 
                               "Independent Variables Tested in Model 2:",
                               choices = independent_variable_named, # updating options for all columns available
                               selected = selected_item, inline = F) 
      
      if(is.null(isolate(update_vals$independent_variables_model_3_selected))) { 
        selected_item <- independent_variable_named[1]
      } else {
        selected_item <- isolate(update_vals$independent_variables_model_3_selected)
      }
      updateCheckboxGroupInput(session, "independent_variables_model_3" , 
                               "Independent Variables Tested in Model 3:",
                               choices = independent_variable_named, # updating options for all columns available
                               selected = selected_item, inline = F) 
      
      
      choice_list_color <- c("all_1_col", choice_list)
      names(choice_list_color) <- c("All One Color", names(choice_list))
      
      
      if(is.null(isolate(update_vals$tip_col_category_selected))) { 
        selected_item <- "all_1_col"
      } else { selected_item <- isolate(update_vals$tip_col_category_selected) }
      
      updateSelectInput(session, inputId = "tip_col_category", label = "Point Color", 
                        choices = choice_list_color, selected = selected_item) 
      
      if(!is.null(update_vals$trait_1_treatment_selected)) {
        updates_tf <- any(((isolate(update_vals$trait_1_treatment_selected)) == "disc" & length(colnames(vals$trait_rx)[-1]) > 0), 
                          ((isolate(update_vals$trait_2_treatment_selected)) == "disc" & length(colnames(vals$trait_rx)[-1]) > 1),
                          ((isolate(update_vals$trait_3_treatment_selected)) == "disc" & length(colnames(vals$trait_rx)[-1]) > 2) )
      } else { updates_tf <- F}
      
      if((input$trait_1_treatment == "disc" & length(colnames(vals$trait_rx)[-1]) > 0) | 
         (input$trait_2_treatment == "disc" & length(colnames(vals$trait_rx)[-1]) > 1) | 
         (input$trait_3_treatment == "disc" & length(colnames(vals$trait_rx)[-1]) > 2) | 
         is.null(vals$trait_rx) | updates_tf){ #making reduced choice lists of just discrete variables
        
        if(is.null(isolate(update_vals$trait_1_treatment_selected))){
          selected_disc_options <- (1:length(colnames(vals$trait_rx)[-1]))[c(input$trait_1_treatment == "disc", 
                                                                             input$trait_2_treatment == "disc", 
                                                                             input$trait_3_treatment == "disc")]
        } else {
          selected_disc_options <- (1:length(colnames(vals$trait_rx)[-1]))[c(isolate(update_vals$trait_1_treatment_selected) == "disc", 
                                                                             isolate(update_vals$trait_2_treatment_selected) == "disc", 
                                                                             isolate(update_vals$trait_3_treatment_selected) == "disc")]
        }
        
        if(is.null(vals$trait_rx)) { selected_disc_options <- NULL } 
        
        choice_list_disc <- choice_list[na.omit(selected_disc_options)]
        
        if(is.null(isolate(update_vals$outlier_group_selected))) { 
          selected_item <- choice_list_disc[1]
        } else { selected_item <- isolate(update_vals$outlier_group_selected) }
        
        updateRadioButtons(session, "outlier_group", label = NULL, choices = choice_list_disc, selected = selected_item)
        
        for(i in 1:length(selected_disc_options)){ # loop that excludes any discrete trait with only 2 specimens in any given level
          trait <- as.factor(isolate(vals$trait_rx)[,(1+selected_disc_options[i])])
          level_numbers <- summary(trait)
          if(any(level_numbers < 3)) {
            selected_disc_options[i] <- NA
          }
        }
        
        choice_list_disc_3plus <- choice_list[na.omit(selected_disc_options)]
        choice_list_disc_plus_none <- c("none", choice_list_disc_3plus)
        names(choice_list_disc_plus_none) <- c("None", names(choice_list_disc_3plus))
        
        if(is.null(isolate(update_vals$integration_group_by_selected))) { 
          selected_item <- choice_list_disc_plus_none[1]
        } else { selected_item <- isolate(update_vals$integration_group_by_selected) }
        
        updateRadioButtons(session, "integration_group_by", label = NULL, choices = choice_list_disc_plus_none, selected = selected_item )
        
        
        choice_list_disc_allcol <- c("all_one_color", choice_list_disc)
        names(choice_list_disc_allcol) <- c("All One Color", names(choice_list_disc))
        if(is.null(isolate(update_vals$allometry_color_selected))) { 
          selected_item <- choice_list_disc_allcol[1]
        } else { selected_item <- isolate(update_vals$allometry_color_selected) }
        
        updateSelectInput(session, "allometry_color", label = "Color Points:", 
                          choices = choice_list_disc_allcol, selected = selected_item)
        
        
        
        if(is.null(isolate(update_vals$trajectory_group_selected))) { 
          selected_item <- choice_list_disc[1]
        } else { selected_item <- isolate(update_vals$trajectory_group_selected) }
        
        updateRadioButtons(session, "trajectory_group", "Group:", choices = choice_list_disc, selected = selected_item)
        
        
        
        if(length(choice_list_disc) > 1){
          if(is.null(isolate(update_vals$trajectory_trait_selected))) { 
            selected_item <- choice_list_disc[2]
          } else { selected_item <- isolate(update_vals$trajectory_trait_selected) }
          updateRadioButtons(session, "trajectory_trait", "Trait:", choices = choice_list_disc, selected = selected_item)
        }
        
        choice_list_shape <- c(19, 1, 18, choice_list_disc)
        names(choice_list_shape) <- c("Filled Circle", "Hollow Circle", "Filled Diamond", names(choice_list_disc))
        
        
        
        if(is.null(isolate(update_vals$tip_pch_selected))) { 
          selected_item <- choice_list_shape[1]
        } else { selected_item <- isolate(update_vals$tip_pch_selected) }
        
        updateSelectInput(session, inputId = "tip_pch", label = "Point Shape", 
                          choices = choice_list_shape, selected = selected_item)
        
        if(is.null(isolate(update_vals$show_convex_hull_1_selected))) {
          selected_item <- F
        } else { selected_item <- isolate(update_vals$show_convex_hull_1_selected) }
        
        updateCheckboxInput(session, inputId = "show_convex_hull_1", label = replacement_chull_name[c(input$trait_1_treatment == "disc", 
                                                                                                      input$trait_2_treatment == "disc", 
                                                                                                      input$trait_3_treatment == "disc")][1],
                            value = selected_item) 
        
        two_hulls_dependencies <- (length(which(c(input$trait_1_treatment == "disc", 
                                                  input$trait_2_treatment == "disc", 
                                                  input$trait_3_treatment == "disc"))) > 1)
        if(two_hulls_dependencies) { 
          if(is.null(isolate(update_vals$show_convex_hull_2_selected))) {
            selected_item <- F
          } else { selected_item <- isolate(update_vals$show_convex_hull_2_selected) }
          
          updateCheckboxInput(session, inputId = "show_convex_hull_2", label = replacement_chull_name[c(input$trait_1_treatment == "disc", 
                                                                                                        input$trait_2_treatment == "disc", 
                                                                                                        input$trait_3_treatment == "disc")][2],
                              value = selected_item)  # display a second convex hull option
          if(input$trait_1_treatment == "disc" & 
             input$trait_2_treatment == "disc" & 
             input$trait_3_treatment == "disc") { # display an option for a third hull
            if(is.null(isolate(update_vals$show_convex_hull_3_selected))) {
              selected_item <- F
            } else { selected_item <- isolate(update_vals$show_convex_hull_3_selected) }
            
            updateCheckboxInput(session, inputId = "show_convex_hull_3", label = replacement_chull_name[3], value = selected_item) 
          }
          
        }
      }  
      
      delay(500, { # this delay is necessary because input$trait_1_treatments (plus 2 and 3) are default "cont", so we need to slow this reaction down 1 sec so the other updates can run first
        if((input$trait_1_treatment == "cont" & length(colnames(vals$trait_rx)[-1]) > 0) | 
           (input$trait_2_treatment == "cont" & length(colnames(vals$trait_rx)[-1]) > 1) | 
           (input$trait_3_treatment == "cont" & length(colnames(vals$trait_rx)[-1]) > 2) |
           !is.null(isolate(vals$csize)) | isolate(input$raw_lms_already_aligned) == T) { # making reduced choice lists of just continuous variables
          
          selected_cont_options <- (1:length(colnames(vals$trait_rx)[-1]))[c(input$trait_1_treatment == "cont", 
                                                                             input$trait_2_treatment == "cont", 
                                                                             input$trait_3_treatment == "cont")]
          
          
          choice_list_cont <- choice_list[na.omit(selected_cont_options)]
          names(choice_list_cont) <- all_selected_trait_names[na.omit(selected_cont_options)]
          
          if(!is.null(isolate(vals$csize))) {
            if(!("csize" %in% choice_list_cont)){
            choice_list_cont <- c(choice_list_cont, "csize")
            names(choice_list_cont)[length(choice_list_cont)] <- "Centroid Size"}
          }
          
          
          if(length(choice_list_cont) > 0) { # if any selected variables are continuous
            choice_list_phy_sig <- c("shape", choice_list_cont)
            names(choice_list_phy_sig) <- c("Shape", names(choice_list_cont))
            
            if(is.null(isolate(update_vals$phy_signal_input_selected))) {
              selected_item <- choice_list_phy_sig[1]
            } else { selected_item <- isolate(update_vals$phy_signal_input_selected) }
            
            updateRadioButtons(session, inputId = "phy_signal_input", label = "Test Signal of:",
                               choices = choice_list_phy_sig, selected = selected_item)
          } else {
            updateRadioButtons(session, "phy_signal_input", "Test Signal of:", choices = c("Shape" = "shape"))
          }
        }
      })
    } 
  })
  
  allometry_predictor_listen <- reactive({list(vals$trait_rx, input$go_run_anova)})
  tryObserveEvent(allometry_predictor_listen(), ignoreInit = F, {
    if(input$go_run_anova > 0) {
      req(vals$trait_rx)
      
      if(is.null(vals$trait_rx)) { all_selected_trait_names <- NULL } else {
        all_selected_trait_names <- colnames(vals$trait_rx)[-1] # what are the selected column names from trait file
      }
      
      if(is.null(isolate(vals$csize))) {
        choice_list <- c("by_trait_1", "by_trait_2", "by_trait_3")[1:length(all_selected_trait_names)]
      } else { if(length(colnames(isolate(vals$trait_rx))[-1]) > 0) {
        choice_list <- c(c("by_trait_1", "by_trait_2", "by_trait_3")[1:length(colnames(isolate(vals$trait_rx))[-1])], "csize")
      } else { choice_list <- "csize"}
      }
      
      selected_cont_options <- (1:length(colnames(vals$trait_rx)[-1]))[c(input$trait_1_treatment == "cont", 
                                                                         input$trait_2_treatment == "cont", 
                                                                         input$trait_3_treatment == "cont")]
      
      choice_list_cont <- choice_list[na.omit(selected_cont_options)]
      names(choice_list_cont) <- all_selected_trait_names[na.omit(selected_cont_options)]
      
      if(!is.null(isolate(vals$csize))){
        choice_list_cont <- c(choice_list_cont, "csize")
        names(choice_list_cont)[length(choice_list_cont)] <- "Centroid Size"
      } 
      
      #if(length(choice_list_cont) > 1) {
      if(is.null(isolate(update_vals$allometry_predictor_selected))) {
        selected_item <- choice_list_cont[1]
      } else { selected_item <- isolate(update_vals$allometry_predictor_selected) }
      #}
      updateRadioButtons(session, inputId = "allometry_predictor", label = "Predictor Variable:",
                         choices = choice_list_cont, selected = selected_item)
    }
  })
  
  tryObserveEvent(input$independent_variables, ignoreInit = F, {
    
    if(is.null(update_vals$independent_variables_selected)) {
      independent_variables <- input$independent_variables
    } else { independent_variables <- update_vals$independent_variables_selected}
    
    req(vals$trait_rx)
    col_names_temp <- names(vals$trait_names) # what are the selected column names from trait file
    for(i in 1:length(col_names_temp)) { col_names_temp[i] <- substring(col_names_temp[i], 4, nchar(col_names_temp[i])) } # cutting off "By " at the beginning
    these_traits <- (1:3)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables]
    
    traits_selected <- col_names_temp[these_traits]
    
    if("csize" %in% independent_variables){
      traits_selected <- c(traits_selected, "Centroid Size")
    }
    if(!is.null(update_vals$independent_variables_order_selected)) { 
      traits_selected <- update_vals$independent_variables_order_selected
    }
    
    if(length(traits_selected)>1) {
      vals$trigger_trait_order_hide <- F
      updateOrderInput(session, "independent_variables_order", "Trait Order:", items = traits_selected)
    } else { vals$trigger_trait_order_hide <- T }
    
    
  })
  
  tryObserveEvent(input$independent_variables_model_1, ignoreInit = F, {
    
    if(is.null(update_vals$independent_variables_model_1_selected)) {
      independent_variables <- input$independent_variables_model_1
    } else { independent_variables <- update_vals$independent_variables_model_1_selected}
    
    req(vals$trait_rx)
    if(input$navbar == "Linear Models") {
      col_names_temp <- names(vals$trait_names) # what are the selected column names from trait file
      for(i in 1:length(col_names_temp)) { col_names_temp[i] <- substring(col_names_temp[i], 4, nchar(col_names_temp[i])) } # cutting off "By " at the beginning
      these_traits <- (1:3)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables]
      
      traits_selected <- col_names_temp[these_traits]
      
      if("csize" %in% independent_variables){
        traits_selected <- list(traits_selected, "Centroid Size")
      }
      if(!is.null(update_vals$independent_variables_order_model_1_selected)) { 
        traits_selected <- update_vals$independent_variables_order_model_1_selected
      }
      
      vals$model_comparison_model_1 <- c(vals$model_comparison_model_1,"go")
      updateOrderInput(session, "independent_variables_order_model_1", "Model 1 Trait Order:", items = traits_selected)
    }
    
  })
  
  tryObserveEvent(input$independent_variables_model_2, ignoreInit = F, {
    
    if(is.null(update_vals$independent_variables_model_2_selected)) {
      independent_variables <- input$independent_variables_model_2
    } else { independent_variables <- update_vals$independent_variables_model_2_selected}
    
    req(vals$trait_rx)
    if(input$navbar == "Linear Models") {
      col_names_temp <- names(vals$trait_names) # what are the selected column names from trait file
      for(i in 1:length(col_names_temp)) { col_names_temp[i] <- substring(col_names_temp[i], 4, nchar(col_names_temp[i])) } # cutting off "By " at the beginning
      these_traits <- (1:3)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables]
      
      traits_selected <- col_names_temp[these_traits]
      
      if("csize" %in% independent_variables){
        traits_selected <- list(traits_selected, "Centroid Size")
      }
      if(!is.null(update_vals$independent_variables_order_model_2_selected)) { 
        traits_selected <- update_vals$independent_variables_order_model_2_selected
      }
      
      vals$model_comparison_model_2 <- c(vals$model_comparison_model_2,"go")
      updateOrderInput(session, "independent_variables_order_model_2", "Model 2 Trait Order:", items = traits_selected)
    }
    
  })
  
  tryObserveEvent(input$independent_variables_model_3, ignoreInit = T, {
    
    if(is.null(update_vals$independent_variables_model_3_selected)) {
      independent_variables <- input$independent_variables_model_3
    } else { independent_variables <- update_vals$independent_variables_model_3_selected}
    
    req(vals$trait_rx)
    if(input$navbar == "Linear Models") {
      col_names_temp <- names(vals$trait_names) # what are the selected column names from trait file
      for(i in 1:length(col_names_temp)) { col_names_temp[i] <- substring(col_names_temp[i], 4, nchar(col_names_temp[i])) } # cutting off "By " at the beginning
      these_traits <- (1:3)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables]
      
      traits_selected <- col_names_temp[these_traits]
      
      if("csize" %in% independent_variables){
        traits_selected <- list(traits_selected, "Centroid Size")
      }
      if(!is.null(update_vals$independent_variables_order_model_3_selected)) { 
        traits_selected <- update_vals$independent_variables_order_model_3_selected
      }
      
      vals$model_comparison_model_3 <- c(vals$model_comparison_model_3,"go")
      updateOrderInput(session, "independent_variables_order_model_3", "Model 3 Trait Order:", items = traits_selected)
    }
    
  })
  
  
  
  tryObserveEvent(input$integration_group_by, ignoreInit = F, {
    req(vals$trait_rx)
    if(any(c("by_trait_1", "by_trait_2", "by_trait_3") %in% input$integration_group_by)) {
      col_names_temp <- vals$trait_names # what are the selected column names from trait file
      these_traits <- (2:4)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% input$integration_group_by]
      trait_level_names <- levels(as.factor(vals$trait_rx[,these_traits]))
      trait_levels <- 1:length(trait_level_names)
      names(trait_levels) <- trait_level_names
      
      if(!is.null(update_vals$integration_group_level_selected)){
        selected <- update_vals$integration_group_level_selected
      } else { selected <- trait_levels[1] }
      
      updateRadioButtons(session, "integration_group_level", "Trait Level Plotted:",
                         choices = trait_levels, selected = selected)
    }
  })
  
  tryObserve(priority = 10, {
    req(gpa_coords_rx())
    
    if(!is.null(isolate(update_vals$modularity_group_1_selected))) {
      
      modularity_groups <- rep(1, dim(gpa_coords_rx())[[1]])
      modularity_groups[as.numeric(isolate(update_vals$modularity_group_1_selected))] <- 1
      modularity_groups[as.numeric(isolate(update_vals$modularity_group_2_selected))] <- 2
      modularity_groups[as.numeric(isolate(update_vals$modularity_group_3_selected))] <- 3
      modularity_groups[as.numeric(isolate(update_vals$modularity_group_4_selected))] <- 4
      modularity_groups[as.numeric(isolate(update_vals$modularity_group_5_selected))] <- 5
      modularity_groups[as.numeric(isolate(update_vals$modularity_group_6_selected))] <- 6
      modularity_groups[as.numeric(isolate(update_vals$modularity_group_7_selected))] <- 7
      modularity_groups[as.numeric(isolate(update_vals$modularity_group_8_selected))] <- 8
      
      
      vals$modularity_groups <- modularity_groups
      
      suppressWarnings(updateOrderInput(session, "modularity_group_1", "", items = which(modularity_groups == 1), 
                                        connect = c('modularity_group_2', 'modularity_group_3', 'modularity_group_4', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_2", "", items = which(modularity_groups == 2),
                                        connect = c('modularity_group_1','modularity_group_3',  'modularity_group_4', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_3", "", items = which(modularity_groups == 3),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_4', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_4", "", items = which(modularity_groups == 4),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_5", "", items = which(modularity_groups == 5),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_6", "", items = which(modularity_groups == 6),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_5', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_7", "", items = which(modularity_groups == 7),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_5', 'modularity_group_6', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_8", "", items = which(modularity_groups == 8),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_5', 'modularity_group_6', 'modularity_group_7')))
      
    } else { 
      modularity_groups <- rep(2, dim(gpa_coords_rx())[1])
      
      full_length <- length(modularity_groups)
      modularity_groups[1:ceiling(full_length/2)] <- 1
      modularity_groups[(ceiling(full_length/2)+1):full_length] <- 2
      names(modularity_groups) <- 1:(dim(gpa_coords_rx())[1])
      vals$modularity_groups <- modularity_groups
      
      suppressWarnings(updateOrderInput(session, "modularity_group_1", "", items = names(modularity_groups)[which(modularity_groups == 1)], 
                                        connect = c('modularity_group_2', 'modularity_group_3', 'modularity_group_4', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_2", "", items = names(modularity_groups)[which(modularity_groups == 2)],
                                        connect = c('modularity_group_1','modularity_group_3',  'modularity_group_4', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_3", "", items = NULL,
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_4', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_4", "", items = NULL,
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_5", "", items = NULL,
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_6", "", items = NULL,
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_5', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_7", "", items = NULL,
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_5', 'modularity_group_6', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_8", "", items = NULL,
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_5', 'modularity_group_6', 'modularity_group_7')))
      
      reset("modularity_n_groups")
    }
  })
  
  tryObserveEvent(input$apply_modular_groups_go, {  
    #modularity_groups <- as.character(vals$modularity_groups) # 
    modularity_groups[as.numeric(input$modularity_group_1)] <- 1 
    modularity_groups[as.numeric(input$modularity_group_2)] <- 2 
    modularity_groups[as.numeric(input$modularity_group_3)] <- 3 
    modularity_groups[as.numeric(input$modularity_group_4)] <- 4 
    modularity_groups[as.numeric(input$modularity_group_5)] <- 5 
    modularity_groups[as.numeric(input$modularity_group_6)] <- 6 
    modularity_groups[as.numeric(input$modularity_group_7)] <- 7
    modularity_groups[as.numeric(input$modularity_group_8)] <- 8
    vals$modularity_groups <- modularity_groups
  })
  
  tryObserveEvent(input$modularity_n_groups, ignoreInit = T, {
    modularity_groups <- as.character(vals$modularity_groups) # 
    
    if(nlevels(as.factor(modularity_groups)) > input$modularity_n_groups) {
      selected_items_1 <- which(modularity_groups == 1)
      selected_items_2 <- which(modularity_groups == 2)
      selected_items_3 <- which(modularity_groups == 3)
      selected_items_4 <- which(modularity_groups == 4)
      selected_items_5 <- which(modularity_groups == 5)
      selected_items_6 <- which(modularity_groups == 6)
      selected_items_7 <- which(modularity_groups == 7)
      selected_items_8 <- which(modularity_groups == 8)
      
      
      if(as.numeric(input$modularity_n_groups) < 8) {
        selected_items_1 <- unique(c(selected_items_1, selected_items_8))
        modularity_groups[selected_items_8] <- 1
        if(as.numeric(input$modularity_n_groups) < 7) {
          selected_items_1 <- unique(c(selected_items_1, selected_items_7))
          modularity_groups[selected_items_7] <- 1
          if(as.numeric(input$modularity_n_groups) < 6) {
            selected_items_1 <- unique(c(selected_items_1, selected_items_6))
            modularity_groups[selected_items_6] <- 1
            if(as.numeric(input$modularity_n_groups) < 5) {
              selected_items_1 <- unique(c(selected_items_1, selected_items_5))
              modularity_groups[selected_items_5] <- 1
              if(as.numeric(input$modularity_n_groups) < 4) {
                selected_items_1 <- unique(c(selected_items_1, selected_items_4))
                modularity_groups[selected_items_4] <- 1
                if(input$modularity_n_groups < 3) {
                  selected_items_1 <- unique(c(selected_items_1, selected_items_3))
                  modularity_groups[selected_items_3] <- 1
                }
              }
            }
          }
        }
      }
      
      suppressWarnings(updateOrderInput(session, "modularity_group_1", "", items = which(modularity_groups == 1), 
                                        connect = c('modularity_group_2', 'modularity_group_3', 'modularity_group_4', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_2", "", items = which(modularity_groups == 2),
                                        connect = c('modularity_group_1','modularity_group_3',  'modularity_group_4', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_3", "", items = which(modularity_groups == 3),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_4', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_4", "", items = which(modularity_groups == 4),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_5', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_5", "", items = which(modularity_groups == 5),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_6', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_6", "", items = which(modularity_groups == 6),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_5', 'modularity_group_7', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_7", "", items = which(modularity_groups == 7),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_5', 'modularity_group_6', 'modularity_group_8')))
      suppressWarnings(updateOrderInput(session, "modularity_group_8", "", items = which(modularity_groups == 8),
                                        connect = c('modularity_group_1','modularity_group_2',  'modularity_group_3', 
                                                    'modularity_group_4', 'modularity_group_5', 'modularity_group_6', 'modularity_group_7')))
      
      
    }
    vals$modularity_groups <- modularity_groups
    
  })
  
  trigger_model_disc_listen <- reactive({list(input$independent_variables, input$independent_variables_order,
                                              input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment,
                                              bookmark_vals$adjust_updates)})
  
  tryObserveEvent(trigger_model_disc_listen(), ignoreInit = F, { 
    
    req(input$independent_variables)
    treatments <- c(input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment)
    disc_traits <- which(treatments == "disc")
    
    col_names_temp <- colnames(isolate(vals$trait_rx))[-1] # what are the selected column names from trait file
    
    independent_variables <- c(input$independent_variables)
    if(independent_variables[1] != "init"){
      these_traits <- (1:3)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables]
      
      trait_choices <- NULL
      trait_choices <- c("no_run", intersect(these_traits, disc_traits))
      names(trait_choices) <- c("Not Run", col_names_temp[intersect(these_traits, disc_traits)])
      
      if(is.null(isolate(update_vals$trait_pairwise_selected))){
        selected_item <- trait_choices[1]
      } else { selected_item <- isolate(update_vals$trait_pairwise_selected) }
      
      updateRadioButtons(session, inputId = "trait_pairwise", 
                         choices = trait_choices, 
                         selected = selected_item) 
      
      if(is.null(isolate(update_vals$disparity_groups_selected))){
        selected_item <- trait_choices[1]
      } else { selected_item <- isolate(update_vals$disparity_groups_selected) }
      
      updateRadioButtons(session, "disparity_groups",
                         "Morphological Disparity Groups Tested:",
                         choices = trait_choices, selected = selected_item)
      
      if(is.null(isolate(update_vals$evol_rate_groups_selected))){
        selected_item <- trait_choices[1]
      } else { selected_item <- isolate(update_vals$evol_rate_groups_selected) }
      
      updateRadioButtons(session, "evol_rate_groups",
                         "Evolutionary Rate Groups Tested:",
                         choices = trait_choices, selected = selected_item)
      
      
    }
  })
  
  tryObserveEvent(trigger_model_disc_listen(), ignoreInit = F, priority = -100, {
    
    
    
  })
  
  tryObserveEvent(gpa_coords_rx(), ignoreInit = T, priority = 10000, {
    req(gpa_coords_rx())
    specimen_options <- dimnames(gpa_coords_rx())[[3]]
    
    updateSelectizeInput(session, "remove_outlier_specimen", 
                         choices = specimen_options, options = list(
                           placeholder = 'Select or enter specimen',
                           onInitialize = I('function() { this.setValue(""); }'))) 
    
    updateSelectizeInput(session, "warp_specific_ref_specimen", label =  "Select Reference Specimen by Name:", 
                         choices = specimen_options, options = list(
                           placeholder = 'Select or enter specimen',
                           onInitialize = I('function() { this.setValue(""); }')))
    updateSelectizeInput(session, "warp_specific_targ_specimen", label =  "Select Target Specimen by Name:", 
                         choices = specimen_options, options = list(
                           placeholder = 'Select or enter specimen',
                           onInitialize = I('function() { this.setValue(""); }')))
    
  })
  
  
  tryObserveEvent(input$warp_var_displayed == "full_morphospace", ignoreInit = T,
                  {if(input$warp_var_displayed == "full_morphospace") {
                    reset("warp_axes_selected")}
                  })
  
  tryObserveEvent(input$more_warp_options, {
    showTab("display_more_warp_options", "more_warp_options")
  })
  
  tryObserveEvent(pca_nonphylo_rx(), ignoreInit = F, priority = -100, { # updating the options for which PC axes are available for visualizing on morphospace and visualizing variation in the warp grid
    
    if(is.null(vals$go_example_1)) {
      req(pca_nonphylo_rx())
      x <- dim(pca_nonphylo_rx()$x)[2]
      pc_axes_temp <- 1:x
      if (x > 9) { pc_axes_temp <- 1:10 } # limit the axes options to first 10 axes, shouldnt be necessary to do more
    } else { pc_axes_temp <- 1:8 } # example data have 9 pc axes
    
    if(is.null(update_vals$warp_axes_selected_selected)) {
      selected_item <- 1
    } else { selected_item <- update_vals$warp_axes_selected_selected } 
    
    updateCheckboxGroupInput(session, "warp_axes_selected", label = "Visualize Variation Across PC:",
                             choices = pc_axes_temp, selected = selected_item, inline = T)
    
    axes_options <- NULL # making sure the axes options is new with new datasets
    for(i in 1:length(pc_axes_temp)){
      axes_options <- c(axes_options, pc_axes_temp[i]) # adding and labeling all the axes options for input$pca_x_axis and input$pca_y_axis
      names(axes_options)[i] <- paste("PC", pc_axes_temp[i], sep = "") #  
    }
    
    if(is.null(update_vals$pca_x_axis_selected)) {
      selected_item <- 1
    } else { selected_item <- update_vals$pca_x_axis_selected }
    
    updateSelectInput(session, "pca_x_axis", label = "X axis", choices = axes_options, selected = selected_item)
    
    if(is.null(update_vals$pca_y_axis_selected)) {
      selected_item <- 2
    } else { selected_item <- update_vals$pca_y_axis_selected }
    
    updateSelectInput(session, "pca_y_axis", label = "Y axis", choices = axes_options, selected = selected_item)
    
  })
  
  tryObserve({ # updating the warp grid comparisons when the morphospace is doubleclicked, adding an option to compare the projected point
    if(!is.null(vals$morpho_dbclicked)) {
      
      if(is.null(isolate(update_vals$warp_comparison_start_selected))) {
        selected_item <- "mean"
      } else { selected_item <- isolate(update_vals$warp_comparison_start_selected) }
      
      updateRadioButtons(session, inputId="warp_comparison_start", label = "Warp Comparison Start (Reference):", 
                         choices = c("Mean Shape" = "mean",
                                     "Observed Point (Clicked)" = "selected_obs", 
                                     "Observed Point (By Name)" = "selected_obs_byname", 
                                     "Selected Projected Point (X)" = "selected_proj"),
                         selected = selected_item)
      
      if(is.null(isolate(update_vals$warp_comparison_end_selected))) {
        selected_item <- "selected_proj"
      } else { selected_item <- isolate(update_vals$warp_comparison_end_selected) }
      
      updateRadioButtons(session, inputId="warp_comparison_end", label = "Warp Comparison End (Target):", 
                         choices = c("Mean Shape" = "mean",
                                     "Observed Point (Clicked)" = "selected_obs", 
                                     "Observed Point (By Name)" = "selected_obs_byname", 
                                     "Selected Projected Point (X)" = "selected_proj"),
                         selected = selected_item)
    }
  })
  
  tryObserve(priority = 100, { # this updates the fields of trait treatment
    if(is.null(vals$go_example_1)){
      req(input$file_trait)
      inFile <- input$file_trait
      if (endsWith(inFile$name, '.xlsx') | endsWith(inFile$name, '.xls')| 
          endsWith(inFile$name, '.XLSX') | endsWith(inFile$name, '.XLS')){
        trait_rx_init_full <- readxl::read_excel(inFile$datapath)
        trait_rx_init_full <- as.data.frame(trait_rx_init_full)
      } else if (endsWith(inFile$name, '.csv')| endsWith(inFile$name, '.CSV')){
        trait_rx_init_full <- read.csv(inFile$datapath) # read in trait file (this cant be from the trait_rx or else the reactives become circular)
      }
      trait_rx_init <- trait_rx_init_full[,c(1,as.numeric(input$trait_column))]
      col_names_temp <- colnames(trait_rx_init)[-1] # what are the selected column names from trait file
      
      disc_or_cont <- NULL
      for(i in 1:(ncol(trait_rx_init)-1)){
        by_trait_i <- paste("by_trait", i, sep = "_") # making the right length options for plotting ???change for discrete/continuous specifics
        if(length(levels(as.factor(trait_rx_init[,(i+1)]))) < (length(trait_rx_init[,(i+1)]))/2) { # if we treat selected column i as a factor, and the number of levels is fewer than *half* the number of tips, we treat the data as discrete by default. i+1 is because first column is species names
          treatment <- "disc" } else { treatment <- "cont"} # otherwise, we treat column i as continuous by default 
        disc_or_cont <- c(disc_or_cont, treatment)
      } 
    } else {
      disc_or_cont <- c("space","disc", "disc", "cont", "cont", "cont")[c(as.numeric(input$trait_column), 5,6)] 
      disc_or_cont <- disc_or_cont[1:3] # cutoff extra disc_or_conts
    } 
    for (i in 1:length(disc_or_cont)) { # manually update all three labels if example data are used
      id_name <- paste("trait", i, "treatment", sep = "_")
      
      if(is.null(isolate(update_vals$trait_1_treatment_selected))) {
        selected_item <- disc_or_cont
      } else { selected_item <- c(isolate(update_vals$trait_1_treatment_selected), 
                                  isolate(update_vals$trait_2_treatment_selected), 
                                  isolate(update_vals$trait_3_treatment_selected))}
      
      updateRadioButtons(session, inputId = id_name, 
                         choices = c("Discrete" = "disc", "Continuous" = "cont"), 
                         selected = selected_item[i])
      
    }
  })
  
  #### Updating Statistical Outputs ####
  
  
  tryObserve({
    req(vals$phy_rx)
    req(input$phy_signal_input)
    if(datasets_dont_match() == FALSE) {
      if(input$phy_signal_input == "shape") {
        req(gpa_coords_rx())
        vals$PS_shape <- physignal(A=gpa_coords_rx(),phy=vals$phy_rx,iter=input$signal_perm)
      }
      if(input$phy_signal_input == "by_trait_1" | input$phy_signal_input == "by_trait_2" | input$phy_signal_input == "by_trait_3" | input$phy_signal_input == "csize") {
        which_trait <- (2:5)[c(input$phy_signal_input == "by_trait_1", input$phy_signal_input == "by_trait_2", input$phy_signal_input == "by_trait_3", input$phy_signal_input == "csize")]
        if(which_trait == 5) {
          trait_vector <- vals$csize
        } else {
          req(vals$trait_rx)
          trait_vector <- as.numeric(vals$trait_rx[,which_trait])
          names(trait_vector) <- vals$trait_rx[,1]
        }
        
        if(!anyNA(trait_vector)) {
          vals$PS_shape <- physignal(A=trait_vector,phy=vals$phy_rx,iter=input$signal_perm)
        } else {
          shinyalert(
            title = "Error in Trait Input",
            text = "The trait of interest contains some NA values, causing the function
            <span style='font-family: Courier New'>physignal</span> to fail. This could 
            be due to a discrete trait incorrectly being treated as continuous, among other possibilities.",
            type = "warning",
            html = T
          )
        }
      }
    }
  })
  
  anova_listen <- reactive({list(vals$go_run_anova, vals$go_run_model_comparison)})
  tryObserveEvent(anova_listen(), ignoreInit = F,  {
    vals$trait_1_treatment <- input$trait_1_treatment
    vals$trait_2_treatment <- input$trait_2_treatment
    vals$trait_3_treatment <- input$trait_3_treatment
  })
  
  go_run_anova_listen <- reactive({list(input$go_run_anova, update_vals$independent_variables_order_selected)})
  tryObserveEvent(go_run_anova_listen(), ignoreInit = F, priority = -10, { # suspend necessary because for some reason, (even when using priority) trait_1_treatment was not updated before this ran, causing a problem at the pairwise function
    
    #req(input$independent_variables) # keeps things from crashing if all are unselected
    if(is.null(update_vals$independent_variables_selected)) {
      independent_variables <- input$independent_variables     
    } else { independent_variables <- update_vals$independent_variables_selected }
    
    req(independent_variables)
    if(input$go_run_anova>0){
      if(datasets_dont_match() == FALSE) {
        if(independent_variables[1] != "init") { # required so that this doesn't try to run before input$independent_variables has been updated
          
          if(length(independent_variables) == 1) {
            if(independent_variables == "csize") { model_order <- "By Centroid Size" } else {
              model_order <- names(vals$trait_names)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables] #   
            }
            model_order <- substring(model_order, 4, nchar(model_order))
          } else { model_order <- input$independent_variables_order }
          
          
          if(model_order[1] != "Trait 1") {
            
            results <- model_definition(input.independent_variables = independent_variables, 
                                        vals.trait_rx = vals$trait_rx,
                                        vals.csize = vals$csize, 
                                        vals.trait_1_treatment = vals$trait_1_treatment, 
                                        vals.trait_2_treatment = vals$trait_2_treatment,
                                        vals.trait_3_treatment = vals$trait_3_treatment, 
                                        vals.trait_names = vals$trait_names, 
                                        vals.phy_rx = vals$phy_rx,
                                        gpa_coords_rx_reactive = gpa_coords_rx(), 
                                        input.independent_variables_order = model_order,
                                        input.pgls_ols = input$pgls_ols, 
                                        input.anova_perm = input$anova_perm, 
                                        input.ss_type = input$ss_type)#, input.interactions_included = input$interactions_included)
            
            vals$fit <- results 
            sum <- summary(results)
            if(length(independent_variables) > 1) {
              row.names(sum$table)[1:length(independent_variables)] <- input$independent_variables_order
            } else {
              if(is.null(vals$trait_rx)) { row.names(sum$table)[1] <- 'Centroid Size' } else {
                colnames_var <- colnames(vals$trait_rx)[-1] #  
                these_traits <- (1:3)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables]
                row.names(sum$table)[1] <- colnames_var[these_traits] 
              }
            }
            #   if(input$interactions_included){
            #     ninteractions <- 1
            #     interaction_names <- paste(input$independent_variables_order[1],input$independent_variables_order[2], sep = ":")
            #     if(length(independent_variables) > 2) {
            #       ninteractions <- 4
            #       interaction_names <- c(interaction_names, 
            #                              paste(input$independent_variables_order[1],input$independent_variables_order[3], sep = ":"),
            #                              paste(input$independent_variables_order[2],input$independent_variables_order[3], sep = ":"),
            #                              paste(input$independent_variables_order[1],input$independent_variables_order[2],
            #                                    input$independent_variables_order[3], sep = ":"))
            #     }
            #     
            #     row.names(sum$table)[(length(independent_variables)+1):(length(independent_variables)+ninteractions)] <- interaction_names
            #   }
            vals$anova_table <-  sum$table
            
            if(!is.null(vals$trait_rx)) {
              trait_3 <- trait_2 <- trait_1 <- vals$trait_rx[,2] # filling in trait_2 and 3 so that gdf does not require a bunch of dependencies
              csize <- NULL
              if(!is.null(vals$csize)) { csize <- vals$csize }
              
              if(vals$trait_1_treatment == "disc") {  trait_1 <- as.factor(vals$trait_rx[,2]) }
              if(vals$trait_1_treatment == "cont") {
                trait_1_c <- as.character(vals$trait_rx[,2]) # 
                trait_1 <- as.numeric(trait_1_c)
              }
              if("by_trait_2" %in% independent_variables) {     
                if(vals$trait_2_treatment == "disc") {trait_2 <- as.factor(vals$trait_rx[,3])}
                if(vals$trait_2_treatment == "cont") {
                  trait_2_c <- as.character(vals$trait_rx[,3]) # 
                  trait_2 <- as.numeric(trait_2_c)
                } 
              }
              if("by_trait_3" %in% independent_variables) {     
                if(vals$trait_3_treatment == "disc") {trait_3 <- as.factor(vals$trait_rx[,4])}
                if(vals$trait_3_treatment == "cont") {
                  trait_3_c <- as.character(vals$trait_rx[,4]) # 
                  trait_3 <- as.numeric(trait_3_c)
                } 
              }    
            }
            
            if(input$disparity_groups != "no_run" & !is.null(vals$trait_rx)) {
              
              group_selected <- list(trait_1, trait_2, trait_3)[[as.numeric(input$disparity_groups)]]
              morphol_sum <- morphol.disparity(results, groups = group_selected, print.progress = F, iter = input$anova_perm)
              vals$proc_vars <- morphol_sum$Procrustes.var
              names(vals$proc_vars) <- names(morphol_sum$Procrustes.var)
              comparisons <- combn(ncol(morphol_sum$PV.dist),2)
              group_names <- colnames(morphol_sum$PV.dist)
              data <- cbind(morphol_sum$PV.dist[lower.tri(morphol_sum$PV.dist)], 
                            morphol_sum$PV.dist.Pval[lower.tri(morphol_sum$PV.dist.Pval)])
              
              disp_tab <- matrix(data, nrow = ncol(comparisons), ncol = 2)
              row.names(disp_tab) <- LETTERS[1:nrow(disp_tab)] # place holder so that the loop below works
              for(i in 1:ncol(comparisons)){
                row.names(disp_tab)[i] <- paste(group_names[comparisons[1,i]], group_names[comparisons[2,i]], sep = ":")
              }
              colnames(disp_tab) <- c("d", "Pr > d")
              vals$morphol_disp_table <- disp_tab
              
            }
            
            if(input$evol_rate_groups != "no_run" & !is.null(vals$trait_rx)){
              group_selected <- list(trait_1, trait_2, trait_3)[[as.numeric(input$evol_rate_groups)]]
              names(group_selected) <- vals$phy_rx$tip.label 
              out <- compare.evol.rates(gpa_coords_rx(), phy = vals$phy_rx, gp = group_selected, print.progress = F, iter = input$anova_perm)
              out_mat <- c(out$sigma.d.gp, out$sigma.d.all, out$Z, out$P.value)
              names(out_mat)[(out$Ngroups+1):(out$Ngroups+3)] <- c("Overall Rate" , "Effect Size", "Overall P")
              vals$evol_rates <- out_mat
              
              comparisons <- combn(length(out$sigma.d.gp),2)
              rate_ratio_comparisons <- cbind(unlist(out$sigma.d.gp.ratio), unlist(out$pairwise.pvalue))
              row.names(rate_ratio_comparisons) <- LETTERS[1:nrow(rate_ratio_comparisons)] # place holder so that the loop below works
              for (i in 1:ncol(comparisons)) {
                row.names(rate_ratio_comparisons)[i] <- paste(names(out$sigma.d.gp)[comparisons[1,i]], names(out$sigma.d.gp)[comparisons[2,i]], sep = ":")
              }
              colnames(rate_ratio_comparisons) <- c("Rate Ratio", "P")
              vals$evol_rates_pairwise <- rate_ratio_comparisons
            }
            
            treatments <- FALSE # placeholder
            if(input$trait_pairwise != "no_run" & !is.null(vals$trait_rx)) { 
              columns <- ncol(vals$trait_rx) - 1
              treatments <- c(vals$trait_1_treatment == "disc", vals$trait_2_treatment == "disc",vals$trait_3_treatment == "disc")[1:columns] 
              pair_trait_disc <- treatments[as.numeric(input$trait_pairwise)] 
            } else {pair_trait_disc <- FALSE}
            if(any(treatments) & pair_trait_disc) { # stops this from running if only the continuous trait is selected
              pairwise <- pairwise(results, groups = list(trait_1, trait_2, trait_3)[[as.numeric(input$trait_pairwise)]], print.progress = F) 
              sum_pair <- summary(pairwise)
              vals$pairwise_table <- sum_pair$summary.table
            }
            if(!pair_trait_disc){
              vals$pairwise_table <- NULL
            }
          }
        }
      }
    }
  })
  
  model_comparison_listen <- reactive({list(input$independent_variables_model_1, input$go_run_model_comparison, input$independent_variables_order_model_1)})
  tryObserveEvent(model_comparison_listen(), ignoreInit = F, priority = -10, {
    if(input$go_run_model_comparison > 0) {
      req(input$independent_variables_model_1) # keeps things from crashing if all are unselected
      req(input$independent_variables_model_2) 
      req(vals$trait_rx)
      
      if(is.null(update_vals$independent_variables_model_1_selected)) {
        independent_variables_model_1 <- input$independent_variables_model_1  
        independent_variables_model_2 <- input$independent_variables_model_2 
        independent_variables_model_3 <- input$independent_variables_model_3
      } else { 
        independent_variables_model_1 <- update_vals$independent_variables_model_1_selected
        independent_variables_model_2 <- update_vals$independent_variables_model_2_selected
        independent_variables_model_3 <- update_vals$independent_variables_model_3_selected }
      
      if(input$independent_variables_order_model_1[1] != "Trait 1") {
        if(datasets_dont_match() == FALSE) {
          if(independent_variables_model_1[1] != "init" & independent_variables_model_2[1] != "init") { # required so that this doesn't try to run before input$independent_variables has been updated
            
            if(length(independent_variables_model_1) == 1) {
              if(independent_variables_model_1 == "csize") { model_order_1 <- "By Centroid Size" } else {
                model_order_1 <- names(vals$trait_names)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables_model_1] #   
              }
              model_order_1 <- substring(model_order_1, 4, nchar(model_order_1))
            } else { model_order_1 <- input$independent_variables_order_model_1 }
            
            if(length(independent_variables_model_2) == 1) {
              if(independent_variables_model_2 == "csize") { model_order_2 <- "By Centroid Size" } else {
                model_order_2 <- names(vals$trait_names)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables_model_2] #   
              }
              model_order_2 <- substring(model_order_2, 4, nchar(model_order_2))
            } else { model_order_2 <- input$independent_variables_order_model_2 }
            
            
            if(model_order_2[1] != "Trait 1") {
              
              model_1 <- model_definition(input.independent_variables = independent_variables_model_1, 
                                          vals.trait_rx = vals$trait_rx,
                                          vals.csize = vals$csize, 
                                          vals.trait_1_treatment = vals$trait_1_treatment, 
                                          vals.trait_2_treatment = vals$trait_2_treatment,
                                          vals.trait_3_treatment = vals$trait_3_treatment, 
                                          vals.trait_names = vals$trait_names, 
                                          vals.phy_rx = vals$phy_rx,
                                          gpa_coords_rx_reactive = gpa_coords_rx(), 
                                          input.independent_variables_order = model_order_1,
                                          input.pgls_ols = input$pgls_ols_model_comparison, input.anova_perm = input$anova_perm_model_comparison, 
                                          input.ss_type = input$ss_type_model_comparison)#, input.interactions_included = input$interactions_included_model_comparison)
              
              model_2 <- model_definition(input.independent_variables = independent_variables_model_2, 
                                          vals.trait_rx = vals$trait_rx, vals.csize = vals$csize, 
                                          vals.trait_1_treatment = vals$trait_1_treatment, vals.trait_2_treatment = vals$trait_2_treatment,
                                          vals.trait_3_treatment = vals$trait_3_treatment, vals.trait_names = vals$trait_names, 
                                          vals.phy_rx = vals$phy_rx,
                                          gpa_coords_rx_reactive = gpa_coords_rx(), 
                                          input.independent_variables_order = model_order_2,
                                          input.pgls_ols = input$pgls_ols_model_comparison, input.anova_perm = input$anova_perm_model_comparison, 
                                          input.ss_type = input$ss_type_model_comparison)#, input.interactions_included = input$interactions_included_model_comparison)
            }
            if(input$add_third_model){
              if(length(independent_variables_model_3) == 1) {
                if(independent_variables_model_3 == "csize") { model_order_3 <- "By Centroid Size" } else {
                  model_order_3 <- names(vals$trait_names)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables_model_3] #   
                }
                model_order_2 <- substring(model_order_2, 4, nchar(model_order_2))
              } else { model_order_3 <- input$independent_variables_order_model_3 }
              
              model_3 <- model_definition(input.independent_variables = independent_variables_model_3, 
                                          vals.trait_rx = vals$trait_rx, vals.csize = vals$csize, 
                                          vals.trait_1_treatment = vals$trait_1_treatment, vals.trait_2_treatment = vals$trait_2_treatment,
                                          vals.trait_3_treatment = vals$trait_3_treatment, vals.trait_names = vals$trait_names, 
                                          vals.phy_rx = vals$phy_rx,
                                          gpa_coords_rx_reactive = gpa_coords_rx(), input.independent_variables_order = model_order_3,
                                          input.pgls_ols = input$pgls_ols_model_comparison, input.anova_perm = input$anova_perm_model_comparison, 
                                          input.ss_type = input$ss_type_model_comparison)# , input.interactions_included = input$interactions_included_model_comparison)
            }
            
            if(!input$add_third_model){
              vals$model_comparison <- anova(model_1, model_2, print.progress = F)
            } else {
              vals$model_comparison <- anova(model_1, model_2, model_3, print.progress = F)
            }
            
          }
        }
      }
    }
  })
  
  #  tryObserveEvent(input$independent_variables, priority = 100, {
  #    reset("interactions_included")
  #  })
  
  #### Outputs #####
  
  
  # plots for data input visualization  
  output$shape_file <- output$raw_lms_rx <- renderPrint({
    req(vals$lms_rx)
    print(vals$lms_rx)
  })
  
  output$gpa_aligned_rx <- renderPrint(width = 100, {
    req(gpa_coords_rx())
    if(is.null(vals$go_run_gpa)){
      return("GPA not yet run.")
    } else { print(gpa_coords_rx())}
    
  })
  
  output$phylogeny <- renderPlot(bg = "transparent", height = 900, {
    req(vals$phy_rx)
    plot(vals$phy_rx, show.tip.label = input$show_tip_label_phy_preview)
  })
  
  output$trait_table <- renderTable({
    if(length(which(is.na(vals$trait_rx))) != length(vals$trait_rx)) {
      vals$trait_rx
    }
    
  })
  
  output$trait_1_name <- renderText({
    req(vals$trait_rx)
    if(ncol(vals$trait_rx) > 1){
      colnames(vals$trait_rx)[2]
    } else {return(NULL)}
  })
  
  output$trait_2_name <- renderText({
    req(vals$trait_rx)
    if(ncol(vals$trait_rx) > 2){
      colnames(vals$trait_rx)[3]
    } else {return(NULL)}
  })
  
  output$trait_3_name <- renderText({
    req(vals$trait_rx)
    if(ncol(vals$trait_rx) > 3){
      colnames(vals$trait_rx)[4]
    } else {return(NULL)}
  })
  
  # plots for data checking
  
  output$all_specimens <- renderPlot(bg = "transparent", { 
    req(gpa_coords_rx())
    lm_col <- rep(input$semilms_color_other, dim(gpa_coords_rx())[1])
    if(!is.null(vals$curves_final)) {
      if(nrow(vals$curves_final)>0) {
        curves_temp <- matrix(unlist(vals$curves_final), ncol = 3)
        if((!anyNA(curves_temp[,2])) & !("" %in% curves_temp[,2])){ # as long as that semis column isn't NAs
          lm_col[as.numeric(unlist(vals$curves_final[,c(1,3)]))] <- input$semilms_color_brackets 
          lm_col[as.numeric(unlist(vals$curves_final[,2]))] <- input$semilms_color
        }
      }
    }
    par(mar = c(2,6,2,2))
    plotAllSpecimens(gpa_coords_rx(),  label = T, links = vals$links_df, 
                     plot.param = list(txt.cex = 1.5, txt.col = input$semilms_color_labels,
                                       pt.bg = input$semilms_color_individlms, 
                                       link.col = input$semilms_color_links,
                                       mean.bg = lm_col))
  })
  
  output$outliers <- renderPlot(bg = "transparent", { 
    req(gpa_coords_rx())
    grouping <- NULL
    column <- (2:4)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% input$outlier_group]
    if(is.null(update_vals$outlier_group_tf_selected)) {
      outlier_group_tf <- input$outlier_group_tf
    } else { outlier_group_tf <- update_vals$outlier_group_tf_selected}
    
    if(length(column) > 0 & outlier_group_tf) { 
      grouping <- as.factor(vals$trait_rx[,column]) 
      grouping <- grouping[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1])]
    } else {grouping <- NULL}
    if(outlier_group_tf) {
      which_group_new <- as.numeric(input$outlier_group_level_plotted)
    } else { which_group_new <- 1 }
    x <- plotOutliers.ekb1(gpa_coords_rx(), groups = grouping)
    
    plotOutliers.ekb2(x, gpa_coords_rx(), groups = grouping, 
                      which_group = which_group_new, pt_cex = as.numeric(input$outlier_plot_pt_cex),
                      txt_cex = as.numeric(input$outlier_plot_txt_cex), 
                      show_point_names = input$outlier_plot_show_point_names_tf) 
    if(is.null(vals$outlier_row) == "FALSE") { points(x = vals$outlier_row[1], y = vals$outlier_row[2], col = "red", 
                                                      cex = as.numeric(input$outlier_plot_pt_cex) + 2, pch = 1) } # this adds a red circle around whichever specimen was selected and visualized
    
    
  })
  
  output$outlier_selected_lms <- renderPlot(bg = "transparent", {
    req(vals$outlier_row)
    grouping <- NULL
    column <- (2:4)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% input$outlier_group]
    if(length(column) > 0 & input$outlier_group_tf) { 
      grouping <- as.factor(vals$trait_rx[,column]) 
      grouping <- grouping[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1])]
    } else {grouping <- NULL}
    if(input$outlier_group_tf) {
      which_group_new <- as.numeric(input$outlier_group_level_plotted)
    } else { which_group_new <- 1 }
    
    x1 <- plotOutliers.ekb1(gpa_coords_rx(), groups = grouping)
    x <- plotOutliers.ekb2(x1, gpa_coords_rx(), groups = grouping, 
                           which_group = which_group_new, pt_cex = as.numeric(input$outlier_plot_pt_cex),
                           produce_plot = F, txt_cex = as.numeric(input$outlier_plot_txt_cex), 
                           show_point_names = input$outlier_plot_show_point_names_tf)
    y <- x[order(x, decreasing = TRUE)]
    outlier_df <- data.frame("x" = order(y, decreasing = TRUE), "y" = y)
    selected_outlier <- match(row.names(vals$outlier_row), names(x))
    lm_col <- rep(input$vis_outliers_color_other, dim(gpa_coords_rx())[1])
    if(!is.null(vals$curves_final)) {
      temp_mat <- matrix(vals$curves_final, ncol = 3, byrow = T)
      if(!anyNA(temp_mat[,2])){ 
        lm_col[as.numeric(unlist(vals$curves_final[,2]))] <- input$vis_outliers_color
      }
    }
    
    par(mar = c(0,2,4,0))
    plot(gpa_coords_rx()[,,selected_outlier], col = lm_col, pch = 19, cex = 2,
         main = paste("Selected Specimen: ", row.names(vals$outlier_row), sep = ""), 
         asp = 1, axes = F) # displays the selected specimen landmark placement
    text(x = gpa_coords_rx()[,,selected_outlier][,1]-0.01, y = gpa_coords_rx()[,,selected_outlier][,2] - 0.01, 
         labels = 1:(nrow(gpa_coords_rx()[,,selected_outlier])), col = input$vis_outliers_color_labels) # adds labels to the landmarks
    if(!is.null(vals$links_df)){
      for (i in 1:nrow(vals$links_df)) {
        seg_df <- gpa_coords_rx()[,,selected_outlier]
        xys <- seg_df[vals$links_df[i,],]
        segments(x0 = xys[1], y0 = xys[3],x1 = xys[2], y1 = xys[4], col = input$vis_outliers_color_links)
      }
    }
    
    
  })
  
  output$outlier_removed_names <- renderText(sep = ", ",{
    if(!is.null(isolate(vals$outlier_removed_names)) | !is.null(isolate(update_vals$remove_outlier_specimen_unselected))){
      vals$outlier_removed_names <- unique(c(isolate(vals$outlier_removed_names), 
                                             isolate(update_vals$remove_outlier_specimen_unselected)))
    }
  })
  
  output$outlier_removed_names_tab <- renderText({
    if(is.null(vals$outlier_removed_names)) {
      if(!is.null(isolate(update_vals$remove_outlier_specimen_unselected))) {
        mat <- matrix(isolate(update_vals$remove_outlier_specimen_unselected), ncol = 1)
        colnames(mat) <- " "
        mat
      } else {
        mat <- matrix("None")
        colnames(mat) <- " "
        mat
      }
    } else {
      mat <- matrix(vals$outlier_removed_names, ncol = 1)
      colnames(mat) <- " "
      mat
    }
  })
  
  output$missing_lms_to_estimate <- renderTable(digits = 0, {
    if(anyNA(vals$lms_rx)) {
      these <- which(is.na(vals$lms_rx) == TRUE) #  37  46 116 125 135 144
      these_x <- these[seq(1, length(these), 2)]
      spec_numb <- ceiling(these/(dim(vals$lms_rx)[1]*dim(vals$lms_rx)[2]))
      spec_numb <- spec_numb[seq(1, length(spec_numb), 2)] # deleting the duplicates without erasing specimen numbers when 2 lms are missing within a specimen
      
      this_many_lms <- length(spec_numb)
      mat <- matrix(NA, ncol = 2, nrow = this_many_lms)
      colnames(mat) <- c("Specimen", "Landmark")
      
      for(i in 1:this_many_lms){
        reduced_these <- these_x[i] - dim(vals$lms_rx)[1]*dim(vals$lms_rx)[2]*(spec_numb[i] - 1)
        mat[i,] <- c(spec_numb[i], reduced_these)
      }
      mat[,1] <- dimnames(vals$lms_rx)[[3]][mat[,1]]
      
      mat
    } else {
      mat <- matrix("None")
      colnames(mat) <- " "
      mat
    }
  })
  
  output$semilandmark_matrix <- renderTable(digits = 0, {
    if(!is.null(vals$curves_final)){
      if(nrow(vals$curves_final) == 0) {
        mat <- matrix("None")
        colnames(mat) <- " "
        mat
      } else {
        vals$curves_final
      }
    }  else {
      mat <- matrix("None")
      colnames(mat) <- " "
      mat
    }
  })
  
  # Morphospace plot
  tryObserve({
    req(pca_rx())
    if(is.null(vals$phy_rx) | !is.null(vals$go_example_1)) {
      match <- TRUE
    }
    
    if(!is.null(vals$phy_rx) & !is.null(gpa_coords_rx())) {
      match <- identical(vals$phy_rx$tip.label, dimnames(gpa_coords_rx())[[3]]) 
    } 
    
    if(match) {
      xx <- geomorph:::plot.gm.prcomp(
        pca_rx(), axis1 = as.numeric(input$pca_x_axis), axis2 = as.numeric(input$pca_y_axis), 
        phylo = input$include_phylo, phylo.par = list(anc.states = TRUE)) # 
      
      vals$morphospace <- xx
      vals$plot_args <- xx$plot.args
    }
  })
  
  output$morphospace <- renderPlot(bg = scales::alpha("white", 0), {
    req(vals$morphospace)
    pr <- pca_rx() 
    
    # the following section is modified from geomorph:::plot.gm.prcomp
    class(pr) <- "ordinate"
    if(!is.null(vals$tip_pch)) {input_pch <- as.numeric(vals$tip_pch)} else {input_pch <- 19}
    if(is.null(input$flip_x_axis)) { input.flip_axis <- NULL }
    if(round(input$flip_x_axis/2) != input$flip_x_axis/2) {
      input.flip_axis  <- 1 } else { input.flip_axis <- NULL}
    if(round(input$flip_y_axis/2) != input$flip_y_axis/2) {
      input.flip_axis  <- c(input.flip_axis, 2) } 
    input.flip_axis <- as.numeric(input.flip_axis)
    
    pr.plot <- RRPP:::plot.ordinate(pr, axis1 = as.numeric(input$pca_x_axis), axis2 = as.numeric(input$pca_y_axis), 
                                    pch = input_pch, cex = as.numeric(input$tip_cex),
                                    col = vals$tip_col, asp = 1,
                                    flip = input.flip_axis) # its unclear why the axis line types are red too
    
    abline(h = 0, lty = 2) # these are only here until Mike fixes the plot.ordinate function in RRPP
    abline(v = 0, lty = 2) # these are only here until Mike fixes the plot.ordinate function in RRPP
    
    if(input$show_tip_label) {
      text(pr.plot$points, rownames(pr.plot$points), adj = c(-(input$tip_cex)/15,-(input$tip_cex)/15),
           cex = as.numeric(input$tip_txt_cex), col = vals$tip_col)
    }# up to here
    
    # this part is first so that points appear over the top of the phylogeny branches
    if(input$include_phylo) {
      
      z <- vals$morphospace$phylo$phy.pcdata
      edges <- as.matrix(vals$phy_rx$edge)
      
      for (i in 1:NROW(edges)) {
        pts <- z[edges[i, ], ]
        points(pts, type = "l", col = input$edge_col, lwd = as.numeric(input$edge_width), lty = 1) 
      }
      ancs <- z[(length(vals$phy_rx$tip.label)+1):nrow(z),]
      points(ancs, pch = as.numeric(input$node_pch), cex = as.numeric(input$node_cex), col = input$node_col)
      
      if(input$show_node_label) {
        text(ancs, rownames(ancs), adj = c(-(input$node_cex)/15,-(input$node_cex)/15), 
             cex = as.numeric(input$node_txt_cex), col = input$node_txt_col) # all these can be changed if we want to make them options
      }
    }
    
    if(!is.null(vals$morpho_clicked)) {
      if(input$warp_comparison_start == "selected_obs") {
        points(x = vals$specimen_row_ref[,as.numeric(input$pca_x_axis)], y = vals$specimen_row_ref[,as.numeric(input$pca_y_axis)], col = "red", cex = as.numeric(input$tip_cex) + 1.2, pch = 1)
      }
      req(input$warp_comparison_end)
      if(input$warp_comparison_end == "selected_obs") {
        points(x = vals$specimen_row_targ[,as.numeric(input$pca_x_axis)], y = vals$specimen_row_targ[,as.numeric(input$pca_y_axis)], col = "red", cex = as.numeric(input$tip_cex) + 1, pch = 1)
      } 
    }
    
    if(is.null(vals$morpho_dbclicked) == "FALSE") { 
      points(x = vals$projection_row[1], y = vals$projection_row[2], col = "red", cex = as.numeric(input$tip_cex) + 1, pch = 4)
    }
    
    if(input$warp_comparison_show_arrow == TRUE) {
      if(!is.null(input$warp_comparison_end)) {
        if(input$warp_comparison_start == "mean") { start.xy <- c(0,0)}
        if(input$warp_comparison_start == "selected_proj") { start.xy <- vals$projection_row[1:2]}
        if(input$warp_comparison_start == "selected_obs" | input$warp_comparison_start == "selected_obs_byname") { 
          start.xy <- c(as.numeric(vals$specimen_row_ref[,as.numeric(input$pca_x_axis)]), as.numeric(vals$specimen_row_ref[,as.numeric(input$pca_y_axis)])) }
        if(input$warp_comparison_end == "mean") { end.xy <- c(0,0)}
        if(input$warp_comparison_end == "selected_proj") { end.xy <- vals$projection_row[1:2]}
        if(input$warp_comparison_end == "selected_obs"| input$warp_comparison_end == "selected_obs_byname") { 
          end.xy <- c(as.numeric(vals$specimen_row_targ[,as.numeric(input$pca_x_axis)]), as.numeric(vals$specimen_row_targ[,as.numeric(input$pca_y_axis)]))  }
        
        arrows(x0 = start.xy[1], y0 = start.xy[2], x1 = end.xy[1], y1 = end.xy[2], col = "red", lty = 1, lwd = 2)
      }
    }
    
    if(input$show_convex_hull_1 | 
       input$show_convex_hull_2 | 
       input$show_convex_hull_3) {
      selected_trait <- (2:4)[c(input$show_convex_hull_1, input$show_convex_hull_2, input$show_convex_hull_3)]
      colored_tips <- (2:4)[c(input$tip_col_category == "by_trait_1", input$tip_col_category == "by_trait_2", input$tip_col_category == "by_trait_3")]
      
      col_list_modified <- list(vals$color_options_list$hull1, vals$color_options_list$hull2, vals$color_options_list$tips)
      
      if(length(colored_tips) == 0) {
        col_list_modified <-  list(vals$color_options_list$hull1, vals$color_options_list$hull2, vals$color_options_list$hull3)
      }
      if(length(colored_tips) > 0) { 
        if(colored_tips == selected_trait[1]) {
          col_list_modified <- list(vals$color_options_list$tips, vals$color_options_list$hull1, vals$color_options_list$hull2)
        } else {
          if(length(selected_trait) > 1) {
            if(colored_tips == selected_trait[2]) { 
              col_list_modified <- list(vals$color_options_list$hull1, vals$color_options_list$tips, vals$color_options_list$hull2 )
            }
          }
        }
      }
      
      
      for (i in 1:length(selected_trait)) {
        this_col <- selected_trait[i]
        tip_col_fac <- as.character(vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),this_col])
        lev <- length(unique(tip_col_fac))
        lev_options <- unique(tip_col_fac)
        col_mat <- unlist(col_list_modified[[i]]) 
        for(j in 1:length(unique(tip_col_fac))) {
          reordered_edges <- pr$x[which(tip_col_fac == lev_options[j]), c(as.numeric(input$pca_x_axis), as.numeric(input$pca_y_axis))]
          edge_points <- row.names(reordered_edges)[chull(reordered_edges)]
          polygon(x = pr$x[edge_points, as.numeric(input$pca_x_axis)], 
                  y = pr$x[edge_points, as.numeric(input$pca_y_axis)], 
                  col = adjustcolor(col_mat[j],alpha.f = .5), 
                  border = col_mat[j]) # copy this code to the export part of the morphospace
        }
      }
    }
  }, width = "auto", height = "auto")
  
  # Plots with warp grids
  
  plotRefToTarget_rx <- reactive({ 
    req(ref_rx())
    req(targ_rx())
    req(vals$warp_initiated)
    
    par(mar = c(0,0,0,0))
    plotRefToTarget(ref_rx(), targ_rx(), mag = as.numeric(input$warp_mag), label = input$warp_labels,
                    links = vals$links_df, bg = "transparent", method = input$warp_type, gridPars = gridPar_vals())
  })
  
  output$warp_grid_name <- renderText({
    req(vals$warp_initiated)
    req(targ_rx())
    paste("Warp Grid of", names(targ_rx()[1]), sep = " ")
  })
  
  output$warp_grid <- renderPlot(bg = "transparent",{ 
    req(vals$warp_initiated)
    plotRefToTarget_rx()  
  })
  
  tryObserve({
    if(input$tip_col_category != "all_1_col" | input$show_convex_hull_1 | input$show_convex_hull_2 | input$show_convex_hull_3) {
      color_options_all <- c(input$trait_colors_lev1, input$trait_colors_lev2, input$trait_colors_lev3,
                             input$trait_colors_lev4, input$trait_colors_lev5, input$trait_colors_lev6,
                             input$trait_colors_lev7, input$trait_colors_lev8, input$trait_colors_lev9,
                             input$trait_colors_lev10, input$trait_colors_lev11)
      i <- 0
      if(input$tip_col_category == "by_trait_1") { i <- 1 }
      if(input$tip_col_category == "by_trait_2") { i <- 2 }
      if(input$tip_col_category == "by_trait_3") { i <- 3 }
      if(input$tip_col_category == "csize") { i <- 4 }
      all_treatments <- c(input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment, "cont")
      hulls <- (1:3)[c(input$show_convex_hull_1, input$show_convex_hull_2, input$show_convex_hull_3)] # the last part of this is a placeholder so that hull exists as an item for if 
      tip_col_options <- NULL
      hull_1_col_options <- NULL
      hull_2_col_options <- NULL
      hull_3_col_options <- NULL
      
      if(i > 0){
        if("cont" == all_treatments[i]) { used_cols <- 2 } else {
          levs <- levels(as.factor(vals$trait_rx[,(i+1)]))
          used_cols <- length(levs)
        }
        tip_col_options <- color_options_all[1:used_cols]
        tip_col_options_left <- color_options_all[(used_cols+1):length(color_options_all)]
      } else { 
        tip_col_options <- NULL 
        tip_col_options_left <- color_options_all
      }
      
      if(input$show_convex_hull_1 | input$show_convex_hull_2 | input$show_convex_hull_3) {
        if(hulls[1] != i) {
          levs <- levels(as.factor(vals$trait_rx[,(hulls[1]+1)]))
          used_cols <- length(levs)
          hull_1_col_options <- tip_col_options_left[1:used_cols]
          tip_col_options_left <- tip_col_options_left[(used_cols+1):length(tip_col_options_left)]
        }
        if(length(hulls) > 1) {
          if(hulls[1] != i & hulls[2] != i) {
            levs <- levels(as.factor(vals$trait_rx[,(hulls[2]+1)]))
            used_cols <- length(levs)
            hull_2_col_options <- tip_col_options_left[1:used_cols]
          } 
          if(hulls[1] == i & hulls[2] != i) { # the second condition in here shouldnt be necessary
            levs <- levels(as.factor(vals$trait_rx[,(hulls[2]+1)]))
            used_cols <- length(levs)
            hull_1_col_options <- tip_col_options_left[1:used_cols]
          }
          
          if(length(hulls) == 3) {
            if(hulls[3] != i) {
              levs <- levels(as.factor(vals$trait_rx[,(hulls[3]+1)]))
              used_cols_hull_3 <- length(levs)
              hull_3_col_options <- tip_col_options_left[1:used_cols_hull_3]
            } 
            if(hulls[2] == i) {
              levs <- levels(as.factor(vals$trait_rx[,(hulls[3]+1)]))
              used_cols <- length(levs)
              hull_2_col_options <- tip_col_options_left[1:used_cols]
            }
          }
        }
      }
      
      vals$color_options_list <- list("tips" = tip_col_options, "hull1" = hull_1_col_options, "hull2" = hull_2_col_options, "hull3" = hull_3_col_options)
    }
  })
  
  output$legend_tip_col <- renderPlot(width = 140, height = 140, bg = "transparent", {
    color_options <- vals$color_options_list$tips
    
    i <- 1
    if(input$tip_col_category == "by_trait_1") { i <- 2 }
    if(input$tip_col_category == "by_trait_2") { i <- 3 }
    if(input$tip_col_category == "by_trait_3") { i <- 4 }
    if(input$tip_col_category == "csize") { i <- 5 }
    treatment <- c("none", input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment, "cont")[i]
    if(i == 5) { legend_name <- "Centroid Size"
    } else { legend_name <- colnames(vals$trait_rx)[i] }
    if(i != 5) {req(vals$trait_rx) }
    if(nchar(legend_name) > 20) { 
      broken_title <- strsplit(legend_name, " ")
      if(length(broken_title[[1]]) > 1) {
        pieces <- round(length(broken_title[[1]])/2)
        broken_title_1 <- paste(broken_title[[1]][1:pieces], sep=" ", collapse=" ")
        broken_title_2 <- paste(broken_title[[1]][(pieces+1):length(broken_title[[1]])], sep=" ", collapse=" ")
        legend_name <- paste(broken_title_1 , broken_title_2, sep = "\n") } else {
          full_length <- nchar(legend_name)
          broken_title <- substring(legend_name, c(1, full_length/2), c(full_length/2 - 1, full_length))
          legend_name <- paste(broken_title[1],broken_title[2], sep = "\n")
        }
    }
    if(treatment == "cont") {
      if(i == 5) {
        trait_data <- vals$csize
      } else {
        trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),i]
      }
      par(mar = c(0,0,0,0))
      plot.new()
      legend("bottomleft", legend = c(NA,
                                      round(min(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                      round(mean(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                      round(max(as.numeric(trait_data)),4)),
             fill = c("transparent", colorRampPalette(colors = c(input$trait_colors_lev1, input$trait_colors_lev2))(11)), box.col = "transparent",
             border = NA, y.intersp = .5, title = legend_name) 
    } 
    if(treatment == "disc") {
      trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),i]
      lev_options <- unique(trait_data)
      
      par(mar = c(0,0,0,0))
      plot.new()
      legend("bottomleft", legend = as.character(lev_options), 
             fill = color_options[1:length(lev_options)],  box.col = "transparent", 
             title = legend_name) 
    }
  })
  
  output$legend_hull_col_1 <- renderPlot(width = 140, height = 140, bg = "transparent",  {
    if(input$show_convex_hull_1 | input$show_convex_hull_2 | input$show_convex_hull_3) {
      
      i <- (1:3)[c(input$show_convex_hull_1, input$show_convex_hull_2, input$show_convex_hull_3)]
      tip_col_cat_exact <- (1:4)[c(input$tip_col_category == "by_trait_1", input$tip_col_category == "by_trait_2", input$tip_col_category == "by_trait_3", input$tip_col_category == "all_1_col")]
      all_treatments <- c(input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment)
      
      if (length(i) > 1) {
        if(tip_col_cat_exact != i[1]) {
          i <- i[1]
        } else {
          i <- i[2]
        }
      }
      
      color_options_pruned <- vals$color_options_list$hull1
      
      legend_name <- colnames(vals$trait_rx)[(i+1)]
      if(nchar(legend_name) > 20) { 
        broken_title <- strsplit(legend_name, " ")
        if(length(broken_title[[1]]) > 1) {
          pieces <- round(length(broken_title[[1]])/2)
          broken_title_1 <- paste(broken_title[[1]][1:pieces], sep=" ", collapse=" ")
          broken_title_2 <- paste(broken_title[[1]][(pieces+1):length(broken_title[[1]])], sep=" ", collapse=" ")
          legend_name <- paste(broken_title_1 , broken_title_2, sep = "\n") } else {
            full_length <- nchar(legend_name)
            broken_title <- substring(legend_name, c(1, full_length/2), c(full_length/2 - 1, full_length))
            legend_name <- paste(broken_title[1],broken_title[2], sep = "\n")
          }
      }
      
      if(all_treatments[i] == "cont") {
        trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),(i+1)]
        par(mar = c(0,0,0,0))
        plot.new()
        legend("bottomleft", legend = c(NA,
                                        round(min(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                        round(mean(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                        round(max(as.numeric(trait_data)),4)),
               fill = c("transparent", colorRampPalette(colors = c(color_options_pruned[1], color_options_pruned[2]))(11)), box.col = "transparent",
               border = NA, y.intersp = .5, title = legend_name) 
      } 
      if(all_treatments[i] == "disc") {
        trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),(i+1)]
        lev_options <- unique(trait_data)
        
        
        par(mar = c(0,0,0,0))
        plot.new()
        legend("bottomleft", legend = as.character(lev_options), 
               fill = color_options_pruned[1:length(lev_options)],  box.col = "transparent", 
               title = legend_name)
      }
    }
    
  })
  
  output$legend_hull_col_2 <- renderPlot(width = 140, height = 140, bg = "transparent", { 
    if((input$show_convex_hull_1 & input$show_convex_hull_2) |
       (input$show_convex_hull_1 & input$show_convex_hull_3) |
       (input$show_convex_hull_2 & input$show_convex_hull_3)) {
      
      i <- (1:3)[c(input$show_convex_hull_1, input$show_convex_hull_2, input$show_convex_hull_3)]
      tip_col_cat_exact <- (1:4)[c(input$tip_col_category == "by_trait_1", input$tip_col_category == "by_trait_2", 
                                   input$tip_col_category == "by_trait_3", input$tip_col_category == "all_1_col")]
      all_treatments <- c(input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment)
      
      if(length(i) == 2 & !any(i %in% tip_col_cat_exact)) {
        i <- i[2]
      }
      if(length(i) == 3) {
        if(!any(i %in% tip_col_cat_exact)) {
          i <- i[2]
        } else {
          i <- i[-(tip_col_cat_exact)]
          i <- i[2]
        }
      }
      legend_name <- colnames(vals$trait_rx)[(i+1)]
      if(nchar(legend_name) > 20) { 
        broken_title <- strsplit(legend_name, " ")
        if(length(broken_title[[1]]) > 1) {
          pieces <- round(length(broken_title[[1]])/2)
          broken_title_1 <- paste(broken_title[[1]][1:pieces], sep=" ", collapse=" ")
          broken_title_2 <- paste(broken_title[[1]][(pieces+1):length(broken_title[[1]])], sep=" ", collapse=" ")
          legend_name <- paste(broken_title_1 , broken_title_2, sep = "\n") } else {
            full_length <- nchar(legend_name)
            broken_title <- substring(legend_name, c(1, full_length/2), c(full_length/2 - 1, full_length))
            legend_name <- paste(broken_title[1],broken_title[2], sep = "\n")
          }
      }
      
      if(length(i) == 1) { # this excludes the one case where the original length of i is 2, but one of them   
        color_options_pruned <- vals$color_options_list$hull2
        
        if(all_treatments[i] == "cont") {
          trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),(i+1)]
          par(mar = c(0,0,0,0))
          plot.new()
          legend("bottomleft", legend = c(NA,
                                          round(min(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                          round(mean(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                          round(max(as.numeric(trait_data)),4)),
                 fill = c("transparent", colorRampPalette(colors = c(color_options_pruned[1], color_options_pruned[2]))(11)), box.col = "transparent",
                 border = NA, y.intersp = .5, title = legend_name) 
        } 
        if(all_treatments[i] == "disc") {
          trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),(i+1)]
          lev_options <- unique(trait_data)
          
          
          par(mar = c(0,0,0,0))
          plot.new()
          legend("bottomleft", legend = as.character(lev_options), 
                 fill = color_options_pruned[1:length(lev_options)],  box.col = "transparent", 
                 title = legend_name)
        }
      }
    }
  })
  
  output$legend_shape <- renderPlot(width = 140, height = 140, bg = "transparent",  {
    if(input$tip_pch  == "by_trait_1" | input$tip_pch  == "by_trait_2" | input$tip_pch  == "by_trait_3" ) {
      if(input$tip_pch  == "by_trait_1") {i <- 2} 
      if(input$tip_pch  == "by_trait_2") {i <- 3}
      if(input$tip_pch  == "by_trait_3") {i <- 4}
      tip_pch_fac <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),i]
      lev <- length(unique(tip_pch_fac))
      lev_options <- unique(tip_pch_fac)
      pch_options <- c(19,1,15,0,8,3,2,4,5,6,7,9,10,11,12,13,14,16,17,18)[1:lev]
      pch_mat <- cbind(as.character(lev_options), pch_options) 
      legend_name <- colnames(vals$trait_rx)[i]
      if(nchar(legend_name) > 20) { 
        broken_title <- strsplit(legend_name, " ")
        if(length(broken_title[[1]]) > 1) {
          pieces <- round(length(broken_title[[1]])/2)
          broken_title_1 <- paste(broken_title[[1]][1:pieces], sep=" ", collapse=" ")
          broken_title_2 <- paste(broken_title[[1]][(pieces+1):length(broken_title[[1]])], sep=" ", collapse=" ")
          legend_name <- paste(broken_title_1 , broken_title_2, sep = "\n") } else {
            full_length <- nchar(legend_name)
            broken_title <- substring(legend_name, c(1, full_length/2), c(full_length/2 - 1, full_length))
            legend_name <- paste(broken_title[1],broken_title[2], sep = "\n")
          }
      }
      par(mar = c(0,0,0,0))
      plot.new()
      legend("bottomleft", legend = pch_mat[,1], pch = as.numeric(pch_mat[,2]), box.col = "transparent", # x = c(0,1), y = c(1-legend_height,1)
             title = legend_name)
    }
  })
  
  output$phy_signal_results <- renderPrint({
    req(vals$PS_shape)
    summary(vals$PS_shape)
  })
  
  output$phy_signal_plot <- renderPlot(bg = "transparent", { 
    req(vals$PS_shape)  # below is an edited version of geomorph:::plot.physignal, just deleted the title of the plot
    K.val <- vals$PS_shape$random.K
    K.obs <- vals$PS_shape$phy.signal
    p <- vals$PS_shape$pvalue
    ndec <- nchar(1 / vals$PS_shape$permutations) - 2
    K.obs <- round(K.obs, ndec)
    p <- round(p, ndec)
    hist(K.val,30,freq=TRUE,col="gray",xlab="Phylogenetic Signal from Random Permutations (K)", cex.main=0.8, main = "")
    arrows(K.obs,50,K.obs,5,length=0.1,lwd=2)
    text(K.obs,53, "Observed K")
  })
  
  output$model_tested <- renderText({
    name <- "Model Tested: Shape ~ "
    vars <- NULL
    
    if(is.null(update_vals$independent_variables_selected)) { independent_variables <- input$independent_variables } else {
      independent_variables <- update_vals$independent_variables_selected
    }
    
    if("by_trait_1" %in% independent_variables) {vars <- c(vars, substring(names(vals$trait_names)[1], 4, nchar(names(vals$trait_names)[1])))}
    if("by_trait_2" %in% independent_variables) {vars <- c(vars, substring(names(vals$trait_names)[2], 4, nchar(names(vals$trait_names)[2])))}
    if("by_trait_3" %in% independent_variables) {vars <- c(vars, substring(names(vals$trait_names)[3], 4, nchar(names(vals$trait_names)[3])))}
    if("csize" %in% independent_variables) {vars <- c(vars, "Centroid Size")}
    
    if(length(vars)>1) { 
      if(is.null(update_vals$independent_variables_order_selected)) {
        vars <- input$independent_variables_order 
      } else { vars <- update_vals$independent_variables_order_selected }
    }
    #if(input$interactions_included) { # needs to be updated to involve update_vals$___
    #  if(length(independent_variables) == 2) {
    #    vars <- c(vars, paste(vars[1], vars[2], sep = "*"))
    #  }
    #  if(length(independent_variables) == 3) {
    #    vars <- c(vars, paste(vars[1], vars[2], sep = "*"), paste(vars[1], vars[3], sep = "*"), paste(vars[2], vars[3], sep = "*"))
    #  }
    #}
    name <- paste(name, vars[1], sep = "")
    if(length(vars)>1){
      for (i in 2:length(vars)){
        name <- paste(name, vars[i], sep = " + ")
      }
    }
    vals$mod_name <- name
    name
  })
  
  output$model_tested_2 <- renderText({
    if(is.null(vals$mod_name)){
      if(!is.null(isolate(update_vals$mod_name))) { isolate(update_vals$mod_name) }
    } else {
      vals$mod_name
    }
  })
  
  output$model_comparison_model_1 <- renderText({ # copied from model_tested
    name <- "Model 1: Shape ~ "
    vars <- NULL
    
    if(is.null(update_vals$independent_variables_model_1_selected)) { independent_variables_model_1 <- input$independent_variables_model_1 } else {
      independent_variables_model_1 <- update_vals$independent_variables_model_1_selected
    } 
    
    if("by_trait_1" %in% independent_variables_model_1) {vars <- c(vars, substring(names(vals$trait_names)[1], 4, nchar(names(vals$trait_names)[1])))}
    if("by_trait_2" %in% independent_variables_model_1) {vars <- c(vars, substring(names(vals$trait_names)[2], 4, nchar(names(vals$trait_names)[2])))}
    if("by_trait_3" %in% independent_variables_model_1) {vars <- c(vars, substring(names(vals$trait_names)[3], 4, nchar(names(vals$trait_names)[3])))}
    if("csize" %in% independent_variables_model_1) {vars <- c(vars, "Centroid Size")}
    
    if(length(vars)>1) { 
      if(is.null(update_vals$independent_variables_order_model_1_selected)) {
        vars <- input$independent_variables_order_model_1 
      } else { vars <- update_vals$independent_variables_order_model_1_selected }
    }
    #if(input$interactions_included) {
    #  if(length(input$independent_variables_model_1) == 2) {
    #    vars <- c(vars, paste(vars[1], vars[2], sep = "*"))
    #  }
    #  if(length(input$independent_variables_model_1) == 3) {
    #    vars <- c(vars, paste(vars[1], vars[2], sep = "*"), paste(vars[1], vars[3], sep = "*"), paste(vars[2], vars[3], sep = "*"))
    #  }
    #}
    name <- paste(name, vars[1], sep = "")
    if(length(vars)>1){
      for (i in 2:length(vars)){
        name <- paste(name, vars[i], sep = " + ")
      }
    }
    
    name
  })
  
  output$model_comparison_model_2 <- renderText({ # copied from model_tested
    name <- "Model 2: Shape ~ "
    vars <- NULL
    
    if(is.null(update_vals$independent_variables_model_2_selected)) { independent_variables_model_2 <- input$independent_variables_model_2 } else {
      independent_variables_model_2 <- update_vals$independent_variables_model_2_selected
    } 
    
    if("by_trait_1" %in% independent_variables_model_2) {vars <- c(vars, substring(names(vals$trait_names)[1], 4, nchar(names(vals$trait_names)[1])))}
    if("by_trait_2" %in% independent_variables_model_2) {vars <- c(vars, substring(names(vals$trait_names)[2], 4, nchar(names(vals$trait_names)[2])))}
    if("by_trait_3" %in% independent_variables_model_2) {vars <- c(vars, substring(names(vals$trait_names)[3], 4, nchar(names(vals$trait_names)[3])))}
    if("csize" %in% independent_variables_model_2) {vars <- c(vars, "Centroid Size")}
    
    if(length(vars)>1) { 
      if(is.null(update_vals$independent_variables_order_model_2_selected)) {
        vars <- input$independent_variables_order_model_2
      } else { vars <- update_vals$independent_variables_order_model_2_selected }
    }
    # if(input$interactions_included) {
    #   if(length(input$independent_variables_model_2) == 2) {
    #     vars <- c(vars, paste(vars[1], vars[2], sep = "*"))
    #   }
    #   if(length(input$independent_variables_model_2) == 3) {
    #     vars <- c(vars, paste(vars[1], vars[2], sep = "*"), paste(vars[1], vars[3], sep = "*"), paste(vars[2], vars[3], sep = "*"))
    #   }
    # }
    
    name <- paste(name, vars[1], sep = "")
    if(length(vars)>1){
      for (i in 2:length(vars)){
        name <- paste(name, vars[i], sep = " + ")
      }
    }
    
    name
  })
  output$model_comparison_model_3 <- renderText({ # copied from model_tested
    req(vals$trait_rx)
    req(input$independent_variables_model_3)
    if(input$independent_variables_model_3[1] != "Trait 1") {
      name <- "Model 3: Shape ~ "
      vars <- NULL
      
      if(is.null(update_vals$independent_variables_model_3_selected)) { independent_variables_model_3 <- input$independent_variables_model_3 } else {
        independent_variables_model_3 <- update_vals$independent_variables_model_3_selected
      } 
      
      if("by_trait_1" %in% independent_variables_model_3) {vars <- c(vars, substring(names(vals$trait_names)[1], 4, nchar(names(vals$trait_names)[1])))}
      if("by_trait_2" %in% independent_variables_model_3) {vars <- c(vars, substring(names(vals$trait_names)[2], 4, nchar(names(vals$trait_names)[2])))}
      if("by_trait_3" %in% independent_variables_model_3) {vars <- c(vars, substring(names(vals$trait_names)[3], 4, nchar(names(vals$trait_names)[3])))}
      if("csize" %in% independent_variables_model_3) {vars <- c(vars, "Centroid Size")}
      
      if(length(vars)>1) { 
        if(is.null(isolate(update_vals$independent_variables_order_model_3_selected))) {
          vars <- input$independent_variables_order_model_3
        } else { vars <- isolate(update_vals$independent_variables_order_model_3_selected) }
      }
      # if(input$interactions_included) {
      #   if(length(input$independent_variables_model_3) == 2) {
      #     vars <- c(vars, paste(vars[1], vars[2], sep = "*"))
      #   }
      #   if(length(input$independent_variables_model_3) == 3) {
      #     vars <- c(vars, paste(vars[1], vars[2], sep = "*"), paste(vars[1], vars[3], sep = "*"), paste(vars[2], vars[3], sep = "*"))
      #   }
      # }
      
      name <- paste(name, vars[1], sep = "")
      if(length(vars)>1){
        for (i in 2:length(vars)){
          name <- paste(name, vars[i], sep = " + ")
        }
      }
      name
    }
  })
  
  output$allometry_plot <- renderPlot(bg = "transparent", {
    req(vals$fit) 
    req(input$allometry_predictor)
    this_trait <- (2:5)[c(input$allometry_predictor == "by_trait_1", input$allometry_predictor == "by_trait_2", input$allometry_predictor == "by_trait_3",input$allometry_predictor == "csize" )]
    if(this_trait == 5) { 
      if(input$allometry_log_csize) {
        predictor <- log(vals$csize)
      } else { predictor <- vals$csize }
    } else { predictor <- vals$trait_rx[,this_trait] }
    this_color <- (1:4)[c(input$allometry_color == "all_one_color", input$allometry_color == "by_trait_1", input$allometry_color == "by_trait_2", input$allometry_color == "by_trait_3")]
    if(this_color == 1) { color <- rep("black", nrow(vals$trait_rx))} else {
      color <- as.factor(vals$trait_rx[,this_color])
    }
    geomorph:::plot.procD.lm(vals$fit, type = input$allometry_type, reg.type = input$allometry_reg_type, 
                             logsz = input$allometry_log_csize,
                             predictor = predictor, col = color, 
                             pch = 19, cex = as.numeric(input$allometry_pt_size))
    
  })
  
  #output$allometry_results <- renderText({
  #  paste("bla bla bla") # this exists in Linear Model
  #})
  
  # output$trajectory_plot <- renderPlot(bg = "transparent", {
  #   req(vals$TA)
  #   this_trait <- (2:4)[c(input$trajectory_group == "by_trait_1", input$trajectory_group == "by_trait_2", input$trajectory_group == "by_trait_3")]
  #   treatment <- as.factor(vals$trait_rx[,this_trait])
  #   TP <- plot(vals$TA, pch = 19, cex = 0.7, col = as.numeric(larvalMorph$treatment))
  #   add.trajectories(TP, traj.bg = 1:nlevels(larvalMorph$treatment), 
  #                    start.bg = 1:nlevels(larvalMorph$treatment),
  #                    end.bg = 1:nlevels(larvalMorph$treatment))
  #   
  # })
  # 
  # output$trajectory_results <- renderPrint({
  #   req(vals$fit)
  #   #this_group <- (2:4)[c(input$trajectory_group == "by_trait_1", input$trajectory_group == "by_trait_2", input$trajectory_group == "by_trait_3")]
  #   #treatment <- as.factor(vals$trait_rx[,this_group])
  #   #this_trait <- (2:4)[c(input$trajectory_trait == "by_trait_1", input$trajectory_trait == "by_trait_2", input$trajectory_trait == "by_trait_3")]
  #   #group <- as.factor(vals$trait_rx[,this_trait])
  #   
  #   data("larvalMorph")
  #   gdf <- geomorph.data.frame(Y.gpa, treatment = larvalMorph$treatment, 
  #                              family = larvalMorph$family)
  #   fit.common <- procD.lm(coords ~ log(Csize) + treatment/family, 
  #                          data = gdf, print.progress = FALSE)
  #   
  #   vals$TA <- trajectory.analysis(fit.common, 
  #                                  groups = gdf$treatment, 
  #                                  traj.pts = gdf$family,
  #                                  pca = TRUE, print.progress = FALSE)
  #   summary(vals$TA , attribute = "MD")
  # })
  
  output$model_comparison_fit <- renderPrint({
    req(vals$model_comparison)
    vals$model_comparison
  })
  
  output$model_comparison <- renderTable(rownames = T, na = " ", digits = 4, {
    req(vals$model_comparison)
    sum <- vals$model_comparison$table
    
    sum[,1] <- formatC(sum[,1], digits = 0)
    skiprows <- nrow(sum)
    sum[-skiprows,2] <- formatC(sum[-skiprows,2], digits = 0)
    
    if(nrow(sum) == 3) {
      row.names(sum) <- c("Model 1", "Model 2", "Total")
    } 
    if(nrow(sum) == 4) {
      row.names(sum) <- c("Model 1", "Model 2", "Model 3", "Total")
    } 
    sum
  })
  
  
  output$stats_shape_trait_overall_fit <- renderPrint({
    req(vals$fit)
    vals$fit
  })
  
  output$stats_shape_trait_overall <- renderTable(rownames = T, na = " ", digits = 7,  {
    req(vals$anova_table)
    
    vals$anova_table[,1] <- formatC(vals$anova_table[,1], digits = 0)
    skiprows <- nrow(vals$anova_table)
    vals$anova_table[-skiprows,4] <- formatC(vals$anova_table[-skiprows,4], digits = 3)
    skiprows <- c(nrow(vals$anova_table)-1,nrow(vals$anova_table)) 
    vals$anova_table[-skiprows,5] <- formatC(vals$anova_table[-skiprows,5], digits = 3)
    vals$anova_table[-skiprows,6] <- formatC(vals$anova_table[-skiprows,6], digits = 3)
    vals$anova_table[-skiprows,7] <- formatC(vals$anova_table[-skiprows,7], digits = 3)
    
    vals$anova_table
    
  })
  
  output$stats_shape_trait_pairwise <- renderTable(rownames = T, na = "", digits = 4, {
    req(vals$pairwise_table)
    vals$pairwise_table
  })
  
  output$stats_morphol_disp_variances <- renderTable(rownames = T, digits = 8, {
    req(vals$proc_vars)
    matrix(vals$proc_vars, dimnames = list(names(vals$proc_vars), "Var"))
  })
  
  output$stats_morphol_disp_pairwise <- renderTable(rownames = T, na = "", digits = 4, {
    req(vals$morphol_disp_table)
    vals$morphol_disp_table
  })
  
  output$stats_evol_rates <- renderTable(rownames = T, digits = 8, {
    req(vals$evol_rates)
    matrix(vals$evol_rates, dimnames = list(names(vals$evol_rates), " "))
  })
  
  output$stats_evol_rates_pairwise <- renderTable(rownames = T, digits = 4, {
    req(vals$evol_rates_pairwise)
    vals$evol_rates_pairwise
  })
  
  output$stats_modularity_test <- renderPrint({
    req(gpa_coords_rx())
    if(input$modularity_phylo_tf == F) {
      vals$modularity_test <- modularity.test(A = gpa_coords_rx(), partition = vals$modularity_groups, 
                                              iter = as.numeric(input$modularity_perm),
                                              print.progress = F)
    } else {
      req(vals$phy_rx)
      vals$modularity_test <- phylo.modularity(A = gpa_coords_rx(), partition.gp = vals$modularity_groups, 
                                               phy = vals$phy_rx, iter = as.numeric(input$modularity_perm),
                                               print.progress = F)
    }
    req(vals$modularity_test)
    summary(vals$modularity_test)
  })
  
  output$plot_modularity_visualization <- renderPlot(bg = "transparent", {
    req(vals$mshape_gpacoords_rx) 
    coords <- vals$mshape_gpacoords_rx
    point_cols <- as.character(vals$modularity_groups) 
    point_cols[which(point_cols == "1")] <- input$module_1_col
    point_cols[which(point_cols == "2")] <- input$module_2_col
    point_cols[which(point_cols == "3")] <- input$module_3_col
    point_cols[which(point_cols == "4")] <- input$module_4_col
    point_cols[which(point_cols == "5")] <- input$module_5_col
    point_cols[which(point_cols == "6")] <- input$module_6_col
    point_cols[which(point_cols == "7")] <- input$module_7_col
    point_cols[which(point_cols == "8")] <- input$module_8_col
    
    plot(matrix(coords, ncol = 2), axes = F, asp = 1, pch = 19, xlab = "", ylab = "", 
         col = point_cols, cex = as.numeric(input$plot_modularity_visualization_cex),
         main = NULL)
    if(!is.null(vals$links_df)) {
      for(i in 1:nrow(vals$links_df)) {
        links <- rbind(coords[vals$links_df[i,1],], coords[vals$links_df[i,2],])
        points(x = links[,1], y = links[,2], type = "l")
      }
    }
    text(matrix(coords, ncol = 2), labels = 1:nrow(coords), pos = 4, col = point_cols, 
         cex = as.numeric(input$plot_modularity_visualization_cex))
  })
  
  output$plot_modularity_test <- renderPlot(bg = "transparent", {
    req(vals$modularity_test)
    plot(vals$modularity_test)
  })
  
  output$stats_compare_multi_evol_rates <- renderPrint({
    req(gpa_coords_rx())
    req(vals$phy_rx)
    req(vals$modularity_groups)
    modularity_groups <- droplevels(as.factor(vals$modularity_groups))
    vals$compare_multi_evol_rates <- try(compare.multi.evol.rates(A = gpa_coords_rx(), 
                                                                  phy = vals$phy_rx, gp = modularity_groups, 
                                                                  iter = as.numeric(input$modularity_perm),
                                                                  print.progress = F), silent = T)
    if(inherits(vals$compare_multi_evol_rates, "try-error")) { vals$compare_multi_evol_rates <- NULL } else {
      summary(vals$compare_multi_evol_rates)
    }
  })
  
  output$plot_compare_multi_evol_rates <- renderPlot(bg = "transparent", {
    req(vals$compare_multi_evol_rates)
    plot(vals$compare_multi_evol_rates)
  })
  
  output$global_integration_plot <- renderPlot(bg = "transparent", {
    globalIntegration(A = gpa_coords_rx(), ShowPlot = T) 
  })
  
  output$integration_test_plot <- renderPlot(bg = "transparent", { # groups defined by_trait_2 when both are approrpriate
    req(input$integration_group_by)
    req(vals$modularity_groups)
    req(gpa_coords_rx())
    if(input$integration_group_by == "none"){
      if(input$integration_phylo_tf) {
        req(vals$phy_rx)
        vals$IT <- phylo.integration(gpa_coords_rx(), phy = vals$phy_rx, 
                                     partition.gp = vals$modularity_groups, iter = as.numeric(input$integration_test_perm), print.progress = F)
      } else vals$IT <- integration.test(gpa_coords_rx(), partition.gp = vals$modularity_groups, 
                                         iter = as.numeric(input$integration_test_perm), print.progress = F)
      if(!is.null(vals$IT$XScores)) {
        plot(vals$IT)
      }
    } else {
      i <- (2:4)[c(input$integration_group_by == "by_trait_1", input$integration_group_by == "by_trait_2", input$integration_group_by == "by_trait_3")]
      if(!is.null(vals$trait_rx)) {
        groupings <- as.factor(vals$trait_rx[,i])
        integ.test <- list(NA)
        if(input$integration_phylo_tf) {
          req(vals$phy_rx)
          for(i in 1:nlevels(groupings)){
            coords_subset <- gpa_coords_rx()[,,which(groupings == levels(groupings)[i])]
            phy_subset <- keep.tip(vals$phy_rx, dimnames(coords_subset)[[3]])
            integ.test.new <- phylo.integration(coords_subset, phy = phy_subset, partition.gp = vals$modularity_groups,
                                                iter = as.numeric(input$integration_test_perm), print.progress = F)
            integ.test[[i]] <- integ.test.new
          }
          input$integration_group_level
          vals$IT <- integ.test[[as.numeric(isolate(input$integration_group_level))]]
        } else {
          for(i in 1:nlevels(groupings)){
            coords.subset <- gpa_coords_rx()[,,which(groupings == levels(groupings)[i])]
            integ.test.new <- integration.test(coords.subset, partition.gp = vals$modularity_groups, 
                                               iter = as.numeric(input$integration_test_perm), 
                                               print.progress = F)
            integ.test[[i]] <- integ.test.new
          }
          input$integration_group_level # this is required so that the vals$ bit doesn't get triggered for this to run again if isolate taken out
          
          if(!is.null(isolate(input$integration_group_level))) {
            vals$IT <- integ.test[[as.numeric(isolate(input$integration_group_level))]]
          }
        }
        if(!is.null(vals$IT$XScores)) { plot(vals$IT) }
      }
    }
    if(nlevels(as.factor(vals$modularity_groups)) > 2) { 
      plot.new() 
      text(0.5,0,"Plot not available for more than 2 modules.")
    }
    
  })
  
  output$integration_test_results <- renderPrint({
    summary(vals$IT)
  })
  
  output$symmetry_plot <- renderPlot(bg = "transparent", { 
    if(input$run_symmetry_go > 0) {
      req(isolate(vals$Data_gpa))
      
      full.mat <- as.data.frame(isolate(input$symmetry_definitions)[-nrow(isolate(input$symmetry_definitions)),])
      
      vals$symmetry_replicate <- full.mat$Replicate
      vals$symmetry_ind <- full.mat$Individual
      vals$symmetry_side <- full.mat$Side
      
      if(length(unique(isolate(vals$symmetry_replicate))) != 1){ # if there are replicates
        symmetry_replicate <- isolate(vals$symmetry_replicate)
      } else {
        symmetry_replicate <- NULL
      }
      
      vals$bilat_symmetry <- bilat.symmetry(A = isolate(vals$Data_gpa), 
                                            ind = isolate(vals$symmetry_ind), 
                                            side = isolate(vals$symmetry_side), 
                                            replicate = symmetry_replicate, 
                                            land.pairs = isolate(vals$symmetry_land_pairs),
                                            object.sym = as.logical(input$symmetry_obj_sym), 
                                            iter = as.numeric(isolate(input$symmetry_perm)), 
                                            print.progress = F)
      
      plot(vals$bilat_symmetry)
      
    }
  })
  
  output$symmetry_results <- renderPrint({
    req(vals$bilat_symmetry)
    summary(vals$bilat_symmetry)
  })
  
  #### Exports ####
  
  # ??? fix so that plot rendering is not redundant. possible with ggplot, unsure otherwise.
  
  output$export_aligned_lms <- downloadHandler( 
    filename = function() {paste('lms_aligned.tps')},
    content = function(file) { writeland.tps(gpa_coords_rx(), file) }
  )
  
  output$export_plot_all_specimens <- downloadHandler(
    filename = function() { paste('all_specimens_aligned.pdf') },
    content = function(file) {
      pdf(file)
      req(gpa_coords_rx())
      lm_col <- rep(input$semilms_color_other, dim(gpa_coords_rx())[1])
      if(!anyNA(vals$curves_final[,2])){ # as long as that semis column isn't NAs
        lm_col[as.numeric(unlist(vals$curves_final[,c(1,3)]))] <- input$semilms_color_brackets 
        lm_col[as.numeric(unlist(vals$curves_final[,2]))] <- input$semilms_color
      }
      par(mar = c(2,6,2,2))
      plotAllSpecimens(gpa_coords_rx(),  label = T, links = vals$links_df, 
                       plot.param = list(txt.cex = 1.5, txt.col = input$semilms_color_labels,
                                         pt.bg = input$semilms_color_individlms, 
                                         link.col = input$semilms_color_links,
                                         mean.bg = lm_col))
      dev.off() 
    }
  ) 
  
  output$export_semilm_mat <- downloadHandler( 
    filename = function() {paste('semilms_matrix.csv')},
    content = function(file) {
      write.csv(vals$curves_final, file, row.names = F)
    }
  )
  
  output$export_visualize_outliers_all <- downloadHandler( 
    filename = function() { paste('outliers_plot.pdf') },
    content = function(file) { 
      
      req(gpa_coords_rx())
      grouping <- NULL
      column <- (2:4)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% input$outlier_group]
      if(length(column) > 0 & input$outlier_group_tf) { 
        grouping <- as.factor(vals$trait_rx[,column]) 
        grouping <- grouping[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1])]
      } else {grouping <- NULL}
      if(input$outlier_group_tf) {
        which_group_new <- as.numeric(input$outlier_group_level_plotted)
      } else { which_group_new <- 1 }
      
      x <- plotOutliers.ekb1(gpa_coords_rx(), groups = grouping)
      
      pdf(file)
      plotOutliers.ekb2(x, gpa_coords_rx(), groups = grouping, which_group = which_group_new, pt_cex = as.numeric(input$outlier_plot_pt_cex),
                        txt_cex = as.numeric(input$outlier_plot_txt_cex), show_point_names = input$outlier_plot_show_point_names_tf) 
      if(is.null(vals$outlier_row) == "FALSE") { points(x = vals$outlier_row[1], y = vals$outlier_row[2], col = "red", 
                                                        cex = as.numeric(input$outlier_plot_pt_cex) + 2, pch = 1) } # this adds a red circle around whichever specimen was selected and visualized
      dev.off() # copied from above. using a reactive alone doesn't work
      
    }
  )
  
  output$export_individual_specimen_shape <- downloadHandler( 
    filename = function() { paste(row.names(vals$outlier_row), "-landmarks.pdf", sep = "") },
    content = function(file) { 
      pdf(file)
      req(vals$outlier_row)
      if(is.null(vals$outlier_row)) {
        plot(1, type="n", axes=F, xlab="", ylab="")
      } else {
        grouping <- NULL
        column <- (2:4)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% input$outlier_group]
        if(length(column) > 0 & input$outlier_group_tf) { 
          grouping <- as.factor(vals$trait_rx[,column]) 
          grouping <- grouping[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1])]
        } else {grouping <- NULL}
        if(input$outlier_group_tf) {
          which_group_new <- as.numeric(input$outlier_group_level_plotted)
        } else { which_group_new <- 1 }
        
        x1 <- plotOutliers.ekb1(gpa_coords_rx(), groups = grouping)
        x <- plotOutliers.ekb2(x1, gpa_coords_rx(), groups = grouping, 
                               which_group = which_group_new, pt_cex = as.numeric(input$outlier_plot_pt_cex),
                               produce_plot = F, txt_cex = as.numeric(input$outlier_plot_txt_cex), 
                               show_point_names = input$outlier_plot_show_point_names_tf)
        y <- x[order(x, decreasing = TRUE)]
        outlier_df <- data.frame("x" = order(y, decreasing = TRUE), "y" = y)
        selected_outlier <- match(row.names(vals$outlier_row), names(x))
        lm_col <- rep(input$vis_outliers_color_other, dim(gpa_coords_rx())[1])
        if(!is.null(vals$curves_final)) {
          temp_mat <- matrix(vals$curves_final, ncol = 3, byrow = T)
          if(!anyNA(temp_mat[,2])){ 
            lm_col[as.numeric(unlist(vals$curves_final[,2]))] <- input$vis_outliers_color
          }
        }
        par(mar = c(0,2,4,0))
        plot(gpa_coords_rx()[,,selected_outlier], col = lm_col, pch = 19, cex = 2,
             main = paste("Selected Specimen: ", row.names(vals$outlier_row), sep = ""), 
             asp = 1, axes = F) # displays the selected specimen landmark placement
        text(x = gpa_coords_rx()[,,selected_outlier][,1]-0.01, y = gpa_coords_rx()[,,selected_outlier][,2] - 0.01, 
             labels = 1:(nrow(gpa_coords_rx()[,,selected_outlier])), col = input$vis_outliers_color_labels) # adds labels to the landmarks
        if(!is.null(vals$links_df)){
          for (i in 1:nrow(vals$links_df)) {
            seg_df <- gpa_coords_rx()[,,selected_outlier]
            xys <- seg_df[vals$links_df[i,],]
            segments(x0 = xys[1], y0 = xys[3],x1 = xys[2], y1 = xys[4], col = input$vis_outliers_color_links)
          }
        }
      }
      dev.off() # copied from above. using a reactive alone doesn't work
    }
  )
  
  output$export_phy_signal_plot <- downloadHandler(
    filename = function() { paste("phylogenetic_signal_plot.pdf")},
    content = function(file) {
      req(vals$PS_shape)
      pdf(file, width = 10, height = 6, pointsize = 14)
      K.val <- vals$PS_shape$random.K
      K.obs <- vals$PS_shape$phy.signal
      p <- vals$PS_shape$pvalue
      ndec <- nchar(1 / vals$PS_shape$permutations) - 2
      K.obs <- round(K.obs, ndec)
      p <- round(p, ndec)
      hist(K.val,30,freq=TRUE,col="gray",xlab="Phylogenetic Signal from Random Permutations (K)", cex.main=0.8, main = "")
      arrows(K.obs,50,K.obs,5,length=0.1,lwd=2)
      text(K.obs,53, "Observed K")
      dev.off()
    }
  )
  
  output$export_phy_signal_results  <- downloadHandler(
    filename = function() { paste("phylogenetic_signal_results.csv")},
    content = function(file) {
      req(vals$PS_shape)
      physig <- vals$PS_shape$phy.signal
      pval <- vals$PS_shape$pvalue
      Z <- vals$PS_shape$Z
      out<-c(Z, physig, pval)
      rownames(out) <- c("Z", "PhySig (K)", "Pval")
      write.csv(out, file)
    }
  )
  
  output$export_modularity_visualization_plot <- downloadHandler(
    filename = function() { paste("export_module_visualization_plot.pdf") },
    content = function(file) {
      pdf(file, width = 10, height = 6, pointsize = 8)
      req(gpa_coords_rx())
      coords <- vals$mshape_gpacoords_rx
      point_cols <- as.character(vals$modularity_groups) 
      point_cols[which(point_cols == "1")] <- input$module_1_col
      point_cols[which(point_cols == "2")] <- input$module_2_col
      point_cols[which(point_cols == "3")] <- input$module_3_col
      point_cols[which(point_cols == "4")] <- input$module_4_col
      point_cols[which(point_cols == "5")] <- input$module_5_col
      point_cols[which(point_cols == "6")] <- input$module_6_col
      point_cols[which(point_cols == "7")] <- input$module_7_col
      point_cols[which(point_cols == "8")] <- input$module_8_col
      
      plot(matrix(coords, ncol = 2), axes = F, asp = 1, pch = 19, xlab = "", ylab = "", 
           col = point_cols, cex = as.numeric(input$plot_modularity_visualization_cex),
           main = NULL)
      if(!is.null(vals$links_df)) {
        for(i in 1:nrow(vals$links_df)) {
          links <- rbind(coords[vals$links_df[i,1],], coords[vals$links_df[i,2],])
          points(x = links[,1], y = links[,2], type = "l")
        }
      }
      text(matrix(coords, ncol = 2), labels = 1:nrow(coords), pos = 4, col = point_cols, 
           cex = as.numeric(input$plot_modularity_visualization_cex))
      dev.off()
    }
  )   
  
  output$export_global_integration_plot <- downloadHandler(
    filename = function() { paste("global_integration_plot.pdf")},
    content = function(file) {
      pdf(file, width = 18, height = 13)
      globalIntegration(A = gpa_coords_rx(), ShowPlot = T) 
      dev.off()
    }
  )
  
  output$export_integration_test_plot <- downloadHandler(
    filename = function() { paste("integration_test_plot.pdf")},
    content = function(file) {
      pdf(file, width = 10, height = 7)
      if(input$integration_group_by == "none"){
        if(input$integration_phylo_tf) {
          req(vals$phy_rx)
          vals$IT <- phylo.integration(gpa_coords_rx(), phy = vals$phy_rx, 
                                       partition.gp = vals$modularity_groups, iter = as.numeric(input$integration_test_perm), print.progress = F)
        } else vals$IT <- integration.test(gpa_coords_rx(), partition.gp = vals$modularity_groups, 
                                           iter = as.numeric(input$integration_test_perm), print.progress = F)
        if(!is.null(vals$IT$XScores)) {
          plot(vals$IT)
        }
      } else {
        i <- (2:4)[c(input$integration_group_by == "by_trait_1", input$integration_group_by == "by_trait_2", input$integration_group_by == "by_trait_3")]
        groupings <- as.factor(vals$trait_rx[,i])
        integ.test <- list(rep(NA, nlevels(groupings)))
        if(input$integration_phylo_tf) {
          req(vals$phy_rx)
          for(i in 1:nlevels(groupings)){
            coords_subset <- gpa_coords_rx()[,,which(groupings == levels(groupings)[i])]
            phy_subset <- keep.tip(vals$phy_rx, dimnames(coords_subset)[[3]])
            integ.test.new <- phylo.integration(coords_subset, phy = phy_subset, partition.gp = vals$modularity_groups,
                                                iter = as.numeric(input$integration_test_perm), print.progress = F)
            integ.test[[i]] <- integ.test.new
          }
          input$integration_group_level
          vals$IT <- integ.test[[as.numeric(isolate(input$integration_group_level))]]
        } else {
          for(i in 1:nlevels(groupings)){
            coords.subset <- gpa_coords_rx()[,,which(groupings == levels(groupings)[i])]
            integ.test.new <- integration.test(coords.subset, partition.gp = vals$modularity_groups, 
                                               iter = as.numeric(input$integration_test_perm), print.progress = F)
            integ.test[[i]] <- integ.test.new
          }
          input$integration_group_level # this is required so that the vals$ bit doesn't get triggered for this to run again if isolate taken out
          
          vals$IT <- integ.test[[as.numeric(isolate(input$integration_group_level))]]
        }
        if(!is.null(vals$IT$XScores)) {
          plot(vals$IT)
        }
      }
      dev.off()
    }
  )
  
  output$export_symmetry_plot <- downloadHandler(
    filename = function() { paste("symmetry_plot.pdf")},
    content = function(file) {
      
      if(input$run_symmetry_go > 0) {
        req(isolate(vals$Data_gpa))
        pdf(file, width = 18, height = 13)
        full.mat <- as.data.frame(isolate(input$symmetry_definitions)[-nrow(isolate(input$symmetry_definitions)),])
        
        vals$symmetry_replicate <- full.mat$Replicate
        vals$symmetry_ind <- full.mat$Individual
        vals$symmetry_side <- full.mat$Side
        
        if(length(unique(isolate(vals$symmetry_replicate))) != 1){ # if there are replicates
          symmetry_replicate <- isolate(vals$symmetry_replicate)
        } else {
          symmetry_replicate <- NULL
        }
        
        vals$bilat_symmetry <- bilat.symmetry(A = isolate(vals$Data_gpa), 
                                              ind = isolate(vals$symmetry_ind), 
                                              side = isolate(vals$symmetry_side), 
                                              replicate = symmetry_replicate, 
                                              land.pairs = isolate(vals$symmetry_land_pairs),
                                              object.sym = as.logical(input$symmetry_obj_sym), 
                                              iter = as.numeric(isolate(input$symmetry_perm)), 
                                              print.progress = F)
        
        plot(vals$bilat_symmetry)
        dev.off()
        
      }
    }
  )
  
  output$export_rotation_loadings <- downloadHandler(
    filename = function() { paste("rotation_loadings.csv")},
    content = function(file) { 
      write.csv(pca_rx()$rotation, file)
    }
  )
  
  output$export_morphospace <- downloadHandler(
    filename = function() { paste("morphospace_plot.pdf") },
    content = function(file) { 
      pdf(file, width = 15, height = 15)
      req(vals$morphospace)
      
      pr <- pca_rx() 
      
      # the following section is modified from geomorph:::plot.gm.prcomp
      class(pr) <- "ordinate"
      if(!is.null(vals$tip_pch)) {input_pch <- as.numeric(vals$tip_pch)} else {input_pch <- 19}
      if(is.null(input$flip_x_axis)) { input.flip_axis <- NULL }
      if(round(input$flip_x_axis/2) != input$flip_x_axis/2) {
        input.flip_axis  <- 1 } else { input.flip_axis <- NULL}
      if(round(input$flip_y_axis/2) != input$flip_y_axis/2) {
        input.flip_axis  <- c(input.flip_axis, 2) } 
      input.flip_axis <- as.numeric(input.flip_axis)
      
      pr.plot <- RRPP:::plot.ordinate(pr, axis1 = as.numeric(input$pca_x_axis), axis2 = as.numeric(input$pca_y_axis), 
                                      pch = input_pch, cex = as.numeric(input$tip_cex),
                                      col = vals$tip_col, asp = 1,
                                      flip = input.flip_axis) # its unclear why the axis line types are red too
      
      abline(h = 0, lty = 2) # these are only here until Mike fixes the plot.ordinate function in RRPP
      abline(v = 0, lty = 2) # these are only here until Mike fixes the plot.ordinate function in RRPP
      
      if(input$show_tip_label) {
        text(pr.plot$points, rownames(pr.plot$points), adj = c(-(input$tip_cex)/15,-(input$tip_cex)/15),
             cex = as.numeric(input$tip_txt_cex), col = vals$tip_col)
      }# up to here
      
      # this part is first so that points appear over the top of the phylogeny branches
      if(input$include_phylo) {
        
        z <- vals$morphospace$phylo$phy.pcdata
        edges <- as.matrix(vals$phy_rx$edge)
        
        for (i in 1:NROW(edges)) {
          pts <- z[edges[i, ], ]
          points(pts, type = "l", col = input$edge_col, lwd = as.numeric(input$edge_width), lty = 1) 
        }
        ancs <- z[(length(vals$phy_rx$tip.label)+1):nrow(z),]
        points(ancs, pch = as.numeric(input$node_pch), cex = as.numeric(input$node_cex), col = input$node_col)
        
        if(input$show_node_label) {
          text(ancs, rownames(ancs), adj = c(-(input$node_cex)/15,-(input$node_cex)/15), 
               cex = as.numeric(input$node_txt_cex), col = input$node_txt_col) # all these can be changed if we want to make them options
        }
      }
      
      if(!is.null(vals$morpho_clicked)) {
        if(input$warp_comparison_start == "selected_obs") {
          points(x = vals$specimen_row_ref[,as.numeric(input$pca_x_axis)], y = vals$specimen_row_ref[,as.numeric(input$pca_y_axis)], col = "red", cex = as.numeric(input$tip_cex) + 1.2, pch = 1)
        }
        if(input$warp_comparison_end == "selected_obs") {
          points(x = vals$specimen_row_targ[,as.numeric(input$pca_x_axis)], y = vals$specimen_row_targ[,as.numeric(input$pca_y_axis)], col = "red", cex = as.numeric(input$tip_cex) + 1, pch = 1)
        } 
      }
      
      if(is.null(vals$morpho_dbclicked) == "FALSE") { 
        points(x = vals$projection_row[1], y = vals$projection_row[2], col = "red", cex = as.numeric(input$tip_cex) + 1, pch = 4)
      }
      
      if(input$warp_comparison_show_arrow == TRUE) {
        if(!is.null(input$warp_comparison_end)) {
          if(input$warp_comparison_start == "mean") { start.xy <- c(0,0)}
          if(input$warp_comparison_start == "selected_proj") { start.xy <- vals$projection_row[1:2]}
          if(input$warp_comparison_start == "selected_obs"| input$warp_comparison_start == "selected_obs_byname") { 
            start.xy <- c(as.numeric(vals$specimen_row_ref[,as.numeric(input$pca_x_axis)]), as.numeric(vals$specimen_row_ref[,as.numeric(input$pca_y_axis)])) }
          if(input$warp_comparison_end == "mean") { end.xy <- c(0,0)}
          if(input$warp_comparison_end == "selected_proj") { end.xy <- vals$projection_row[1:2]}
          if(input$warp_comparison_end == "selected_obs"| input$warp_comparison_end == "selected_obs_byname") { 
            end.xy <- c(as.numeric(vals$specimen_row_targ[,as.numeric(input$pca_x_axis)]), as.numeric(vals$specimen_row_targ[,as.numeric(input$pca_y_axis)]))  }
          arrows(x0 = start.xy[1], y0 = start.xy[2], x1 = end.xy[1], y1 = end.xy[2], col = "red", lty = 1, lwd = 2)
        }
      }
      
      if(input$show_convex_hull_1 | 
         input$show_convex_hull_2 | 
         input$show_convex_hull_3) {
        selected_trait <- (2:4)[c(input$show_convex_hull_1, input$show_convex_hull_2, input$show_convex_hull_3)]
        colored_tips <- (2:4)[c(input$tip_col_category == "by_trait_1", input$tip_col_category == "by_trait_2", input$tip_col_category == "by_trait_3")]
        
        col_list_modified <- list(vals$color_options_list$hull1, vals$color_options_list$hull2, vals$color_options_list$tips)
        
        if(length(colored_tips) == 0) {
          col_list_modified <-  list(vals$color_options_list$hull1, vals$color_options_list$hull2, vals$color_options_list$hull3)
        }
        if(length(colored_tips) > 0) { 
          if(colored_tips == selected_trait[1]) {
            col_list_modified <- list(vals$color_options_list$tips, vals$color_options_list$hull1, vals$color_options_list$hull2)
          } else {
            if(length(selected_trait) > 1) {
              if(colored_tips == selected_trait[2]) { 
                col_list_modified <- list(vals$color_options_list$hull1, vals$color_options_list$tips, vals$color_options_list$hull2 )
              }
            }
          }
        }
        
        
        for (i in 1:length(selected_trait)) {
          this_col <- selected_trait[i]
          tip_col_fac <- as.character(vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),this_col])
          lev <- length(unique(tip_col_fac))
          lev_options <- unique(tip_col_fac)
          col_mat <- unlist(col_list_modified[[i]]) 
          for(j in 1:length(unique(tip_col_fac))) {
            reordered_edges <- pr$x[which(tip_col_fac == lev_options[j]), c(as.numeric(input$pca_x_axis), as.numeric(input$pca_y_axis))]
            edge_points <- row.names(reordered_edges)[chull(reordered_edges)]
            polygon(x = pr$x[edge_points, as.numeric(input$pca_x_axis)], 
                    y = pr$x[edge_points, as.numeric(input$pca_y_axis)], 
                    col = adjustcolor(col_mat[j],alpha.f = .5), 
                    border = col_mat[j]) # copy this code to the export part of the morphospace
          }
        }
      }
      
      
      if(any(input$tip_col_category == 'by_trait_1', input$show_convex_hull_1, 
             input$tip_col_category == 'by_trait_2', input$show_convex_hull_2, 
             input$tip_col_category == 'by_trait_3', input$show_convex_hull_3,
             input$tip_col_category == "csize")){
        
        plot.new()
        color_options <- vals$color_options_list$tips
        
        i <- 1
        if(input$tip_col_category == "by_trait_1") { i <- 2 }
        if(input$tip_col_category == "by_trait_2") { i <- 3 }
        if(input$tip_col_category == "by_trait_3") { i <- 4 }
        if(input$tip_col_category == "csize") { i <- 5 }
        treatment <- c("none", input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment, "cont")[i]
        if(i == 5) { legend_name <- "Centroid Size" } else { legend_name <- colnames(vals$trait_rx)[i] }
        
        if(nchar(legend_name) > 20) { 
          broken_title <- strsplit(legend_name, " ")
          if(length(broken_title[[1]]) > 1) {
            pieces <- round(length(broken_title[[1]])/2)
            broken_title_1 <- paste(broken_title[[1]][1:pieces], sep=" ", collapse=" ")
            broken_title_2 <- paste(broken_title[[1]][(pieces+1):length(broken_title[[1]])], sep=" ", collapse=" ")
            legend_name <- paste(broken_title_1 , broken_title_2, sep = "\n") } else {
              full_length <- nchar(legend_name)
              broken_title <- substring(legend_name, c(1, full_length/2), c(full_length/2 - 1, full_length))
              legend_name <- paste(broken_title[1],broken_title[2], sep = "\n")
            }
        }
        if(treatment == "cont") {
          if(i == 5) {
            trait_data <- vals$csize
          } else {
            trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),i]
          }
          par(mar = c(0,0,0,0))
          legend(x = .25, y = .5, legend = c(NA,
                                             round(min(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                             round(mean(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                             round(max(as.numeric(trait_data)),4)),
                 fill = c("transparent", colorRampPalette(colors = c(input$trait_colors_lev1, input$trait_colors_lev2))(11)), box.col = "transparent",
                 border = NA, y.intersp = .5, title = legend_name)
        } 
        if(treatment == "disc") {
          trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),i]
          lev_options <- unique(trait_data)
          
          par(mar = c(0,0,0,0))
          legend(x = .25, y = .5, legend = as.character(lev_options), 
                 fill = color_options[1:length(lev_options)],  box.col = "transparent", 
                 title = legend_name) 
        }
        
        
        if(input$show_convex_hull_1 | input$show_convex_hull_2 | input$show_convex_hull_3) {
          
          i <- (1:3)[c(input$show_convex_hull_1, input$show_convex_hull_2, input$show_convex_hull_3)]
          tip_col_cat_exact <- (1:4)[c(input$tip_col_category == "by_trait_1", input$tip_col_category == "by_trait_2", input$tip_col_category == "by_trait_3", input$tip_col_category == "all_1_col")]
          all_treatments <- c(input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment)
          
          if (length(i) > 1) {
            if(tip_col_cat_exact != i[1]) {
              i <- i[1]
            } else {
              i <- i[2]
            }
          }
          
          color_options_pruned <- vals$color_options_list$hull1
          
          legend_name <- colnames(vals$trait_rx)[(i+1)]
          if(nchar(legend_name) > 20) { 
            broken_title <- strsplit(legend_name, " ")
            if(length(broken_title[[1]]) > 1) {
              pieces <- round(length(broken_title[[1]])/2)
              broken_title_1 <- paste(broken_title[[1]][1:pieces], sep=" ", collapse=" ")
              broken_title_2 <- paste(broken_title[[1]][(pieces+1):length(broken_title[[1]])], sep=" ", collapse=" ")
              legend_name <- paste(broken_title_1 , broken_title_2, sep = "\n") } else {
                full_length <- nchar(legend_name)
                broken_title <- substring(legend_name, c(1, full_length/2), c(full_length/2 - 1, full_length))
                legend_name <- paste(broken_title[1],broken_title[2], sep = "\n")
              }
          }
          
          if(all_treatments[i] == "cont") {
            trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),(i+1)]
            par(mar = c(0,0,0,0))
            
            legend(x = .5, y = .5, legend = c(NA,
                                              round(min(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                              round(mean(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                              round(max(as.numeric(trait_data)),4)),
                   fill = c("transparent", colorRampPalette(colors = c(color_options_pruned[1], color_options_pruned[2]))(11)), box.col = "transparent",
                   border = NA, y.intersp = .5, title = legend_name) 
          } 
          if(all_treatments[i] == "disc") {
            trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),(i+1)]
            lev_options <- unique(trait_data)
            
            
            par(mar = c(0,0,0,0))
            
            legend(x = .5, y = .5, legend = as.character(lev_options), 
                   fill = color_options_pruned[1:length(lev_options)],  box.col = "transparent", 
                   title = legend_name)
          }
        }
        
        
        if((input$show_convex_hull_1 & input$show_convex_hull_2) |
           (input$show_convex_hull_1 & input$show_convex_hull_3) |
           (input$show_convex_hull_2 & input$show_convex_hull_3)) {
          
          i <- (1:3)[c(input$show_convex_hull_1, input$show_convex_hull_2, input$show_convex_hull_3)]
          tip_col_cat_exact <- (1:4)[c(input$tip_col_category == "by_trait_1", input$tip_col_category == "by_trait_2", input$tip_col_category == "by_trait_3", input$tip_col_category == "all_1_col")]
          all_treatments <- c(input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment)
          
          if(length(i) == 2 & !any(i %in% tip_col_cat_exact)) {
            i <- i[2]
          }
          if(length(i) == 3) {
            if(!any(i %in% tip_col_cat_exact)) {
              i <- i[2]
            } else {
              i <- i[-(tip_col_cat_exact)]
              i <- i[2]
            }
          }
          legend_name <- colnames(vals$trait_rx)[(i+1)]
          if(nchar(legend_name) > 20) { 
            broken_title <- strsplit(legend_name, " ")
            if(length(broken_title[[1]]) > 1) {
              pieces <- round(length(broken_title[[1]])/2)
              broken_title_1 <- paste(broken_title[[1]][1:pieces], sep=" ", collapse=" ")
              broken_title_2 <- paste(broken_title[[1]][(pieces+1):length(broken_title[[1]])], sep=" ", collapse=" ")
              legend_name <- paste(broken_title_1 , broken_title_2, sep = "\n") } else {
                full_length <- nchar(legend_name)
                broken_title <- substring(legend_name, c(1, full_length/2), c(full_length/2 - 1, full_length))
                legend_name <- paste(broken_title[1],broken_title[2], sep = "\n")
              }
          }
          
          if(length(i) == 1) { # this excludes the one case where the original length of i is 2, but one of them   
            color_options_pruned <- vals$color_options_list$hull2
            
            if(all_treatments[i] == "cont") {
              trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),(i+1)]
              par(mar = c(0,0,0,0))
              
              legend(x = .75, y = .5, legend = c(NA,
                                                 round(min(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                                 round(mean(as.numeric(trait_data)),4),NA,NA,NA,NA,
                                                 round(max(as.numeric(trait_data)),4)),
                     fill = c("transparent", colorRampPalette(colors = c(color_options_pruned[1], color_options_pruned[2]))(11)), box.col = "transparent",
                     border = NA, y.intersp = .5, title = legend_name) 
            } 
            if(all_treatments[i] == "disc") {
              trait_data <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),(i+1)]
              lev_options <- unique(trait_data)
              
              
              par(mar = c(0,0,0,0))
              
              legend(x = .75, y = .5, legend = as.character(lev_options),
                     fill = color_options_pruned[1:length(lev_options)],  box.col = "transparent", 
                     title = legend_name)
            }
          }
        }
      }
      
      if(input$tip_pch  == "by_trait_1" | input$tip_pch  == "by_trait_2" | input$tip_pch  == "by_trait_3" ) {
        if(input$tip_pch  == "by_trait_1") {i <- 2} 
        if(input$tip_pch  == "by_trait_2") {i <- 3}
        if(input$tip_pch  == "by_trait_3") {i <- 4}
        tip_pch_fac <- vals$trait_rx[match(dimnames(gpa_coords_rx())[[3]], vals$trait_rx[,1]),i]
        lev <- length(unique(tip_pch_fac))
        lev_options <- unique(tip_pch_fac)
        pch_options <- c(19,1,15,0,8,3,2,4,5,6,7,9,10,11,12,13,14,16,17,18)[1:lev]
        pch_mat <- cbind(as.character(lev_options), pch_options) 
        legend_name <- colnames(vals$trait_rx)[i]
        if(nchar(legend_name) > 20) { 
          broken_title <- strsplit(legend_name, " ")
          if(length(broken_title[[1]]) > 1) {
            pieces <- round(length(broken_title[[1]])/2)
            broken_title_1 <- paste(broken_title[[1]][1:pieces], sep=" ", collapse=" ")
            broken_title_2 <- paste(broken_title[[1]][(pieces+1):length(broken_title[[1]])], sep=" ", collapse=" ")
            legend_name <- paste(broken_title_1 , broken_title_2, sep = "\n") } else {
              full_length <- nchar(legend_name)
              broken_title <- substring(legend_name, c(1, full_length/2), c(full_length/2 - 1, full_length))
              legend_name <- paste(broken_title[1],broken_title[2], sep = "\n")
            }
        }
        par(mar = c(0,0,0,0))
        
        legend(x = .25, y = .25, legend = pch_mat[,1], pch = as.numeric(pch_mat[,2]), box.col = "transparent", # x = c(0,1), y = c(1-legend_height,1)
               title = legend_name)
      }
      dev.off()
    }
  )
  
  output$export_warp_grid <- downloadHandler(
    filename = function() { paste("warp_grid_plot.pdf") },
    content = function(file) { 
      pdf(file, width = 10, height = 10, pointsize = 8)
      req(vals$warp_initiated)
      req(ref_rx())
      par(mar = c(0,0,0,0))
      plotRefToTarget(ref_rx(), targ_rx(), mag = as.numeric(input$warp_mag), label = input$warp_labels,
                      links = vals$links_df, bg = "transparent", method = input$warp_type, gridPars = gridPar_vals())
      
      dev.off()
    }
  )
  
  output$export_aov_table <- downloadHandler(
    filename = function() { paste('ANOVA_stats_table.csv') },
    content = function(file) { write.csv(vals$anova_table, file) }
  )
  
  output$export_pairwise_table <- downloadHandler(
    filename = function() { paste('pairwise_stats_table.csv')},
    content = function(file) { write.csv(vals$pairwise_table, file) }
  )
  
  output$export_proc_variances <- downloadHandler( 
    filename = function() { paste('procrustes_group_variances.csv')},
    content = function(file) { write.csv(vals$proc_vars, file) }
  )
  
  output$export_disparity_pairwise_table <- downloadHandler(
    filename = function() { paste('pairwise_disparity_table.csv')},
    content = function(file) { write.csv(vals$morphol_disp_table, file) }
  )
  
  output$export_evol_rates <- downloadHandler(
    filename = function() { paste('evol_rates.csv')},
    content = function(file) { write.csv(vals$evol_rates, file) }
  )
  
  output$export_rates_pairwise_table <- downloadHandler(
    filename = function() { paste('pairwise_evol_rate_table.csv')},
    content = function(file) { write.csv(vals$evol_rates_pairwise, file) }
  )
  
  output$export_compare_multi_evol_rates_plot <- downloadHandler(
    filename = function() { paste("compare_multi_evol_rates_plot.pdf") },
    content = function(file) { 
      pdf(file, width = 10, height = 6, pointsize = 10)
      req(vals$compare_multi_evol_rates)
      plot(vals$compare_multi_evol_rates)
      dev.off()
    }
  )
  
  output$export_allometry_plot <- downloadHandler(
    filename = function() { paste("allometry_plot.pdf") },
    content = function(file) { 
      
      pdf(file, width = 10, height = 6, pointsize = 8)
      req(vals$fit) 
      req(input$allometry_predictor)
      this_trait <- (2:5)[c(input$allometry_predictor == "by_trait_1", input$allometry_predictor == "by_trait_2", input$allometry_predictor == "by_trait_3",input$allometry_predictor == "csize" )]
      if(this_trait == 5) { 
        if(input$allometry_log_csize) {
          predictor <- log(vals$csize)
        } else { predictor <- vals$csize }
      } else { predictor <- vals$trait_rx[,this_trait] }
      this_color <- (1:4)[c(input$allometry_color == "all_one_color", input$allometry_color == "by_trait_1", input$allometry_color == "by_trait_2", input$allometry_color == "by_trait_3")]
      if(this_color == 1) { color <- rep("black", nrow(vals$trait_rx))} else {
        color <- as.factor(vals$trait_rx[,this_color])
      }
      
      geomorph:::plot.procD.lm(vals$fit, type = input$allometry_type, reg.type = input$allometry_reg_type, 
                               logsz = input$allometry_log_csize,
                               predictor = predictor, col = color, pch = 19, cex = as.numeric(input$allometry_pt_size))
      dev.off()
    }
    
  )
  
  output$export_compare_multi_evol_rates_table <- downloadHandler(
    filename = function() { paste('compare_multi_evol_rates.csv')},
    content = function(file) { write.csv(vals$compare_multi_evol_rates, file) }
  )
  
  output$export_modularity_test_plot <- downloadHandler(
    filename = function() { paste("modularity_test_plot.pdf") },
    content = function(file) { 
      pdf(file, width = 10, height = 6, pointsize = 8)
      req(vals$modularity_test)
      plot(vals$modularity_test)
      dev.off()
    }
  )
  
  output$export_modularity_test_table <- downloadHandler(
    filename = function() { paste('modularity_test.csv')},
    content = function(file) { write.csv(vals$modularity_test, file) }
  )
  
  output$export_model_comparison <- downloadHandler(
    filename = function() { paste("model_comparison.csv")},
    content = function(file) { 
      req(vals$model_comparison)
      sum <- vals$model_comparison$table
      
      sum[,1] <- formatC(sum[,1], digits = 0)
      skiprows <- nrow(sum)
      sum[-skiprows,2] <- formatC(sum[-skiprows,2], digits = 0)
      
      if(nrow(sum) == 3) {
        row.names(sum) <- c("Model 1", "Model 2", "Total")
      } 
      if(nrow(sum) == 4) {
        row.names(sum) <- c("Model 1", "Model 2", "Model 3", "Total")
      } 
      sum
      write.csv(sum, file)
    }
  )
  
  #### Bookmarking ####
  
  onBookmark(function(state) {
    state$values$vals <- vals
    state$values$alert_vals <- alert_vals
    state$values$time <- Sys.time()
    
    update_vals <- reactiveValues()
    
    update_vals$trait_column_selected <- input$trait_column
    update_vals$semilms_manual_input_selected <- input$semilms_manual_input
    update_vals$pgls_ols_selected <- input$pgls_ols
    update_vals$outlier_group_level_plotted_selected <- input$outlier_group_level_plotted
    update_vals$outlier_group_tf_selected <- input$outlier_group_tf
    update_vals$independent_variables_selected <- input$independent_variables
    update_vals$independent_variables_model_1_selected <- input$independent_variables_model_1 
    update_vals$independent_variables_model_2_selected <- input$independent_variables_model_2
    update_vals$independent_variables_model_3_selected <- input$independent_variables_model_3
    update_vals$independent_variables_order_model_1_selected <- input$independent_variables_order_model_1 
    update_vals$independent_variables_order_model_2_selected <- input$independent_variables_order_model_2
    update_vals$independent_variables_order_model_3_selected <- input$independent_variables_order_model_3
    update_vals$tip_col_category_selected <- input$tip_col_category
    update_vals$outlier_group_selected <- input$outlier_group
    update_vals$remove_outlier_specimen_selected <- input$remove_outlier_specimen
    update_vals$integration_group_by_selected <- input$integration_group_by
    update_vals$allometry_color_selected <- input$allometry_color
    update_vals$trajectory_group_selected <- input$trajectory_group
    update_vals$trajectory_trait_selected <- input$trajectory_trait
    update_vals$tip_pch_selected <- input$tip_pch
    update_vals$show_convex_hull_1_selected <- input$show_convex_hull_1
    update_vals$show_convex_hull_2_selected <- input$show_convex_hull_2
    update_vals$show_convex_hull_3_selected <- input$show_convex_hull_3
    update_vals$phy_signal_input_selected <- input$phy_signal_input
    update_vals$allometry_predictor_selected <- input$allometry_predictor
    update_vals$independent_variables_order_selected <- input$independent_variables_order
    update_vals$modularity_group_1_selected <- input$modularity_group_1
    update_vals$modularity_group_2_selected <- input$modularity_group_2
    update_vals$modularity_group_3_selected <- input$modularity_group_3
    update_vals$modularity_group_4_selected <- input$modularity_group_4
    update_vals$modularity_group_5_selected <- input$modularity_group_5
    update_vals$modularity_group_6_selected <- input$modularity_group_6
    update_vals$modularity_group_7_selected <- input$modularity_group_7
    update_vals$modularity_group_8_selected <- input$modularity_group_8
    update_vals$trait_pairwise_selected <- input$trait_pairwise 
    update_vals$disparity_groups_selected <- input$disparity_groups
    update_vals$evol_rate_groups_selected <- input$evol_rate_groups
    update_vals$warp_axes_selected_selected <- input$warp_axes_selected
    update_vals$warp_specific_ref_specimen_selected <- input$warp_specific_ref_specimen
    update_vals$warp_specific_targ_specimen_selected <- input$warp_specific_targ_specimen
    update_vals$remove_outlier_specimen_unselected <- vals$outlier_removed_names
    update_vals$pca_x_axis_selected <- input$pca_x_axis
    update_vals$pca_y_axis_selected <- input$pca_y_axis
    update_vals$warp_comparison_start_selected <- input$warp_comparison_start
    update_vals$warp_comparison_end_selected <- input$warp_comparison_end
    update_vals$trait_1_treatment_selected <- input$trait_1_treatment
    update_vals$trait_2_treatment_selected <- input$trait_2_treatment
    update_vals$trait_3_treatment_selected <- input$trait_3_treatment
    update_vals$go_remove_outlier_specimen_selected <- input$go_remove_outlier_specimen
    update_vals$integration_group_level_selected <- input$integration_group_level
    update_vals$gpa_not_run_alert_selected <- input$gpa_not_run_alert
    update_vals$go_run_gpa_selected <- input$go_run_gpa
    update_vals$outlier_selected_selected <- vals$outlier_row
    update_vals$morphospace_specimen_click_selected <- vals$specimen_row_targ
    
    state$values$update_vals <- update_vals
    
  })
  
  # Read values from state$values when we restore
  onRestore(function(state) {
    
    bookmark_vals$time <- state$values$time
    
    vals$neg_lms <- state$values$vals$neg_lms
    vals$links_df <- state$values$vals$links_df
    vals$specimen_row_targ <- state$values$vals$specimen_row_targ
    vals$specimen_row_ref <- state$values$vals$specimen_row_ref
    vals$go_example_1 <- state$values$vals$go_example_1
    vals$curves_brushed <- state$values$vals$curves_brushed
    vals$curves_final <- state$values$vals$curves_final
    vals$links_dbclicked <- state$values$vals$links_dbclicked
    vals$csize <- state$values$vals$csize
    vals$tip_col <- state$values$vals$tip_col
    vals$modularity_groups <- state$values$vals$modularity_groups
    vals$plot <- state$values$vals$plot
    vals$outlier_row <- state$values$vals$outlier_row
    vals$two_disc_traits <- state$values$vals$two_disc_traits
    vals$tip_pch <- state$values$vals$tip_pch
    vals$lms_rx <- state$values$vals$lms_rx
    vals$PS_shape <- state$values$vals$PS_shape
    vals$links <- state$values$vals$links
    vals$estimates_cancelled <- state$values$vals$estimates_cancelled
    vals$one_disc_traits <- state$values$vals$one_disc_traits
    vals$phy_rx <- state$values$vals$phy_rx
    vals$trait_rx <- state$values$vals$trait_rx
    vals$three_disc_traits <- state$values$vals$three_disc_traits
    vals$morphospace <- state$values$vals$morphospace
    vals$trait_names <- state$values$vals$trait_names
    vals$semis_brushed <- state$values$vals$semis_brushed
    vals$curves_final_anyNAs <- state$values$vals$curves_final_anyNAs
    vals$outlier_removed_names <- state$values$vals$outlier_removed_names
    vals$curves <- state$values$vals$curves
    vals$color_options_list <- state$values$vals$color_options_list
    vals$morpho_clicked <- state$values$vals$morpho_clicked
    vals$morpho_dbclicked <- state$values$vals$morpho_dbclicked
    vals$prune_datasets <- state$values$vals$prune_datasets
    vals$projection_row <- state$values$vals$projection_row
    vals$warp_initiated <- state$values$vals$warp_initiated
    vals$links <- state$values$vals$links
    vals$alert_semilmmat_wrong_format <- state$values$vals$alert_semilmmat_wrong_format
    vals$semilms_manual <- state$values$vals$semilms_manual
    vals$trait_1_treatment <- state$values$vals$trait_1_treatment
    vals$trait_2_treatment <- state$values$vals$trait_2_treatment
    vals$trait_3_treatment <- state$values$vals$trait_3_treatment
    vals$fit <- state$values$vals$fit
    vals$anova_table <- state$values$vals$anova_table
    vals$proc_vars <- state$values$vals$proc_vars
    vals$morphol_disp_table <- state$values$vals$morphol_disp_table
    vals$evol_rates <- state$values$vals$evol_rates
    vals$evol_rates_pairwise <- state$values$vals$evol_rates_pairwise
    vals$pairwise_table <- state$values$vals$pairwise_table
    vals$plot_args <- state$values$vals$plot_args
    vals$TA <- state$values$vals$TA
    vals$modularity_groups <- state$values$vals$modularity_groups
    vals$modularity_test <- state$values$vals$modularity_test
    vals$compare_multi_evol_rates <- state$values$vals$compare_multi_evol_rates
    vals$IT <- state$values$vals$IT
    vals$col_names_temp <- state$values$vals$col_names_temp
    vals$trait_column_max_value <- state$values$vals$trait_column_max_value
    vals$go_pruning <- state$values$vals$go_pruning
    update_vals$mod_name <- vals$mod_name <- state$values$vals$mod_name
    
    
    
    alert_vals$navbar_datainput_welcomealertdone <- state$alert_vals$alert_vals$navbar_datainput_welcomealertdone
    alert_vals$navbar_datainput <- state$values$alert_vals$navbar_datainput
    alert_vals$navbar_datainput_examplealertdone <- state$values$alert_vals$navbar_datainput_examplealertdone
    alert_vals$navbar_datainput_filetpsalertdone <- state$values$alert_vals$navbar_datainput_filetpsalertdone
    alert_vals$navbar_datainput_filetraitalertdone <- state$values$alert_vals$navbar_datainput_filetraitalertdone
    alert_vals$navbar_datainput_dontmatchalertdone <- state$values$alert_vals$navbar_datainput_dontmatchalertdone
    alert_vals$navbar_dataprep <- state$values$alert_vals$navbar_dataprep
    alert_vals$navbar_dataprep_links <- state$values$alert_vals$navbar_dataprep_links
    alert_vals$navbar_dataprep_outliers <- state$values$alert_vals$navbar_dataprep_outliers
    alert_vals$navbar_dataprep_signal <- state$values$alert_vals$navbar_dataprep_signal
    alert_vals$navbar_dataprep_links_semialertdone <- state$values$alert_vals$navbar_dataprep_links_semialertdone
    alert_vals$navbar_dataprep_links_linkalertdone <- state$values$alert_vals$navbar_dataprep_links_linkalertdone
    alert_vals$navbar_morphospaceplot <- state$values$alert_vals$navbar_morphospaceplot
    alert_vals$navbar_morphospaceplot_tipcol <- state$values$alert_vals$navbar_morphospaceplot_tipcol
    alert_vals$navbar_morphospaceplot_tipcolalertdone <- state$values$alert_vals$navbar_morphospaceplot_tipcolalertdone
    alert_vals$navbar_morphospaceplot_tippch <- state$values$alert_vals$navbar_morphospaceplot_tippch
    alert_vals$navbar_morphospaceplot_tippchalertdone <- state$values$alert_vals$navbar_morphospaceplot_tippchalertdone
    alert_vals$navbar_morphospaceplot_warp <- state$values$alert_vals$navbar_morphospaceplot_warp
    alert_vals$navbar_morphospaceplot_warpalertdone <- state$values$alert_vals$navbar_morphospaceplot_warpalertdone
    alert_vals$navbar_linearmodels <- state$values$alert_vals$navbar_linearmodels
    alert_vals$navbar_linearmodels_modeldesign <- state$values$alert_vals$navbar_linearmodels_modeldesign
    alert_vals$navbar_linearmodels_modeldesignalertdone <- state$values$alert_vals$navbar_linearmodels_modeldesignalertdone
    alert_vals$navbar_linearmodels_allometry <- state$values$alert_vals$navbar_linearmodels_allometry
    alert_vals$navbar_linearmodels_allometryalertdone <- state$values$alert_vals$navbar_linearmodels_allometryalertdone
    alert_vals$navbar_linearmodels_trajectory <- state$values$alert_vals$navbar_linearmodels_trajectory
    alert_vals$navbar_linearmodels_trajectoryalertdone <- state$values$alert_vals$navbar_linearmodels_trajectoryalertdone
    
    alert_vals$navbar_exampletutorial <- state$values$alert_vals$navbar_exampletutorial
    alert_vals$gpa_not_ready <- state$values$alert_vals$gpa_not_ready
    alert_vals$prune_warning  <- state$values$alert_vals$prune_warning
    alert_vals$already_aligned_csize_done <- state$values$alert_vals$already_aligned_csize_done
    
    bookmark_vals$adjust_updates <- "true"
    
    update_vals$trait_column_selected <- state$values$update_vals$trait_column_selected
    update_vals$semilms_manual_input_selected <- state$values$update_vals$semilms_manual_input_selected
    update_vals$pgls_ols_selected <- state$values$update_vals$pgls_ols_selected
    update_vals$outlier_group_level_plotted_selected <- state$values$update_vals$outlier_group_level_plotted_selected
    update_vals$outlier_group_tf_selected <- state$values$update_vals$outlier_group_tf_selected
    update_vals$independent_variables_selected <- state$values$update_vals$independent_variables_selected
    update_vals$independent_variables_model_1_selected <- state$values$update_vals$independent_variables_model_1_selected
    update_vals$independent_variables_model_2_selected <- state$values$update_vals$independent_variables_model_2_selected
    update_vals$independent_variables_model_3_selected <- state$values$update_vals$independent_variables_model_3_selected
    update_vals$independent_variables_order_model_1_selected <- state$values$update_vals$independent_variables_order_model_1_selected
    update_vals$independent_variables_order_model_2_selected <- state$values$update_vals$independent_variables_order_model_2_selected
    update_vals$independent_variables_order_model_3_selected <- state$values$update_vals$independent_variables_order_model_3_selected
    update_vals$tip_col_category_selected <- state$values$update_vals$tip_col_category_selected
    update_vals$outlier_group_selected <- state$values$update_vals$outlier_group_selected
    update_vals$remove_outlier_specimen_selected <- state$values$update_vals$remove_outlier_specimen_selected
    update_vals$integration_group_by_selected <- state$values$update_vals$integration_group_by_selected
    update_vals$allometry_color_selected <- state$values$update_vals$allometry_color_selected
    update_vals$trajectory_group_selected <- state$values$update_vals$trajectory_group_selected
    update_vals$trajectory_trait_selected <- state$values$update_vals$trajectory_trait_selected
    update_vals$tip_pch_selected <- state$values$update_vals$tip_pch_selected
    update_vals$show_convex_hull_1_selected <- state$values$update_vals$show_convex_hull_1_selected
    update_vals$show_convex_hull_2_selected <- state$values$update_vals$show_convex_hull_2_selected
    update_vals$show_convex_hull_3_selected <- state$values$update_vals$show_convex_hull_3_selected
    update_vals$phy_signal_input_selected <- state$values$update_vals$phy_signal_input_selected
    update_vals$allometry_predictor_selected <- state$values$update_vals$allometry_predictor_selected
    update_vals$independent_variables_order_selected <- state$values$update_vals$independent_variables_order_selected
    update_vals$modularity_group_1_selected <- state$values$update_vals$modularity_group_1_selected
    update_vals$modularity_group_2_selected <- state$values$update_vals$modularity_group_2_selected
    update_vals$modularity_group_3_selected <- state$values$update_vals$modularity_group_3_selected
    update_vals$modularity_group_4_selected <- state$values$update_vals$modularity_group_4_selected
    update_vals$modularity_group_5_selected <- state$values$update_vals$modularity_group_5_selected
    update_vals$modularity_group_6_selected <- state$values$update_vals$modularity_group_6_selected
    update_vals$modularity_group_7_selected <- state$values$update_vals$modularity_group_7_selected
    update_vals$modularity_group_8_selected <- state$values$update_vals$modularity_group_8_selected
    update_vals$trait_pairwise_selected <- state$values$update_vals$trait_pairwise_selected
    update_vals$disparity_groups_selected <- state$values$update_vals$disparity_groups_selected
    update_vals$evol_rate_groups_selected <- state$values$update_vals$evol_rate_groups_selected
    update_vals$warp_axes_selected_selected <- state$values$update_vals$warp_axes_selected_selected
    update_vals$pca_x_axis_selected <- state$values$update_vals$pca_x_axis_selected 
    update_vals$pca_y_axis_selected <- state$values$update_vals$pca_y_axis_selected 
    update_vals$warp_comparison_start_selected <- state$values$update_vals$warp_comparison_start_selected
    update_vals$warp_comparison_end_selected <- state$values$update_vals$warp_comparison_end_selected
    update_vals$trait_1_treatment_selected <- state$values$update_vals$trait_1_treatment_selected
    update_vals$trait_2_treatment_selected <- state$values$update_vals$trait_2_treatment_selected
    update_vals$trait_3_treatment_selected <- state$values$update_vals$trait_3_treatment_selected
    update_vals$go_remove_outlier_specimen_selected <- state$values$update_vals$go_remove_outlier_specimen_selected
    update_vals$integration_group_level_selected <- state$values$update_vals$integration_group_level_selected
    update_vals$gpa_not_run_alert_selected <- state$values$update_vals$gpa_not_run_alert_selected
    update_vals$go_run_gpa_selected <- state$values$update_vals$go_run_gpa_selected
    update_vals$outlier_selected_selected <- state$values$update_vals$outlier_selected_selected
    update_vals$morphospace_specimen_click_selected <- state$values$update_vals$morphospace_specimen_click_selected
    update_vals$warp_specific_ref_specimen_selected <- state$values$update_vals$warp_specific_ref_specimen_selected
    update_vals$warp_specific_targ_specimen_selected <- state$values$update_vals$warp_specific_targ_specimen_selected
    update_vals$remove_outlier_specimen_unselected <- state$values$update_vals$remove_outlier_specimen_unselected
    
    
  })
  
  onRestored(function(state) {
    shinyalert(
      "Bookmark Opened!",
      paste("You've successfully returned to a previously bookmarked state of the Geomorph Shiny App. The full
      state of the App will take several seconds to fully load, so please be patient. <br></br>
      This state was saved on ", substring(bookmark_vals$time, 1, 10), " at ", substring(bookmark_vals$time, 12, 16), ".", sep = ""),
      type = "success",
      html = T
    )
    
    delay(10000, {
      for(i in names(update_vals)) update_vals[[i]] <- NULL
    }) # resetting the update_vals after a substantial delay
  })
  
  setBookmarkExclude(c("link_click_initiated", "link_click_end",  
                       "semis_selected", "file_tps", "file_phy", "file_trait",
                       "go_file_reset", "go_run_gpa"))
  
  #### End ####
  
  session$onSessionEnded(stopApp)
}

shinyApp(ui= ui, server = server, enableBookmarking = "server")



















