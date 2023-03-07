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
        so using the <strong>'Instructions'</strong> toggle in the bottom left-hand corner, currently 
       labeled <strong>'Instructions Off'</strong>. 
        <br><br>
        Video tutorials are also available on the <strong>Extras</strong> page.",
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
         so using the <strong>'Instructions'</strong> toggle in the bottom left-hand corner, currently 
        labeled <strong>'Instructions Off'</strong>.
        <br><br>
        Video tutorials are also available on the <strong>Extras</strong> page.",
        html = T,
        closeOnClickOutside = T,
        closeOnEsc = T,
        size = "s"
      )
    }
    
  })
  
  alert_vals <- reactiveValues() 
  bookmark_vals <- reactiveValues()
  
  
  tryObserveEvent(eventExpr = input$alert_on_off, {
    if(input$alert_on_off == FALSE) { 
      alert_vals$navbar_datainput <- NULL
      alert_vals$navbar_datainput_examplealertdone <- NULL
      alert_vals$navbar_datainput_filetpsalertdone <- NULL
      alert_vals$navbar_datainput_estimatemissingalertdone <- NULL
      alert_vals$navbar_datainput_filetraitalertdone <- NULL
      alert_vals$navbar_datainput_dontmatchalertdone <- NULL
      alert_vals$navbar_dataprep <- NULL
      alert_vals$navbar_dataprep_links <- NULL
      alert_vals$navbar_dataprep_outliers <- NULL
      alert_vals$navbar_dataprep_gpa <- NULL
      alert_vals$navbar_dataprep_links_semialertdone <- NULL
      alert_vals$navbar_dataprep_links_linkalertdone <- NULL
      alert_vals$navbar_morphospaceplot <- NULL
      alert_vals$navbar_morphospaceplot_tipcol <- NULL
      alert_vals$navbar_morphospaceplot_tipcolalertdone <- NULL
      alert_vals$navbar_morphospaceplot_tippch <- NULL
      alert_vals$navbar_morphospaceplot_tippchalertdone <- NULL
      alert_vals$navbar_morphospaceplot_phylo <- NULL
      alert_vals$navbar_morphospaceplot_warp <- NULL
      alert_vals$navbar_morphospaceplot_warpalertdone <- NULL
      alert_vals$navbar_shapepatterns <- NULL
      alert_vals$navbar_shapepatterns_signal <- NULL
      alert_vals$navbar_shapepatterns_modularity <- NULL
      alert_vals$navbar_shapepatterns_integration <- NULL
      alert_vals$navbar_shapepatterns_symmetry <- NULL
      alert_vals$navbar_shapepatterns_symmetry_startalertdone <- NULL
      alert_vals$navbar_shapepatterns_symmetry_typealertdone <- NULL
      alert_vals$navbar_linearmodels <- NULL
      alert_vals$navbar_linearmodels_modeldesign <- NULL
      alert_vals$navbar_linearmodels_modeldesignalertdone <- NULL
      alert_vals$navbar_linearmodels_modeldesignpairwisedone <- NULL
      alert_vals$navbar_linearmodels_allometry <- NULL
      alert_vals$navbar_linearmodels_allometryalertdone <- NULL
      alert_vals$navbar_linearmodels_modelcomparison <- NULL
      alert_vals$navbar_linearmodels_modelcomparisonalertdone <- NULL
      alert_vals$navbar_linearmodels_trajectoryanalysis <- NULL
      alert_vals$navbar_linearmodels_trajectoryanalysisalertdone <- NULL
      
    }
  }) # update to include all relevant alert_vals 
  
  tryObserveEvent(eventExpr = input$alert_on_off, ignoreInit = T, {
    if(input$alert_on_off == TRUE) {
      if(req(input$navbar) == "Data Input") {
        shinyalert(
          title = "Getting Started",
          text = "To practice using this App, you can load the 
        salamander head example dataset from geomorph (<span style='font-family: Courier New'>plethspecies</span>) by
        pressing the <strong>'Use Example Plethodon Data'</strong> button in the top right corner.
          <br></br>
          Otherwise, upload your own shape, phylogenetic, and/or trait data using the appropriate <strong>'Browse'</strong> buttons.
          <br><br>
          Throughout the App, some analyses or settings might take a few seconds to run. If the wheel in the top right corner
          of the screen is visible and spinning, that means things are being calculated, so we kindly ask for your patience. If your dataset 
          requires extreme computational efficiency, we advise learning to use geomorph directly in R. The machinery 
          behind gmShiny requires quite a few more interdependent processes, which may slow down computations of large datasets.",
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
          A button appears in the top right corner (<strong>'Clear All Inputs'</strong>) that can clear the uploaded data if you'd like to choose different datasets.
           <br></br>
           In the left-most column, you will see options for how to upload, for illustrative purposes, a TPS shape file. This is where 
           you can specify whether the shape data have already been aligned or not, from where to extract the specimen IDs, and whether negative landmarks 
           should be read in as missing data. 
           <br><br>
           The center column displays the uploaded phylogeny.
           <br><br>
           In the right-most column, you can select which columns of your uploaded trait file you wish to use downstream. 
          This functionality is limited to 3 columns of data at a time. 
          As you select the columns of interest,
          you can also specify whether each trait is discrete or continuous. If you select a continuous trait, you can apply a transformation 
          to the data before moving forward. 
          <br></br>
          When you're happy with your data, move on to the <strong>Data Prep</strong> page.",
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
                be run on the <strong>Data Prep</strong> page, but we recommend uploading any phylogenetic and/or trait data before moving forward.
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
  
  tryObserveEvent(eventExpr = input$neg_lms, ignoreInit = T, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_datainput_estimatemissingalertdone)) {
      delay(1000,if(input$neg_lms){
        shinyalert(
          title = "You've indicated that some of your landmark data are missing!",
          text = "You can see in the preview below that the landmarks with negative values have been replaced with NAs. <br><br>
          In the specimen alignment step (GPA), these missing landmarks will be estimated using the function, 
          <span style='font-family: Courier New'>estimate.missing</span> and the method <strong>TPS</strong>. 
          See the geomorph function help file for more details on this procedure.
                ",
          size = "s",
          type = "success",
          html = T
        )
      }
      )
      alert_vals$navbar_datainput_estimatemissingalertdone <- "yes"
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
          In the right-most section, under the <strong>Choose Trait File</strong> option,
          you can select which columns of your uploaded trait file you wish to use downstream. 
          <em>This functionality is limited to the use of up to 3 columns at a time</em>. 
          <br></br>
          As you select the columns of interest,
          you can also specify whether each trait is discrete or continuous. If you select a continuous trait, you can apply a transformation 
          to the data before moving forward. 
          <br></br>
          Upload all shape and phylogenetic data before moving on to the <strong>Data Prep</strong> page.
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
      
      if(req(input$navbar) == "Morphospace and Warp Grids"){
        alert_vals$navbar_morphospaceplot <- "Here"
        if(req(input$tip_col_category) != "all_1_col"){
          alert_vals$navbar_morphospaceplot_tipcol <- "Here"
        }
        if(req(input$show_convex_hull_1)){
          alert_vals$navbar_morphospaceplot_tipcol <- "Here"
        }
        if(input$include_phylo){
          alert_vals$navbar_morphospaceplot_phylo <- "Here"
        }
        if(!(req(input$tip_pch) %in% c("19", "1", "18"))){
          alert_vals$navbar_morphospaceplot_tippch <- "Here"
        }
      }
      
      if(req(input$navbar) == "Shape Patterns"){
        alert_vals$navbar_shapepatterns <- "Here" 
        if(req(input$tab_shapepatterns) == "Modularity"){
          alert_vals$navbar_shapepatterns_modularity  <- "Here" 
        }
        if(req(input$tab_shapepatterns) == "Integration"){
          alert_vals$navbar_shapepatterns_integration  <- "Here"
        }
        if(req(input$tab_shapepatterns) == "Symmetry"){
          alert_vals$navbar_shapepatterns_symmetry <- "Here"
        }
        if(req(input$tab_shapepatterns) == "Phylogenetic Signal"){
          alert_vals$navbar_shapepatterns_signal  <- "Here" 
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
        if(req(input$tab_linearmodels) == "Trajectory Analysis"){
          alert_vals$navbar_linearmodels_trajectoryanalysis <- "Here"  
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
        text = "This page has tabs for: <br> (1) defining landmark links and semilandmarks 
      (2) visualizing outliers or particular specimens of interest  
      & (3) performing the Generalized Procrustes Alignment. <br></br>
      If on the previous page you indicated that your landmarks are not yet aligned, 
      you are required to run the GPA before moving forward.
        <br><br>
        Now that you have progressed from the <strong>Data Input</strong> page, you will see buttons appear
        under every plot and analytical output allowing you to export the plot/results or the associated code. Pressing the 
        <strong>Export Plot</strong> or <strong>Export Results</strong> buttons will produce a .pdf or .csv file
        of the selected plot or results. Pressing the <strong>Export Code</strong> button will produce an .R file which 
        you can use to reproduce the plot or results in R or R Studio.
        <br><br>
        <em>If you choose to export the code, a zip file will be created with code and the current state 
        of the data and settings, along with a file with some supporting functions. Unzip the file, open the 
        appropriate .R file, and read the notes in order to ensure appropriate replication of the analyses.",
        size = "m",
        html = T
      )
    }
  })
  
  tryObserveEvent(alert_vals$navbar_dataprep_links, {
    if(!is.null(alert_vals$navbar_dataprep_links)){
      shinyalert(
        title = "Define Links and Semi-Landmarks",
        text = "On this tab, you will see a plot similar to one generated from the function 
        <span style='font-family: Courier New'>plotAllSpecimens</span>. <br> Use the <strong>'Flip LMs'</strong> buttons
        in the <strong>Settings</strong> panel to reorient the landmarks as is appropriate for your data.
      <br></br>
      To define links between landmarks, <strong style='color:#08a89e'>single-click</strong> 
      the landmarks you wish to connect in order, 
      ending the string by <strong style='color:#3691d1'>double-clicking</strong> the final linked landmark.
      <br></br>
      To assign semilandmarks, <strong style=color:#9560d1>click-and-drag</strong> to highlight the desired semilandmark(s). 
      If the selected semilandmark is linked to other landmarks, these linked landmarks will be automatically assigned as the 
      bracketing landmarks between which the semilandmark will slide during the GPA. 
       <br></br>
       The <strong>Semilandmark Matrix</strong> in the <strong>Settings</strong> panel is auto-filled by defining links and semilandmarks on the plot.
       However, you can also define or edit them directly in the matrix or by uploading a file (<strong>'Upload Semilandmark Matrix'</strong>). 
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
      can be reset at any time using the <strong>'Reset Landmark Links'</strong> button in the <strong>Settings</strong> panel on this tab. 
      <br></br>
      Links are also used to define the bracketing landmarks around semilandmarks. 
          More related instructions appear if you select a semilandmark (<strong style=color:#9560d1>click-and-drag</strong>).",
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
          text = "The semilandmark selection has been recorded in the <strong>Semilandmark Matrix</strong> in the <strong>Settings</strong> panel.<br></br> 
      Before performing the GPA, you will need to assign the bracketing landmarks along which each semilandmark 
      will slide. This may have been filled in automatically if you have already defined links through the selected semilandmark(s). 
      <br><br>
      These <strong>'Before'</strong> and <strong>'After'</strong> landmarks can be overwritten manually or by defining new links through the desired semilandmark(s).
          
          The <strong>Semilandmark Matrix</strong> can be reset using the <strong>'Reset Semilandmark Selection'</strong> button.
          Once your semilandmarks are finalized, press the <strong>'Apply Semilandmark Matrix'</strong>. This will initate a checking procedure
          to make sure the matrix is in the right format, and this must be done before running the GPA.",
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
      (1) manually entering landmark values into the matrix in the <strong>Settings</strong> panel, <br>
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
      If any of the traits uploaded on the <strong>Data Input</strong> page is discrete, 
      an option to <strong>'Visualize Outliers by Trait Group'</strong> will appear in the <strong>Settings</strong> panel, 
      with options to toggle between traits and levels used to generate the plot. This option is only available for 
      levels with more than 1 specimen in it.
      <br></br>
        This tab also allows you to remove specimens from all your datasets. If one of the specimens in your sample appears to be an outlier, 
        you can select said specimen from the <strong>'Exclude Specimen from Dataset'</strong> dropdown menu and press the <strong>'Remove'</strong> button. 
        All corresponding data will be excluded from subsequent analyses. 
        This action can be undone by pressing the <strong>'Undo Removal'</strong> button that appears after removal.",
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
        Before pressing the <strong>'Run GPA'</strong> button, make sure that the data treatments listed
        in the center column are correct. <br><br>
        This step must be completed before moving forward unless the uploaded shape data were previously aligned  
        (specified on the <strong>Data Input</strong> page). <br><br>
        Once the GPA is performed (function <span style='font-family: Courier New'>gpagen</span>), 
        the specimens' centroid sizes will be passed on as a trait that can be used throughout the App (e.g.,
        coloring data points on the <strong>Morphospace and Warp Grids</strong> page, 
        measuring phylogenetic signal on the <strong>Shape Patterns</strong> page, etc.). ",
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
      Fix the error(s) and press <strong>'Apply Semilandmark Matrix'</strong> again before moving forward.",
        type = "error",
        html = T,
        size = "m"
      )
    }
  })
  
  tryObserveEvent(input$tab_dataprep, ignoreInit = T, {
    if(input$tab_dataprep != "Define Links and Semi-Landmarks") {
      if(!is.null(vals$go_semilms_apply)) {
        if(vals$go_semilms_apply == 0) {
          if(nrow(input$semilms_manual_input) > 1) {
            updateTabsetPanel(session, "tab_dataprep", selected = "Define Links and Semi-Landmarks")
            shinyalert(
              title = "Apply Semilandmark Matrix",
              text = "Before moving to the next page, you must apply the semilandmark matrix using the 
        <strong>'Apply Semilandmark Matrix'</strong> button in the <strong>Settings</strong> panel. This will initiate a check to 
        ensure that the matrix is in the right format without errors.",
              type = "error",
              inputId = "alert_semis_not_applied",
              html = T,
              size = "s"
            )
          }
        }
      } else {
        if(nrow(input$semilms_manual_input) > 1) {
          updateTabsetPanel(session, "tab_dataprep", selected = "Define Links and Semi-Landmarks")
          shinyalert(
            title = "Apply Semilandmark Matrix",
            text = "Before moving to the next page, you must apply the semilandmark matrix using the 
        <strong>'Apply Semilandmark Matrix'</strong> button in the <strong>Settings</strong> panel. This will initiate a check to 
        ensure that the matrix is in the right format without errors.",
            type = "error",
            inputId = "alert_semis_not_applied",
            html = T,
            size = "s"
          )
        }
      }
    }
  })
  
  
  tryObserveEvent(alert_vals$navbar_morphospaceplot, {  
    if(input$alert_on_off == TRUE){
      shinyalert(
        title = "Morphospace and Warp Grids Page",
        text = "This page shows the shape data PCA plot generated from the functions 
      <span style='font-family: Courier New'>gm.prcomp</span> and <span style='font-family: Courier New'>plot</span>.
    <br></br>
    There are many ways to modify this plot in the <strong>Settings</strong> panel, including aspects of the alignment,
    which PC axes are displayed, the color, size, shape, and labels of the points, convex hulls,
    and whether to calculate and plot ancestral states along the phylogeny (if uploaded).
    <br></br> 
    If <strong>Point Color</strong> is set to <strong>'All One Color'</strong> (default), 
    you may select from one of the displayed color options below that drop down menu,
    or check the <strong>'Other'</strong> box, which allows you to select any R-defined color name. 
    Alternatively, the points could be
    colored by any uploaded trait data or, if applicable, centroid size. 
    <br></br>
    <strong>Warp Grids</strong> can also be displayed on this page.
    Initiate the warp grid options by <strong style='color:#08a89e'>single-clicking</strong> any point or <strong style='color:#3691d1'>double-clicking</strong> any region on the morphospace plot.",
        size = "m",
        html = T
      )
    }
  })
  
  tryObserveEvent(alert_vals$navbar_morphospaceplot_tipcol, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_morphospaceplot_tipcolalertdone)) {
      shinyalert(
        title = "You've selected to color the points or show convex hulls according to one of your traits!",
        text = " 
      Below the <strong>'Point Color'</strong> drop down menu, several colored boxes have appeared that allow you to 
      define the group color of each level. 
      If the selected trait is continuous, the two boxes represent the <strong>start</strong> 
      and <strong>end</strong> of a color spectrum, 
      along which the points are colored according to their value. If the trait is discrete, the boxes correspond to the levels of said trait.
      <br></br>
      A legend appeared on the left side of the morphospace plot, indicating which colors 
      belong to which trait levels.
      
      This legend can be repositioned with a <strong style=color:#9560d1>click-and-drag</strong>.",
        html = T,
        type = "success",
        size = "m"
      )
      alert_vals$navbar_morphospaceplot_tipcolalertdone <- "yes"
    }
  })
  
  tryObserveEvent(alert_vals$navbar_morphospaceplot_phylo, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_morphospaceplot_phyloalertdone)) {
      shinyalert(
        title = "You've opted to display a phylomorphospace!",
        text = "Additional options for adjusting visual components of the phylogeny are available
        by checking the <strong>'More Phylogeny Style Options'</strong> box that has appeared in the <strong>Settings</strong> panel.",
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
        title = "You've selected to define the point shapes by one of your traits!",
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
  
  tryObserveEvent(vals$warp_initiated, { 
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_morphospaceplot_warpalertdone)) {
      shinyalert(
        title = "You've initiated a warp grid!",
        text = "This warp grid was produced with the function 
        <span style='font-family: Courier New'>plotRefToTarget</span>, and 
        the page has been scrolled down to view the plot. The associated settings are now available at the bottom of the
        <strong>Settings</strong> panel.
        <br></br>
        Options for <strong>Warp Grid Magnitude</strong> and <strong>Warp Type</strong> correspond to the arguments in the 
        <span style='font-family: Courier New'>plotRefToTarget</span> function, 'mag' and 'method'.
        You can also choose to display the variation along particular PC axes (<strong>Variation Displayed</strong>).
        <br><br>
        The <strong>Warp Comparison Start (Reference)</strong> and <strong>Warp Comparison End (Target)</strong> 
        options correspond to the 
        two points of comparison for the warp grid. An <strong>Observed Point (Clicked)</strong> for either 
        the Reference or Target points can be selected by <strong style='color:#08a89e'>single-clicking</strong> 
        a point on the morphospace above. 
        Alternatively, a particular specimen can be selected using the <strong>Observed Point (By Name)</strong> option 
        for which a drop down menu will appear. 
        <br></br>
        You can also select a theoretical part of morphospace by <strong style='color:#3691d1'>double-clicking</strong>, 
        which can be assigned to either the Reference or Target points of the comparison. This option 
        will appear in the <strong>Warp Comparison</strong> options once said theoretical space is selected. 
        A similar exploration of theoretical shape warp grids can be done using the <span style='font-family: Courier New'>geomorph</span> function, 
        <span style='font-family: Courier New'>picknplot.shape</span> in R.
        <br><br>
        To clarify exactly 
        which shapes, observed or projected, are involved in the warp grid, an arrow has been added to the morphospace above
        showing the directionality between the selected Reference and Target points.
        This can be removed by unchecking the box <strong>'Show Comparison Trajectory'</strong>,
        <br></br>
        Even more stylistic options for the warp grid are available by checking <strong>'Show More Warp Grid Style Options'</strong>.",
        html = T,
        type = "success",
        size = "l"
      )
      alert_vals$navbar_morphospaceplot_warpalertdone <- "yes"
    }
  })
  
  
  tryObserveEvent(alert_vals$navbar_shapepatterns, {
    if(input$alert_on_off == TRUE){
      shinyalert(
        title = "Shape Patterns",
        text = "This page allows you to explore various shape patterns, 
        including <strong>Modularity</strong>, <strong>Integration</strong>, and <strong>Symmetry</strong>. 
        If a phylogeny has been uploaded, <strong>Phylogenetic Signal</strong> can also be quantified.",
        size = "s",
        html = T
      )
    }
  })
  
  tryObserveEvent(alert_vals$navbar_shapepatterns_modularity, {
    if(input$alert_on_off == TRUE) {
      shinyalert(
        title = "Modularity",
        text = "Here you will find options to test degree of modularity 
        (function <span style='font-family: Courier New'>modularity.test</span>) and, if a phylogeny is uploaded, evolutionary rate
        variation between modules <span style='font-family: Courier New'>compare.multi.evol.rates</span>. 
      <br></br>
      The first step is to define the number of modules and to specify which landmarks belong
      to each module by dragging the landmark number to the appropriate heading in the <strong>Settings</strong> panel. 
      Once all landmarks are assigned, double check the assignments with the <strong>Module Visualization</strong> plot
        in the main panel, then 
      press the <strong>'Assign LMs to Modules'</strong> button to finalize the module definitions. 
      ",
        size  = "m",
        html = T
      )
    }
  })
  
  tryObserveEvent(input$apply_modular_groups_go, {
    if(input$alert_on_off == TRUE & input$apply_modular_groups_go > 0& is.null(alert_vals$navbar_shapepatterns_modularitygoalertdone)) {
      shinyalert(
        title = "Modules Assigned!",
        text = "Now that your modules have been assigned, scroll down on the main panel to see the results.
        Under the heading <strong>Degree of Modularity</strong> is output from the function 
        <span style='font-family: Courier New'>modularity.test</span> or <span style='font-family: Courier New'>phylo.modularity</span>,
        depending on the option selected for <strong>'Evaluate Modularity in a Phylogenetic Context'</strong> in the <strong>Settings</strong> panel 
        (only available if a phylogeny is uploaded). This plot displays the null CR Coefficient distribution, with which the 
        p value and effect size of the observed value are determined.
      <br></br>
      The plot under the heading <strong>Evolutionary Rate Variation Between Modules</strong> 
      was generated using the function <span style='font-family: Courier New'>compare.multi.evol.rates</span>. 
        Similar to the CR plot, this displays the null distribution of the rate ratio between the modules. If more than
        2 modules have been defined, the observed rate ratio corresponds to the rate ratio between the most disparate group rates 
        (i.e., fastest evolving module:slowest evolving module).",
        size  = "m",
        html = T,
        type = "success"
      )
      alert_vals$navbar_shapepatterns_modularitygoalertdone <- 'yes'
    }
  })
  
  tryObserveEvent(alert_vals$navbar_shapepatterns_integration, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_shapepatterns_integrationgoalertdone)) {
      shinyalert(
        title = "Integration",
        text = "Here you can calculate various measures of integration. The first plot in this tab is of the global integration 
        generated from the function <span style='font-family: Courier New'>globalIntegration</span>. 
      <br></br>
      The results under the heading, <strong>Integration Between Modules</strong> displays the level of 
      integration between modules, which must be defined in the <strong>'Modularity'</strong> tab.
      The plot is generated using the <span style='font-family: Courier New'>integration.test</span> or 
      <span style='font-family: Courier New'>phylo.integration</span> function, 
      depending on the option selected for <strong>'Evaluate Integration in a Phylogenetic Context'</strong> 
      in the <strong>Settings</strong> panel 
        (only available if a phylogeny is uploaded). The plot visualizing PLS Block 1 vs Block 2 can only be generated
        when there are only 2 modules.
        <br><br>
         Integration between modules can also be calculated on a subset of 
         specimens as defined by any uploaded discrete trait 
       data and is specified in the <strong>Settings</strong> panel. Only traits for which 3 
        or more specimens belong to each level can be visualized.",
        size  = "m",
        html = T
      )
      alert_vals$navbar_shapepatterns_integrationgoalertdone <- 'yes'
    }
  })
  
  tryObserveEvent(alert_vals$navbar_shapepatterns_symmetry, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_shapepatterns_symmetry_startalertdone)) {
      shinyalert(
        title = "Symmetry",
        text = "Here you can calculate levels of symmetry and asymmetry in your shape data. These analyses
        are produced from the function <span style='font-family: Courier New'>bilat.symmetry</span>. 
        <br><br>
        This tab 
        can also enable you to pass on either the symmetry or asymmetric components of your shape variation
        after the symmetry parameters have been defined and the analyses have been run ('<strong>Use Symmetric 
        Component of Shape Variation</strong>' and <strong>Use Asymmetric 
        Component of Shape Variation</strong>' buttons in the <strong>Settings</strong> panel, respectively). Pressing
        either of these buttons will replace the shape data with the corresponding output throughout the App.
        <br><br>
        The first step is defining whether your shape files should be treated as <strong>Object</strong> or <strong>Matching</strong> 
        symmetry (<strong>Symmetry Type</strong> in the <strong>Settings</strong> panel). If each distinct specimen displays both
        sides of the symmetrical specimen, choose <strong>Object</strong> symmetry. If each distinct specimen represents only one side
        of the symmetrical specimen, choose <strong>Matching</strong>.
      <br></br>
      For <strong>Object</strong> symmetry (default), you must define which, if any, specimens represent replicates of single individual specimens. 
      This can be done manually or with a file upload (press associated buttons under <strong>Define Specimen Assignments</strong>). If 
      each specimen represents a new individual, press the <strong>No Replicates</strong> button.
      <br><br>
      You must also define which landmarks represent opposite sides of the symmetrical object. This is done under <strong>Assign Landmarks to Sides</strong>
      and can be manually entered or defined with a file upload. The appropriate file format has 2 columns, where each row represents the landmark numbers that are mirrored 
      across the plane of symmetry. Not all landmarks must be specified to a side, but none can be repeated, and all landmarks in a single column must correspond to a single side.
      <br><br>
      Once these symmetry parameters are defined, you should be able to visualize your definitions in the main panel under the
      <strong>Landmark Side Assignment Visualization</strong> heading. Once things look correct, press the <strong>Calculate</strong> to
      run these analyses.
        ",
        size  = "l",
        html = T
      )
      alert_vals$navbar_shapepatterns_symmetry_startalertdone <- 'yes'
    }
  })
  
  tryObserveEvent(input$symmetry_obj_sym, ignoreInit = T, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_shapepatterns_symmetry_typealertdone)) {
      shinyalert(
        title = "Matching Symmetry", 
        text = "You've now selected <strong>Matching</strong> symmetry for your data.
        <br><br>
        This type of symmetry analyses requires you to define three components of each specimen: individual, replicate, and side.
        These can be specified with a file upload or manually entered (press corresponding button under <strong>Define Specimen Assignments</strong>).
        <br><br>
        If your dataset does not have any replicates, each specimen row should have an independent identifier in the <strong>'Indiv'</strong>
        column, and a '1' in the <strong>'Rep'</strong> column. The side column should contain only '1's and '2's corresponding to the side 
        that specimen represents. See the video tutorials or the help file for 
        <span style='font-family: Courier New'>bilat.symmetry</span> for more details on this formating.
        <br><br>
        As with the <strong>Object</strong> symmetry analyses, you must press <strong>Calculate</strong> to initiate these analyses.",
        html = T,
        size = "m",
        inputId = "symmetry_land_pairs_file_input"
      )
      alert_vals$navbar_shapepatterns_symmetry_typealertdone <- 'yes'
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
        in the <strong>Settings</strong> panel.",
        size  = "s",
        html = T
      )
    }
  })
  
  tryObserveEvent(alert_vals$navbar_linearmodels, {  
    if(input$alert_on_off == TRUE){
      shinyalert(
        title = "Linear Models",
        text = "This page shows several ways to evaluate linear models with <span style='font-family: Courier New'>geomorph</span>. 
        All analyses are generated from the functions <span style='font-family: Courier New'>procD.pgls</span> or 
      <span style='font-family: Courier New'>procD.lm</span>. <br><br>
        For greater flexibility
        with these (and all) App operations, we advise learning to use <span style='font-family: Courier New'>geomorph</span> in R.",
        size = "s",
        html = T
      )
    }
  })
  
  # navbar_linearmodels_modeldesignpairwisedone
  tryObserveEvent(input$trait_pairwise, {  
    if(input$trait_pairwise != "no_run"){
      if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_linearmodels_modeldesignpairwisedone)){
        shinyalert(
          title = "Pairwise Analysis Selected",
          text = "This pairwise analysis has some limitations that the developers are hoping to expand upon in future versions of the app.
        <br><br>
        Currently, the pairwise test <strong>does not</strong> include:
        <li> The ability to define or indicate a specific null model </li>
        <li> An option for slopes </li>
        <li> An option to adjust confidence level</li>
        <li> A way to use interactions between traits as distinct pairwise groups </li>
        ",
          size = "s",
          type = "warning",
          html = T
        )
      }
      alert_vals$navbar_linearmodels_modeldesignpairwisedone <- 'yes'
    }
    
  })
  
  
  tryObserveEvent(alert_vals$navbar_linearmodels_modeldesign, {
    if(input$alert_on_off == T & is.null(alert_vals$navbar_linearmodels_modeldesignalertdone)) {
      shinyalert(
        title = "Model Design",
        text = "With this tab, you can investigate how shape varies with respect to 
        your uploaded trait and/or centroid size (if applicable). 
        
        <br><br>
        The first step is defining which independent variables you would like to include in the model (<strong>Independent Variables Tested</strong>). 
        If more than one trait is selected, an option will appear to change the order of the traits by dragging and dropping. 
        As you select traits and change their order, the defined model will be explicated at the top of the main panel (<strong>Model Tested:...</strong>).
        If you have uploaded any discrete trait data, you can also choose to run related analyses, such as pairwise comparisons, morphological disparity, and 
        evolutionary rate (functions <span style='font-family: Courier New'>pairwise</span>, <span style='font-family: Courier New'>morphol.disparity</span>, and 
        <span style='font-family: Courier New'>compare.evol.rates</span>, respectively;
        all options in the <strong>Settings</strong> panel).
        <br><br>Once you are happy with your model and all remaining analyses have been specified, press the <strong>'Calculate'</strong> button at the bottom
        of the <strong>Settings</strong> panel. If any settings are subsequently changed, 
        you will need to press <strong>'Calculate'</strong> again to rerun the analyses.
        Once this model is defined and calculated, you can visualize the allometry plot in the 
        <strong>Allometry</strong> tab. 
        <br><br>Once again, the flexibility of these model designs is limited compared to what is possible in geomorph. For 
        models that include interactions, for example, we advise learning to use <span style='font-family: Courier New'>geomorph</span> in R.",
        html = T,
        size = "l"
      )
      alert_vals$navbar_linearmodels_modeldesignalertdone <- "yes" 
    }
  })
  
  tryObserveEvent(alert_vals$navbar_linearmodels_allometry, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_linearmodels_allometryalertdone)) {
      shinyalert(
        title = "Allometry",
        text = "Using the model defined in the <strong>'Model Design'</strong> tab, here you will find the associated Allometry plot.
        If the <strong>'Calculate'</strong> button has not been pressed in the <strong>Model Design</strong> tab, no plot will appear, and 
        the settings will not have updated to include the appropriate options.
        <br><br> The settings <strong>Allometry Type</strong>,
        <strong>Regression Type</strong>, <strong>Predictor Variable</strong>, and <strong>Color Points</strong> correspond to the arguments 
        'type', 'reg.type', 'predictor', and 'col' in the <span style='font-family: Courier New'>procD.lm</span>
        function, respectively. 
        <br><br> 
        The <strong>Predictor Variable</strong> can be any uploaded continuous variable or centroid size (if applicable).
        If <strong>'Centroid Size'</strong> is selected, you can choose whether or not to log transform the centroid sizes 
        in the <strong>Settings</strong> panel. Finally, if you choose a <strong>Color Points</strong> option other than
        <strong>'All One Color'</strong>, boxes will appear, allowing you to specify the coloring scheme.",
        html = T,
        size = "m"
      )
      alert_vals$navbar_linearmodels_allometryalertdone <- "yes"
    }
  })
  
  tryObserveEvent(alert_vals$navbar_linearmodels_modelcomparison, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_linearmodels_modelcomparisonalertdone)) {
      shinyalert(
        title = "Model Comparison",
        text = "This tab is similar to the <strong>Model Design</strong> tab in how each model is defined. 
        How you define each model is reflected at the top of the main panel (<strong>Model 1:...</strong>).
        Up to three models can be compared (check box '<strong>Add a Third Model</strong>' will open up the appropriate options. 
        <br><br>
        Some analyses may run even if they are not statistically sound. For instance, an ANOVA table will be exported 
        even if two of your models are identical. Be sure to know and understand the statistical theory behind the models you are comparing,
        and their order, before interpreting these analyses..",
        html = T,
        size = "s"
      )
      alert_vals$navbar_linearmodels_modelcomparisonalertdone <- "yes"
    }
  })
  
  
  tryObserveEvent(alert_vals$navbar_linearmodels_trajectoryanalysis, {
    if(input$alert_on_off == TRUE & is.null(alert_vals$navbar_linearmodels_trajectoryanalysisalertdone)) {
      shinyalert(
        title = "Trajectory Analysis",
        text = "This tab is similar to the other tabs on this page in that the model being tested 
        can be found at the top of the main panel (<strong>'Model Tested:...'</strong>). 
        These analyses require a <strong>Trajectory Group</strong> and a <strong>Trajectory Trait</strong> and 
        are generated with the functions <span style='font-family: Courier New'>trajectory.analysis</span>,
        <span style='font-family: Courier New'>add.trajectories</span>, and 
        <span style='font-family: Courier New'>plot</span>.
        <br><br>
        Several components of this analysis must be specified before pressing <strong>'Calculate'</strong> in the 
        <strong>Settings</strong> panel, 
        and plotting options are available below that button.",
        html = T,
        size = "m"
      )
      alert_vals$navbar_linearmodels_trajectoryanalysisalertdone <- "yes"
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
          by pressing the <strong>'Prune Datasets to Match'</strong> button
          that has appeared near the top of the page. 
          <br></br>
          Note: this mismatch might simply be the result of inconsistent ordering of the specimens/species. 
          Even if this is the case, you must press the prune button, which will reorder the data to match.
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
        periods, dashes, spaces, or other formatting differences should be rectified and the fixed files 
        should be uploaded before attempting 
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
    wrong_tab <- (input$navbar != "Data Input" & input$navbar != "Extras")
    if(datasets_dont_match() & wrong_tab){
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
    req(gpa_coords_rx())
    if(input$navbar != "Data Prep" & input$navbar != "Data Input"  & input$navbar != "Extras") {
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
  tryObserveEvent(input$symmetry_obj_sym, ignoreInit = F, {
    
    vals$go_symmetry_file <-  NULL
    vals$go_symmetry_landpairs_file <- NULL
    vals$go_show_specimen_assignments <- NULL
    vals$go_show_landmark_assignments <- NULL
    vals$show_no_replicates_text <- NULL
    vals$run_symmetry_go <- NULL
    vals$bilat_symmetry <- NULL
    reset("symmetry_file_upload")
    reset("symmetry_land_pairs_file_upload")
    reset("run_symmetry_go")
    reset("go_symmetry_no_replicates")
    
    updateMatrixInput(session, "symmetry_definitions", 
                      value = symmetry_manual_matrix)
    updateMatrixInput(session, "symmetry_landpairs_definitions", 
                      value = symmetry_landpairs_manual_matrix)
    
    
  })
  
  tryObserveEvent(input$go_symmetry_no_replicates, ignoreInit = F, {
    
    if(input$go_symmetry_no_replicates > 0) {
      vals$go_symmetry_file <- NULL
      vals$go_show_specimen_assignments <- NULL
      
      
      reset("symmetry_file_upload")
      if(input$symmetry_obj_sym) {
        symmetry_manual_matrix_indrep1s <- matrix(1, ncol = 2, nrow = dim(gpa_coords_rx())[3])
        symmetry_manual_matrix_indrep1s[,1] <- 1:nrow(symmetry_manual_matrix_indrep1s)
        colnames(symmetry_manual_matrix_indrep1s) <- c("Indiv", "Rep")
        vals$show_no_replicates_text <- "go"
      } else {
        symmetry_manual_matrix_indrep1s <- matrix(1, ncol = 2, nrow = dim(gpa_coords_rx())[3])
        symmetry_manual_matrix_indrep1s[,1] <- 1:nrow(symmetry_manual_matrix_indrep1s)
        sym_def_df <- as.data.frame(input$symmetry_definitions)
        if(length(sym_def_df$Side) == dim(gpa_coords_rx())[3]) {
          symmetry_manual_matrix_indrep1s <- cbind(symmetry_manual_matrix_indrep1s, sym_def_df$Side)
        } else {
          symmetry_manual_matrix_indrep1s <-  cbind(symmetry_manual_matrix_indrep1s, NA)
        }
        colnames(symmetry_manual_matrix_indrep1s) <- c("Indiv", "Rep", "Side")
      }
      row.names(symmetry_manual_matrix_indrep1s) <- dimnames(gpa_coords_rx())[[3]]
      updateMatrixInput(session, "symmetry_definitions", 
                        value = symmetry_manual_matrix_indrep1s)
    }
  })
  
  tryObserveEvent(input$go_symmetry_file, ignoreInit = F,  priority = 10, {
    if(!is.null(input$go_symmetry_file)) {
      vals$go_symmetry_file <- input$go_symmetry_file
      vals$show_no_replicates_text <- NULL
    }
  })
  
  tryObserveEvent(input$go_symmetry_landpairs_file, ignoreInit = F,  priority = 10, {
    if(!is.null(input$go_symmetry_landpairs_file)){
      vals$go_symmetry_landpairs_file <- input$go_symmetry_landpairs_file
    }
  })
  
  tryObserveEvent(input$symmetry_file_upload, ignoreInit = T, {
    req(gpa_coords_rx())
    req(input$symmetry_file_upload)
    
    inFile <- input$symmetry_file_upload
    if (endsWith(inFile$name, '.xlsx') | endsWith(inFile$name, '.xls')| 
        endsWith(inFile$name, '.XLSX') | endsWith(inFile$name, '.XLS')){
      symmetry_file <- readxl::read_excel(inFile$datapath)
    } else if (endsWith(inFile$name, '.csv') | endsWith(inFile$name, '.CSV')){
      symmetry_file <- read.csv(inFile$datapath) # read in trait file (this cant be from the trait_rx or else the reactives become circular)
    } 
    symmetry_file <- as.matrix(symmetry_file)
    
    if(input$symmetry_obj_sym) {
      colnames(symmetry_file) <- c("Specimen", "Indiv", "Rep")
      if(ncol(symmetry_file) != 3) {
        shinyalert(
          title = "Incorrect File Format",
          text = "The file you have chosen is not in the correct format. 
        For Object symmetry analyses, the file must have 3 columns: specimen ID, numerical assignments 
        for Individuals, and numerical assignments for Replicates in that particular order.",
          type = "error",
          html = T
        )
      } else {
        if(anyNA(match(dimnames(gpa_coords_rx())[[3]], symmetry_file[,1]))) {
          shinyalert(
            title = "Mismatch in Specimen Names",
            text = "The specimen names in the first column in the uploaded file do not match the names of the specimens 
            in the shape dataset.",
            type = "error",
            html = T
          )
        } else {
          symmetry_file <- symmetry_file[match(dimnames(gpa_coords_rx())[[3]], symmetry_file[,1]), ]
          rownames(symmetry_file) <- symmetry_file[,1]
          symmetry_file <- symmetry_file[,-1]
          updateMatrixInput(session, "symmetry_definitions", value = symmetry_file)
          vals$go_show_specimen_assignments <- "go"
        }
      }
    } else {
      colnames(symmetry_file) <- c("Specimen", "Indiv", "Rep", "Side")
      if(ncol(symmetry_file) != 4) { # change what triggers this and what is accepted
        shinyalert(
          title = "Incorrect File Format",
          text = "The file you have chosen is not in the correct format. 
        For Matching symmetry analyses, the file must have 4 columns: specimen ID, numerical assignments 
        for Individuals, numerical assignments for Replicates, and numerical assignments 
        for Sides in that particular order.",
          type = "error",
          html = T
        )
      } else {
        if(anyNA(match(dimnames(gpa_coords_rx())[[3]], symmetry_file[,1]))) {
          shinyalert(
            title = "Mismatch in Specimen Names",
            text = "The specimen names in the first column in the uploaded file do not match the names of the specimens 
            in the shape dataset.",
            type = "error",
            html = T
          )
        } else {
          symmetry_file <- symmetry_file[match(dimnames(gpa_coords_rx())[[3]], symmetry_file[,1]), ]
          rownames(symmetry_file) <- symmetry_file[,1]
          symmetry_file <- symmetry_file[,-1]
          updateMatrixInput(session, "symmetry_definitions", value = symmetry_file)
          vals$go_show_specimen_assignments <- "go"
        }
      }
    }
  })
  
  tryObserveEvent(input$go_symmetry_manual, ignoreInit = T,{
    vals$go_symmetry_file <- NULL
    vals$show_no_replicates_text <- NULL
    vals$go_show_specimen_assignments <- "go"
    if(input$symmetry_obj_sym){
      symmetry_manual_matrix_expanded <- matrix(NA, nrow = dim(gpa_coords_rx())[3], ncol = 2)
      rownames(symmetry_manual_matrix_expanded) <- dimnames(gpa_coords_rx())[[3]]
      colnames(symmetry_manual_matrix_expanded) <- c("Indiv", "Rep")
      updateMatrixInput(session, "symmetry_definitions", 
                        value = symmetry_manual_matrix_expanded)
    } else {
      symmetry_manual_matrix_expanded <- matrix(NA, nrow = dim(gpa_coords_rx())[3], ncol = 3)
      rownames(symmetry_manual_matrix_expanded) <- dimnames(gpa_coords_rx())[[3]]
      colnames(symmetry_manual_matrix_expanded) <- c("Indiv", "Rep", "Side")
      updateMatrixInput(session, "symmetry_definitions", 
                        value = symmetry_manual_matrix_expanded)
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
    symmetry_land_pairs_file <- as.matrix(symmetry_land_pairs_file)
    colnames(symmetry_land_pairs_file) <- c("Side 1", "Side 2")
    vals$go_show_landmark_assignments <- "go"
    
    if(ncol(symmetry_land_pairs_file) != 2) { 
      shinyalert(
        title = "Incorrect File Format",
        text = "The file you have chosen is not in the correct format. 
        For Landmark Side Assignments, the file must have 2 columns: 1 for each side of the paired landmarks.",
        type = "error",
        html = T
      )
    } else {
      landpair_vec <- as.factor(symmetry_land_pairs_file)
      if(length(landpair_vec) != length(unique(landpair_vec))) {
        shinyalert(
          title = "Repeated Landmark",
          text = "One of the landmarks is repeated in the uploaded matrix. Please fix 
          this manually before applying the symmetry analyses.",
          type = "warning",
          html = T
        )
      } 
      if(any(!(landpair_vec %in% 1:dim(gpa_coords_rx())[1]))) {
        shinyalert(
          title = "Mislabeled Landmark",
          text = "One of the landmark numbers does not match the landmarks available.
          Either the numeric value is higher than the number of landmarks in the dataset or 
          some other character type is in the matrix. Please fix 
          this manually before applying the symmetry analyses.",
          type = "warning",
          html = T
        )
      }
      updateMatrixInput(session, "symmetry_landpairs_definitions",
                        value = symmetry_land_pairs_file)
    }
    
  })
  
  tryObserveEvent(input$go_symmetry_landpairs_manual, ignoreInit = T, priority = 10, {
    vals$go_symmetry_landmark_file <- NULL
    vals$go_show_landmark_assignments <- "go"
    updateMatrixInput(session, "symmetry_landpairs_definitions",
                      value = symmetry_landpairs_manual_matrix)
  })
  
  tryObserveEvent(input$go_symmetry_useoutput, ignoreInit = T, {
    
    vals$trait_rx <- NULL
    vals$phy_rx <- NULL 
    vals$go_example_1 <- NULL
    vals$csize <- NULL
    reset("file_phy")
    reset("file_trait")
    
    vals$symm_lms <- vals$bilat_symmetry$symm.shape
    
    shinyalert( title = "Symmetric Component Used!",
                text = "You have successfully pushed the <strong>symmetric</strong>
                component of shape variation forward to be used as the shape data throughout the app.
                <br><br>
                At this point, adjusting settings in the Symmetry tab may begin to cause errors. If you wish 
                to adjust the symmetry settings and push a new version of the symmetric component forward, 
                please clear out all symmetry settings and start again 
                using the 'Clear Symmetry Settings' button below.",
                type = "success",
                inputId = "symm_trigger",
                size = "m",
                html = T)
    hideTab(inputId = "navbar", target = "Linear Models") # hiding linear models because I haven't figured out a good solution that works for all symm outputs 
  })
  
  tryObserveEvent(input$go_asymmetry_useoutput, ignoreInit = T, {
    vals$trait_rx <- NULL
    vals$phy_rx <- NULL 
    vals$go_example_1 <- NULL
    vals$csize <- NULL
    reset("file_phy")
    reset("file_trait")
    reset("tip_col_category")
    
    vals$symm_lms <- vals$bilat_symmetry$asymm.shape 
    shinyalert( title = "Asymmetric Component Used!",
                text = "You have successfully pushed the <strong>asymmetric</strong>
                component of shape variation forward to be used as the shape data throughout the app.
                <br><br>
                At this point, adjusting settings in the Symmetry tab may begin to cause errors. If you wish 
                to adjust the symmetry settings and push a new version of the asymmetric component forward, 
                please clear out all symmetry settings and start again 
                using the 'Clear Symmetry Settings' button below.",
                type = "success",
                inputId = "asymm_trigger",
                size = "m",
                html = T)
    
    hideTab(inputId = "navbar", target = "Linear Models") # hiding linear models because I haven't figured out a good solution that works for all symm outputs 
  })
  
  tryObserveEvent(input$run_symmetry_go, ignoreInit = T, {
    if(!is.null(input$run_symmetry_go)){
      vals$run_symmetry_go <- input$run_symmetry_go
    }
  })
  
  bilat_metaO2 <- tryMetaObserve2({ # this doesn't export the code properly. not all tryMetaObserve2s fail !!!!
    metaExpr({
      if(!is.null(vals$run_symmetry_go)) {
        if(is.null(vals$run_symmetry_go_tracker)) { vals$run_symmetry_go_tracker <- 0 } # set it up
        if(vals$run_symmetry_go != vals$run_symmetry_go_tracker) { # this (and the above line in tryObserveEvent( for running symmetry)) is basically changing this into a observeEvent
          vals$run_symmetry_go_tracker <- vals$run_symmetry_go
          req(isolate(gpa_coords_rx()))
          gpa_coords <- isolate(gpa_coords_rx())
          vals$sym_def_df <- as.data.frame(isolate(input$symmetry_definitions))
          vals$symmetry_land_pairs <- isolate(input$symmetry_landpairs_definitions)
          
          
          symmetry_replicate <- isolate(vals$sym_def_df$Rep)
          if(length(which(symmetry_replicate == ""))>0) { symmetry_replicate <- symmetry_replicate[-which(symmetry_replicate == "")] }
          symmetry_ind <- isolate(vals$sym_def_df$Indiv)
          if(length(which(symmetry_ind == ""))>0) { symmetry_ind <- symmetry_ind[-which(symmetry_ind == "")]}
          
          if(isolate(vals$sym_def_df)[nrow(isolate(vals$sym_def_df)),1] == "") { 
            vals$sym_def_df <- isolate(vals$sym_def_df)[-nrow(isolate(vals$sym_def_df)),]}
          
          error1 <- length(symmetry_ind)/dim(gpa_coords)[3] != round(length(symmetry_ind)/dim(gpa_coords)[3])
          if(..(input$symmetry_obj_sym)) {
            error2_df <- paste(isolate(vals$sym_def_df$Indiv), isolate(vals$sym_def_df$Rep))
            error2 <- length(unique(error2_df)) != length(error2_df)
          } else {
            error2_df <- paste(isolate(vals$sym_def_df$Indiv), isolate(vals$sym_def_df$Rep), isolate(vals$sym_def_df$Side))
            error2 <- length(unique(error2_df)) != length(error2_df)
          }
          
          error3 <- anyNA(match(dimnames(gpa_coords)[[3]], rownames(isolate(vals$sym_def_df)))) # this compares the new shape to the old shape. obviously gonna throw an error
          error4 <- anyNA(match(rownames(isolate(vals$sym_def_df)), dimnames(gpa_coords)[[3]]))
          
          
          
          if(any(error1, error2, error3, error4)) {
            shinyalert(
              title = "Error in Specimen Assignments",
              text = "Some component of the specimen assignment data is in the incorrect format. Please check the Specimen Assignments matrix for errors. 
            <br><br>Some common issues are improperly 
          labeling replicates within each individual, leaving some values blank, or a mismatch between the row names
          and the labels for the shape dataset.",
              html= T,
              type = "error"
            )
          } else { 
            if(..(input$symmetry_obj_sym) == F) {
              symmetry_side <- isolate(vals$sym_def_df$Side)
              gdf <- geomorph.data.frame(shape = gpa_coords, 
                                         ind = symmetry_ind, 
                                         side = symmetry_side,
                                         replicate = symmetry_replicate)
            } else { 
              side <- NULL
              gdf <- geomorph.data.frame(shape = gpa_coords,
                                         ind = symmetry_ind, 
                                         replicate = symmetry_replicate)
              
              symmetry_land_pairs <- isolate(vals$symmetry_land_pairs)
              symmetry_land_pairs[,1] <- as.numeric(symmetry_land_pairs[,1])
              symmetry_land_pairs[,2] <- as.numeric(symmetry_land_pairs[,2])
              symmetry_land_pairs <- symmetry_land_pairs[complete.cases(symmetry_land_pairs),]
              symmetry_land_pairs <- matrix(symmetry_land_pairs, ncol = 2)
            }
            
            if(..(input$symmetry_obj_sym) == F) {
              error1 <- any(!(c(1,2) %in% symmetry_side))
              error2 <- any(!(symmetry_side %in% c(1,2)))
              error3 <- length(symmetry_side) != length(symmetry_ind)
              
              if(any(error1, error2, error3)) {
                shinyalert(
                  title = "Error in Side Assignments",
                  text = "Some component of the side assignment data is in the incorrect format. Some possible errors include:
  labeling a specimen with any label other than '1' or '2', an incomplete Specimen Assignments matrix, 
  or not assigning any specimens to one of the sides.",
                  html = T,
                  type = "error"
                )
              } else {
                
                if(..(input$raw_lms_already_aligned) == F) { 
                  vals$bilat_symmetry <- bilat.symmetry(A = vals$Data_gpa, 
                                                        ind = ind,
                                                        side = side, 
                                                        replicate = replicate, 
                                                        land.pairs = symmetry_land_pairs,
                                                        object.sym = as.logical(..(input$symmetry_obj_sym)), 
                                                        iter = isolate(..(input$symmetry_perm)), 
                                                        data = gdf,
                                                        print.progress = F)
                } else { 
                  vals$bilat_symmetry <- bilat.symmetry(A = shape, 
                                                        ind = ind, 
                                                        side = side, 
                                                        replicate = replicate, 
                                                        land.pairs = symmetry_land_pairs,
                                                        object.sym = as.logical(..(input$symmetry_obj_sym)), 
                                                        iter = isolate(..(input$symmetry_perm)), 
                                                        data = gdf,
                                                        print.progress = F) }
                
              }
            } else {
              symmetry_land_pairs_fac <- as.factor(symmetry_land_pairs)
              error1 <- length(symmetry_land_pairs_fac)/2 != round(length(symmetry_land_pairs_fac)/2)
              error2 <- any(!(symmetry_land_pairs_fac %in% 1:dim(gpa_coords)[1]))
              error3 <- length(symmetry_land_pairs_fac) != length(unique(symmetry_land_pairs_fac))
              if(any(error1, error2, error3)) {
                shinyalert(
                  title = "Error in Landmark Side Assignments",
                  text = "Some component of the landmark side assignment data is in the incorrect format. 
              Some possible errors include: a landmark number not available in the dataset assigned to either side (e.g., 
              LM 14 assigned to Side 1 when only 11 LMs exist), a landmark is repeated in the Landmark Side Assignments matrix,
              or one or more landmarks does not have a matched landmark on the other side.",
                  html= T,
                  type = "error"
                )
              } else {
                
                if(..(input$raw_lms_already_aligned) == F) { 
                  vals$bilat_symmetry <- bilat.symmetry(A = vals$Data_gpa, 
                                                        ind = ind, 
                                                        side = side, 
                                                        replicate = replicate, 
                                                        land.pairs = symmetry_land_pairs,
                                                        object.sym = as.logical(..(input$symmetry_obj_sym)), 
                                                        iter = isolate(..(input$symmetry_perm)), 
                                                        data = gdf,
                                                        print.progress = F)
                } else {
                  vals$bilat_symmetry <- bilat.symmetry(A = shape, 
                                                        ind = ind, 
                                                        side = side, 
                                                        replicate = replicate, 
                                                        land.pairs = symmetry_land_pairs,
                                                        object.sym = as.logical(..(input$symmetry_obj_sym)), 
                                                        iter = isolate(..(input$symmetry_perm)), 
                                                        data = gdf,
                                                        print.progress = F)
                }
              } 
            }
          } 
          
          
        } 
      } else { vals$bilat_symmetry <- NULL  }
      
    })
  })
  
  #### Input Data Reactives ####
  
  vals <- reactiveValues()
  
  lms_rx <- reactive({ # landmark reactive element
    ee <- input$go_remove_outlier_specimen_reset #trigger
    if(is.null(vals$go_example_1)){ # if the user is NOT using the sample pleth data
      if(input$shape_file_type == "TPSorNTS") { # if the user selects "TPS" as shape file type
        req(input$file_tps) # stop running this reactive if there is no input tps file selected
        inFile <- input$file_tps
        if (endsWith(inFile$name, '.nts')){
          temp_LMs <- readland.nts(inFile$datapath)
        } else if (endsWith(inFile$name, '.tps') | endsWith(inFile$name, '.TPS')){
          temp_LMs <- readland.tps(inFile$datapath, specID = input$spec_id, negNA = input$neg_lms,
                                   warnmsg = F) # read in the tps data with options
        }}
    } 
    if(input$shape_file_type == "StereoMorph") {
      req(input$file_stereomorph)
      inFile <- input$file_stereomorph
      
      shapes <- suppressWarnings(readShapes(inFile$datapath)) # suppressing warnings because the warnings are not informative 
      vals$file_stereomorph_shape_curven <- length(shapes$curves.control[[1]])
      
      if(length(shapes$curves.control[[1]]) > 0 & !is.null(vals$go_run_stereomorph_curves)) { # if there are curves
        curve_n_vec <- c(isolate(input$stereomorph_curve1_n), 
                         isolate(input$stereomorph_curve2_n), # create a vector the length of how many curves there are
                         isolate(input$stereomorph_curve3_n), 
                         isolate(input$stereomorph_curve4_n),
                         isolate(input$stereomorph_curve5_n), 
                         isolate(input$stereomorph_curve6_n))
        min_val <- min(length(shapes$curves.control[[1]]), 6)
        curve_n_vec <- curve_n_vec[1:min_val] # take all the curve data up to 6 curves, but no more
        if(length(shapes$curves.control[[1]]) > 6) {
          curve_n_vec <- c(curve_n_vec, rep(input$stereomorph_curve6_n, length(shapes$curves.control[[1]])-6)) 
          # repeating the number of curve points in curve # 6 across all subsequent curves
        }
        vals$curve_n_vec <- curve_n_vec
      } else {
        vals$curve_n_vec <- NULL # otherwise replace with NULL
      }
      shapesGM <- readland.shapes(shapes, nCurvePts = vals$curve_n_vec, continuous.curve = input$cont_curve) # will throw error if asked for 2LMs on a curve where the start and end LM are the same (pup fish eye)
      # Error in `rownames<-`(`*tmp*`, value = curve.mat.nms) : 
      # attempt to set 'rownames' on an object with no dimensions
      
      lm_mat <- matrix(NA, ncol = length(shapesGM$landmarks[[1]]), nrow = length(shapesGM$landmarks))
      for(i in 1:length(shapesGM$landmarks)) {
        for(j in 1:nrow(shapesGM$landmarks[[1]])) {
          for(k in 1:ncol(shapesGM$landmarks[[1]])) {
            if(k == 2) {  this_col <- (j*2) } else { this_col <- (j*2)-1  }
            lm_mat[i, this_col] <- shapesGM$landmarks[[i]][j,k]
          }
        }
      }
      
      shapearray <- arrayspecs(A = lm_mat, p = length(shapesGM$landmarks[[1]])/2, k = 2)
      shape_names <- sapply(strsplit(inFile$name, split='.txt', fixed=TRUE), function(x) (x[1]))
      dimnames(shapearray)[[3]] <- shape_names
      vals$shapesGM <- shapesGM
      temp_LMs <- shapearray
      
    }
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
    
    if(length(dimnames(temp_LMs)[[3]]) != length(unique(dimnames(temp_LMs)[[3]]))){
      name_levels <- unique(dimnames(temp_LMs)[[3]])
      for(i in 1:length(name_levels)) { # renaming all specimens for which there are repeat names as ".1" ".2" etc 
        these_specimens <- which(dimnames(temp_LMs)[[3]] == name_levels[i])
        if(length(these_specimens) > 1) {
          dimnames(temp_LMs)[[3]][these_specimens] <- paste(dimnames(temp_LMs)[[3]][these_specimens], 1:length(these_specimens), 
                                                            sep = ".")
        }
      }
    }
    if(dim(temp_LMs)[2] == 3) { # if the data are 3d, hide data prep tab for defining links
      hideTab(inputId = "tab_dataprep", target = "Define Links and Semi-Landmarks")
    }
    
    return(temp_LMs) # this is when the button for example data
  }) 
  
  tryObserveEvent(input$file_tps, {
    if(!is.null(input$file_tps)) { vals$tps_rx_upload_state <- 'uploaded' } 
  })
  
  tryObserveEvent(input$file_stereomorph, {
    if(!is.null(input$file_stereomorph)) { vals$tps_rx_upload_state <- 'uploaded' }
  })
  
  tryObserveEvent(input$shape_file_type, ignoreInit = T, priority = -10, {
    if(!is.null(vals$tps_rx_upload_state)) { 
      if(vals$tps_rx_upload_state == "reset"){
        vals$file_stereomorph_shape_curven <- 0 # used for conditional paneling of curve_n options
      }
    }
  })
  
  tryObserveEvent(lms_rx(), ignoreInit = F, {
    req(vals$tps_rx_upload_state)
    if(vals$tps_rx_upload_state == "reset") { 
      vals$links_df <- NULL
      vals$lms_rx <- NULL } else { 
        vals$lms_rx <- lms_rx()
      }
  })
  
  tryObserveEvent(input$go_run_stereomorph_curves, ignoreInit = F, {
    vals$go_run_stereomorph_curves <- input$go_run_stereomorph_curves
    vals$stereomorph_curve1_n <- input$stereomorph_curve1_n
    vals$stereomorph_curve2_n <- input$stereomorph_curve2_n
    vals$stereomorph_curve3_n <- input$stereomorph_curve3_n
    vals$stereomorph_curve4_n <- input$stereomorph_curve4_n
    vals$stereomorph_curve5_n <- input$stereomorph_curve5_n
    vals$stereomorph_curve6_n <- input$stereomorph_curve6_n
    vals$links_df <- NULL 
  })
  
  phy_rx <- reactive({ # phylogeny reactive element
    temp_phy <- NULL 
    ee <- input$go_remove_outlier_specimen_reset #trigger
    if(is.null(vals$go_example_1)){ # if the example button has not been pressed
      if(!is.null(input$file_phy)) {
        inFile <- input$file_phy
        if (endsWith(inFile$name, '.nexus') | endsWith(inFile$name, '.nex')){
          temp_phy <- read.nexus(inFile$datapath)
        } else if (endsWith(inFile$name, '.tre')){
          temp_phy <- read.tree(inFile$datapath) # read in the tree
        }
      } else { if(!is.null(isolate(vals$phy_rx))) {return(isolate(vals$phy_rx))} }
    }
    if(!is.null(vals$go_example_1)){ 
      vals$phy_rx_upload_state <- "example"
      temp_phy <- plethspecies$phy 
    }
    if(!is.null(temp_phy)) {
      showTab(inputId = "tab_shapepatterns", "Phylogenetic Signal")
      return(temp_phy)
    } else { 
      hideTab(inputId = "tab_shapepatterns", "Phylogenetic Signal")
      return(NULL)    }
  }) 
  
  tryObserveEvent(input$file_phy, {
    vals$phy_rx_upload_state <- 'uploaded'
  })
  
  tryObserveEvent(phy_rx(), ignoreInit = F, {
    req(vals$phy_rx_upload_state)
    if(vals$phy_rx_upload_state == "reset") { 
      vals$phy_rx <- NULL
    } else {
      vals$phy_rx <- phy_rx() 
    }
  })
  
  trait_rx <- reactive({
    trait_table <- NULL
    dd <- input$go_remove_outlier_specimen_reset #trigger, keep
    trait_column <- as.numeric(input$trait_column)
    
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
      }
    }
    if(!is.null(vals$go_example_1)){
      vals$trait_rx_upload_state <- 'example'
      trait_table <- as.matrix(example_mat[,c(1,trait_column)])

    }
    if(!is.null(trait_table)) {
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
  
  prune_both_ways <- reactive({list(vals$outlier_removed_names, vals$go_pruning, vals$unprune)})
  
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
  
  
  gpa_coords_rx <- metaReactive2({ # this reactive helps with whether or not the data should be aligned 
    req(vals$lms_rx)
    nothing <- input$symm_trigger # this trigger is required rather than go_symmetry_useoutput because otherwise they run in the wrong order
    nothing <- input$asymm_trigger
    metaExpr({
      vals$estimates_cancelled <- NULL
      if(..(input$raw_lms_already_aligned) == F) {
        if(!is.null(vals$symm_lms)) { # if we've pressed the button for using the symmetric or asymmetric component of shape as coords
          vals$csize <- NULL
          vals$trait_rx <- NULL
          vals$phy_rx <- NULL 
          vals$Data_gpa <- NULL # this is only made in this gpa_coords_rx to use in bilat.symmetry analyses, so this reset is probably for the best
          lms_final <- vals$symm_lms
        } else {
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
              }else {
                vals$estimates_cancelled <- T
              } 
            }else { 
              vals$estimates_cancelled <- T
            }
          }else {
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
            updateRadioButtons(session, inputId = "neg_lms", "Negative LMs Are:", # option to indicate whether negative LMs should be taken as missing data or as real negative values
                               choices = c("True LMs" = FALSE, "Missing Data" = TRUE), 
                               selected = FALSE) # doesnt need a bookmarking workaround
          }else {
            
            if(!is.null(vals$curves)) {
              if(nrow(vals$curves) > 0){
                curves <- as.matrix(vals$curves)
                curve.mat <- matrix(as.integer(curves), ncol = 3, byrow = F)
                colnames(curve.mat) <- c("before", "slide", "after")
              }else { curve.mat <- NULL}
            }else { curve.mat <- NULL } 
            vals$Data_gpa <- gpagen(lms_estimated, curves = curve.mat, 
                                    ProcD = ..(input$ProcD), Proj = ..(input$Proj), 
                                    print.progress = F)
            lms_final <- vals$Data_gpa$coords
            vals$csize <- vals$Data_gpa$Csize 
            names(vals$csize) <- dimnames(lms_final)[[3]]
          } 
        }
      }else {
        if(isolate(input$go_symmetry_useoutput)>0 | isolate(input$go_asymmetry_useoutput)>0) { # if we've pressed the button for using the symmetric or asymmetric component of shape as coords
          lms_final <- vals$symm_lms 
        }
        lms_final <- vals$lms_rx # pass lms straight through 
        vals$csize <- NULL
      }
      if(!anyNA(lms_final)) { 
        if(!is.null(lms_final)) { # necessary for symmetry clear button
          vals$mshape_gpacoords_rx <- mshape(lms_final) # also making a mean shape item so modularity doesnt take so long
        } 
        lms_final
      }else { NULL }
    })
  }) 
  
  run_gpa_reset_listen <- reactive({ list(gpa_coords_rx(), vals$curves_final_anyNAs)})
  tryObserveEvent(run_gpa_reset_listen(), ignoreInit = T, { # making the Run GPA button reset if you change anything that might affect it
    updateActionButton(session, "go_run_gpa")
  })
  
  #### Processing data in various ways for analyses and visualization ####
  
  pca_nonphylo_rx <- reactive({
    req(gpa_coords_rx()) # dont run until gpa_coords defined
    
    temp_pca <- prcomp(two.d.array(gpa_coords_rx())) # run a prcomp for when phylogeny is not being shown on morphospace
    
    input.flip_axis <- vals$input.flip_axis
    if(length(input.flip_axis)>0){
      x_axis <- as.numeric(input$pca_x_axis)
      y_axis <- as.numeric(input$pca_y_axis)
      if(round(length(which(input.flip_axis == 1))/2) != length(which(input.flip_axis == 1))/2) { # if there are an odd number of 1s in this list
        temp_pca$x[,x_axis] <- temp_pca$x[,x_axis]*(-1)
      }
      if(round(length(which(input.flip_axis == 2))/2) != length(which(input.flip_axis == 2))/2) {
        temp_pca$x[,y_axis] <- temp_pca$x[,y_axis]*(-1)
      }
    }
    
    temp_pca
  })
  
  pca_rx <- metaReactive2({
    req(gpa_coords_rx()) # dont run until gpa_coords defined
    metaExpr({
      if(..(datasets_dont_match()) == FALSE) {
        temp_pca <- geomorph:::gm.prcomp(..(gpa_coords_rx()), vals$phy_rx, GLS = ..(input$gls_center_tf),
                                         align.to.phy = ..(input$align_to_phy_tf), 
                                         transform = as.logical(..(input$transform_resid_tf))) # gm.prcomp for phylomorphospace
        
        input.flip_axis <- vals$input.flip_axis
        if(length(input.flip_axis)>0){
          x_axis <- as.numeric(input$pca_x_axis)
          y_axis <- as.numeric(input$pca_y_axis)
          if(round(length(which(input.flip_axis == 1))/2) != length(which(input.flip_axis == 1))/2) { # if there are an odd number of 1s in input.flip_axis
            temp_pca$x[,x_axis] <- temp_pca$x[,x_axis]*(-1) 
          }
          if(round(length(which(input.flip_axis == 2))/2) != length(which(input.flip_axis == 2))/2) { # if there are an odd number of 2s in input.flip_axis
            temp_pca$x[,y_axis] <- temp_pca$x[,y_axis]*(-1)
          }
        }
        
        temp_pca
        
      }
    })
  })
  
  x_lab_rx <- reactive({ paste("PC", input$pca_x_axis, sep="") }) # making a reactive for which pc axes to display
  
  y_lab_rx <- reactive({ paste("PC", input$pca_y_axis, sep="") }) # making a reactive for which pc axes to display
  
  # Trait Treatment for Input Settings
  
  tryObserve(priority = -100, {
    req(input$trait_column)
    
    xx <- c(input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment)[1:length(input$trait_column)]
    
    if(length(which(xx=="disc"))>0) {vals$one_disc_traits <- "one_trait"} else {vals$one_disc_traits <- NULL}
    if(length(which(xx=="disc"))>1) {vals$two_disc_traits <- "two_traits"} else {vals$two_disc_traits <- NULL}
    if(length(which(xx=="disc"))>2) {vals$three_disc_traits <- "three_traits"} else {vals$three_disc_traits <- NULL}
    if(length(input$trait_column) == 1) {vals$two_disc_traits <- vals$three_disc_traits <- NULL}
  })
  
  
  # Morphospace Tip Color
  
  tryObserveEvent(input$tip_col_category, ignoreInit = T, { # when the point tip col category is changed, uncheck "other" box
    reset("tip_col_other_tf")
  })
  
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
    x <- gridPar(pt.bg = input$warp_pt_bg, pt.size = input$warp_pt_size, 
                 link.col = input$warp_link_col, link.lwd = input$warp_link_lwd, link.lty = as.numeric(input$warp_link_lty), 
                 #out.col = input$warp_out_col, out.cex = as.numeric(input$warp_out_cex),  # Needs shape outline, functionality to come
                 tar.pt.bg = input$warp_tar_pt_bg,  tar.pt.size = input$warp_tar_pt_size, 
                 tar.link.col = input$warp_tar_link_col, tar.link.lwd = input$warp_tar_link_lwd, tar.link.lty = as.numeric(input$warp_tar_link_lty), 
                 #tar.out.col = input$warp_tar_out_col, tar.out.cex = as.numeric(input$warp_tar_out_cex), # Needs shape outline, functionality to come 
                 n.col.cell = input$warp_n_col_cell, 
                 grid.col = input$warp_grid_col, grid.lwd = input$warp_grid_lwd, grid.lty = as.numeric(input$warp_grid_lty), 
                 #txt.adj = as.numeric(input$warp_txt_adj), # non-functional
                 txt.pos = as.numeric(input$warp_txt_pos), txt.cex = input$warp_txt_cex, txt.col = input$warp_txt_col)
    return(x)
  })
  
  output$example_tps_selected <- reactive({return(!is.null(vals$go_example_1))}) # this output allows for appropriate options to be available when using example pleth data
  outputOptions(output, 'example_tps_selected', suspendWhenHidden=FALSE)
  
  output$stereomorph_curvetotal_n <- reactive({ return(vals$file_stereomorph_shape_curven) })
  outputOptions(output, 'stereomorph_curvetotal_n', suspendWhenHidden=FALSE)
  
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
  
  output$outlier_removed <- reactive({return(!is.null(vals$outlier_removed_names))})
  outputOptions(output, "outlier_removed", suspendWhenHidden = F)
  
  output$show_no_replicates_text <- reactive({return(!is.null(vals$show_no_replicates_text))})
  outputOptions(output, "show_no_replicates_text", suspendWhenHidden = F)
  
  output$show_symmetry_file <- reactive({return(!is.null(vals$go_symmetry_file))})
  outputOptions(output, "show_symmetry_file", suspendWhenHidden = F)
  
  output$show_symmetry_landpairs_file <- reactive({return(!is.null(vals$go_symmetry_landpairs_file))})
  outputOptions(output, "show_symmetry_landpairs_file", suspendWhenHidden = F)
  
  output$show_symmetry_definitions <- reactive({
    if(!is.null(vals$go_show_specimen_assignments)) {
      return(TRUE)} else { return(FALSE) }
  })
  outputOptions(output, "show_symmetry_definitions", suspendWhenHidden = F)
  
  output$show_symmetry_landpairs_definitions <- reactive({
    if(!is.null(vals$go_show_landmark_assignments)) {
      if(input$symmetry_obj_sym) {
        return(TRUE)
      }else { return(FALSE)}
    } else {return(FALSE)}
  })
  outputOptions(output, "show_symmetry_landpairs_definitions", suspendWhenHidden = F)
  
  output$symmetry_replicate <- reactive({return(input$symmetry_replicate != 0)})
  outputOptions(output, 'symmetry_replicate', suspendWhenHidden=FALSE)
  
  output$symmetry_side <- reactive({return(input$symmetry_side != 0)})
  outputOptions(output, 'symmetry_side', suspendWhenHidden=FALSE)
  
  output$symmetry_initiated <- reactive({return(!is.null(vals$run_symmetry_go))})
  outputOptions(output, "symmetry_initiated", suspendWhenHidden = F)
  
  output$symmetry_pushed <- reactive({return((input$go_symmetry_useoutput == 0) & (input$go_asymmetry_useoutput == 0))})
  outputOptions(output, "symmetry_pushed", suspendWhenHidden = F)
  
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
  
  output$links_dbclick_initiated <- reactive({return(!is.null(vals$links_df))}) # for conditional paneling when plotAllSpecimens is double clicked to show the link reset button
  outputOptions(output, 'links_dbclick_initiated', suspendWhenHidden=FALSE)
  
  output$semis_initiated <- reactive({
    if(!is.null(vals$curves_final)) {
      out <- nrow(vals$curves_final) > 0
    } else { out <- F }
    return(out)
  }) # for conditional paneling to display semis reset button 
  outputOptions(output, 'semis_initiated', suspendWhenHidden=FALSE)
  
  datasets_dont_match <- metaReactive({
    if(is.null(vals$lms_rx)) {FALSE}else{ # making the button stay hidden if we've used the example dataset and then hit clear
      if(!is.null(vals$phy_rx)){ # if there is a phylogeny
        if(!is.null(vals$trait_rx)) { # if there is also a trait file
          !(identical(vals$trait_rx[,1], dimnames(vals$lms_rx)[[3]]) & 
              identical(dimnames(vals$lms_rx)[[3]], vals$phy_rx$tip.label)) # return T/F of whether the data match
        }else{ # if only the phy file is uploaded
          !identical(dimnames(vals$lms_rx)[[3]], vals$phy_rx$tip.label) # return T/F of whether the data match
        }
      }else{
        if(!is.null(vals$trait_rx)) { # if only the trait file is uploaded
          !identical(vals$trait_rx[,1], dimnames(vals$lms_rx)[[3]]) # return T/F of whether the data match
        }else{FALSE} # if neither file is uploaded, return a FALSE ??? unsure
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
  
  output$anova_initiated <- reactive({
    return(input$go_run_anova) 
  })
  outputOptions(output, 'anova_initiated', suspendWhenHidden=FALSE)
  
  output$allometry_color_nlev <- reactive({ 
    this_color <- NULL
    this_color <- (2:4)[c(input$allometry_color == "by_trait_1" | input$show_allometry_convex_hull_1, 
                          input$allometry_color == "by_trait_2" | input$show_allometry_convex_hull_2, 
                          input$allometry_color == "by_trait_3" | input$show_allometry_convex_hull_3)]
    vals$allometry_color_lev_names <- unique(as.vector(vals$trait_rx[,this_color]))
    out <- nlevels(as.factor(vals$trait_rx[,this_color]))
    return(out)
  })
  outputOptions(output, 'allometry_color_nlev', suspendWhenHidden=FALSE)
  
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
  
  output$traj_initiated <- reactive({ return(input$go_trajectory_run > 0)})
  outputOptions(output, 'traj_initiated', suspendWhenHidden=FALSE)
  
  output$traj_trait_selected <- reactive({ return(input$trajectory_trait != "none")})
  outputOptions(output, 'traj_trait_selected', suspendWhenHidden=FALSE)
  
  output$traj_trait_nlevels <- reactive({ return(length(vals$traj_trait_levels)) })
  outputOptions(output, 'traj_trait_nlevels', suspendWhenHidden=FALSE)
  
  #### tryObserveEvents for Example Data, Prune Datasets, and Clear Inputs Buttons #### 
  
  tryObserveEvent(eventExpr = input$go_example, ignoreInit = T, {
    if(input$go_example > 0 ) {
      vals$go_example_1 <- input$go_example   
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
  
  tryObserveEvent(eventExpr = input$go_run_gpa, ignoreInit = T, {
    if(input$go_run_gpa>0) { vals$go_run_gpa <- c(vals$go_run_gpa, "go") }
  })
  
  tryObserveEvent(eventExpr = vals$go_example_1, priority = 10000, ignoreInit = T, { # example dataset button
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
    if(!is.null(vals)){
      for(i in names(vals)) vals[[i]] <- NULL
    }
    vals$tip_pch <- 19
    vals$tip_col <- "black"
    
    updateRadioButtons(session, "shape_file_type" , selected = "TPSorNTS")
    reset("stereomorph_curve1_n")
    reset("stereomorph_curve2_n")
    reset("stereomorph_curve3_n")
    reset("stereomorph_curve4_n")
    reset("stereomorph_curve5_n")
    reset("stereomorph_curve6_n")
    reset("file_stereomorph")
    reset("trait_column")
    reset("file_tps")
    reset("file_phy")
    reset("file_trait")
    reset("go_run_gpa")
    reset("spec_id")
    reset("neg_lms")
    reset("go_example")
    
    reset("semilms_manual_input")
    
    reset("flip_x_axis")
    reset("flip_y_axis")
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
    reset("vis_outliers_color_brackets")
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
    reset("symmetry_obj_sym")
    reset("symmetry_land_pairs_file_upload")
    reset("symmetry_land_pairs_file_input")
    reset("symmetry_side")
    reset("symmetry_file_upload")
    reset("symmetry_perm")
    
    reset("symm_trigger")
    reset("asymm_trigger")
    reset("go_symmetry_file")
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
    hideTab(inputId = "tab_linearmodels", "Trajectory Analysis")
  })
  
  tryObserveEvent(eventExpr = vals$go_pruning, ignoreInit = F, { 
    req(gpa_coords_rx()) # this only runs if shape data exist
    if(!is.null(vals$go_pruning)) {
      vals$prune_datasets <- "pruned" # initiating lms_rx, phy_rx, and trait_rx to be rerun
    } else {
      vals$prune_datasets <- NULL
    }
  })
  
  tryObserveEvent(eventExpr = input$warp_reset, ignoreInit = T,{ # reset button resets a lot of things including the placement 
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
  
  tryObserveEvent(eventExpr = input$symmetry_reset, ignoreInit = T, {
    if(input$symmetry_reset > 0) {
      vals$symmetry_land_pairs <- NULL
      vals$symm_lms <- NULL
      vals$bilat_symmetry <- NULL
      vals$go_symmetry_file <-  NULL
      vals$go_symmetry_landpairs_file <- NULL
      vals$go_show_specimen_assignments <- NULL
      vals$go_show_landmark_assignments <- NULL
      vals$show_no_replicates_text <- NULL
      vals$run_symmetry_go <- NULL
      
      reset("go_symmetry_useoutput")
      reset("go_asymmetry_useoutput")
      reset("symm_trigger")
      reset("asymm_trigger")
      reset("symmetry_file_upload")
      reset("symmetry_land_pairs_file_upload")
      reset("run_symmetry_go")
      reset("go_symmetry_no_replicates")
      reset("symmetry_obj_sym")
      reset("symmetry_land_pairs_file_input")
      reset("symmetry_side")
      reset("symmetry_perm")
      reset("symmetry_reset")
      
      updateMatrixInput(session, "symmetry_definitions", 
                        value = symmetry_manual_matrix)
      updateMatrixInput(session, "symmetry_landpairs_definitions", 
                        value = symmetry_landpairs_manual_matrix)
    }
  })
  
  #### Dealing with Plot Clicks  #####
  tryObserveEvent(eventExpr = input$go_semilms_apply, ignoreInit = T, {
    if(vals$curves_final_anyNAs) {
      alert_vals$gpa_not_ready <- T
      vals$go_semilms_apply <- 0
    } else {
      alert_vals$gpa_not_ready <- F
      vals$go_semilms_apply <- input$go_semilms_apply
      vals$curves <- vals$curves_final # triggers a rerun of gpa_coords_rx()
    }
  })
  
  
  #
  
  tryObserveEvent(eventExpr = input$morphospace_specimen_click, ignoreInit = T, {
    input.morphospace_specimen_click <- input$morphospace_specimen_click
    if(!is.null(input.morphospace_specimen_click)) {
      rotation_x_df <- as.data.frame(pca_nonphylo_rx()$x)
      vals$clicked_point <- nearPoints(rotation_x_df, input$morphospace_specimen_click, 
                                       xvar = x_lab_rx(), yvar = y_lab_rx(),
                                       threshold = 20, maxpoints = 1)  # this prints the x and y values of the nearest point (within 20 pixels) to the click on the morphospace
    }   
  })
  
  morphospace_clicked_listen <- reactive({list(vals$clicked_point,
                                               input$warp_comparison_start, 
                                               input$warp_comparison_end )})
  
  tryObserveEvent(eventExpr = morphospace_clicked_listen(), ignoreInit = T, { 
    if(length(unlist(vals$clicked_point))>1) {
      vals$morpho_clicked <- 1 # initiate the vals for conditional paneling
      if(input$warp_comparison_end == "selected_obs") {  vals$specimen_row_targ <- vals$clicked_point }
      if(input$warp_comparison_start == "selected_obs") {  vals$specimen_row_ref <- vals$clicked_point }
      vals$warp_initiated <- "Yes"
    } 
  })
  
  tryObserveEvent(eventExpr = input$morphospace_projection_dbclick, ignoreInit = T, {
    vals$morpho_dbclicked <- 1 # initiate the vals for conditional paneling
    vals$projection_row <- c(input$morphospace_projection_dbclick$x, input$morphospace_projection_dbclick$y) # this prints the exact x and y values where double clicked on the morphospace
    vals$warp_initiated <- "Yes"
  })
  
  tryObserveEvent(vals$warp_initiated, ignoreInit = F, {
    if(!is.null(vals$warp_initiated)) {
      delay(300, session$sendCustomMessage(type = "scrollCallback_warp", 1))     
    }
  })
  
  warp_ref_listen <- reactive({list(input$warp_comparison_start, input$warp_specific_ref_specimen, vals$input.flip_axis)})
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
  ref_rx <- metaReactive2({
    req(gpa_coords_rx())
    metaExpr({
      if(..(input$warp_var_displayed) == "full_morphospace"){ # comparison is whole morphospace
        if(..(input$warp_comparison_start) == "mean") { 
          out <- mshape(..(gpa_coords_rx()))
        } # if the warp comparison ref is the full morphospace mean
        if(..(input$warp_comparison_start) == "selected_obs" | ..(input$warp_comparison_start) == "selected_obs_byname" ) {
          if(!is.null(vals$specimen_row_ref)) {
            temp_pc <- ..(pca_rx())$x
            which_row <- match(row.names(vals$specimen_row_ref), row.names(temp_pc)) # which specimen was clicked matches the pca
            temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, ref = temp_pc[which_row,]) 
            
            out <- temp_pc_pred$ref
          } 
        }
        if(..(input$warp_comparison_start) == "selected_proj") {
          temp_pc <- ..(pca_rx())$x[,c(as.numeric(..(input$pca_x_axis)), as.numeric(..(input$pca_y_axis)))] # this is limited to the 2 pc axes visualized because thats all thats possible with the theoretical part of morphospace
          temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, ref = vals$projection_row) # which x and y values were double clicked as projection
          
          out <- temp_pc_pred$ref
        }
      }
      if(..(input$warp_var_displayed) == "selected_axes"){ # comparison is just across specific axes
        if(..(input$warp_comparison_start) == "mean") {
          temp_pc <- ..(pca_rx())$x[,as.numeric(..(input$warp_axes_selected))]
          if(is.null(dim(temp_pc))) { 
            temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x = temp_pc, Intercept = FALSE, ref = mean(temp_pc))
          }else{ temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x = temp_pc, Intercept = FALSE, ref = colMeans(temp_pc)) }
        }
        if(..(input$warp_comparison_start) == "selected_obs"| ..(input$warp_comparison_start) == "selected_obs_byname" ) {
          if(!is.null(vals$specimen_row_ref)) {
            temp_pc <- ..(pca_rx())$x[,as.numeric(..(input$warp_axes_selected))] 
            if(is.null(dim(temp_pc))) { # this if else is about whether the selected axes form a vector or a matrix
              which_row <- match(row.names(vals$specimen_row_ref), names(temp_pc))
              temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, ref = temp_pc[which_row])
            }else{
              which_row <- match(row.names(vals$specimen_row_ref), row.names(temp_pc))
              temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, ref = temp_pc[which_row,])
            }
          }
        }
        if(..(input$warp_comparison_start) == "selected_proj") {
          included_axes <- intersect(c(as.numeric(..(input$pca_x_axis)), as.numeric(..(input$pca_y_axis))), 
                                     as.numeric(..(input$warp_axes_selected))) # this identifies the shared axes between what pc axes are visible from which the projection was selected and which axes were selected for what variation to show
          temp_pc <- ..(pca_rx())$x[,included_axes] 
          temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, 
                                          ref = vals$projection_row[na.omit(match(c(as.numeric(..(input$pca_x_axis)), 
                                                                                    as.numeric(..(input$pca_y_axis))), included_axes))]) # again selecting the axes that are both visualized and selected
        }
        out <- temp_pc_pred$ref
      }
      out
    })
  })
  
  targ_rx <- metaReactive({
    if(..(input$warp_var_displayed) == "full_morphospace"){
      if(!is.null(..(input$warp_comparison_end))){
        if(..(input$warp_comparison_end) == "mean") { 
          preds <- shape.predictor(..(gpa_coords_rx()), x = ..(pca_rx())$x, meanshape = colMeans(..(pca_rx())$x))
          out <- preds$meanshape
          names(out)<- "Mean Shape"
        }
        if(..(input$warp_comparison_end) == "selected_obs"| ..(input$warp_comparison_end) == "selected_obs_byname") {
          if(!is.null(vals$specimen_row_targ)){
            temp_pc <- ..(pca_rx())$x
            which_row <- match(row.names(vals$specimen_row_targ), row.names(temp_pc))
            temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, tar = temp_pc[which_row,])
            out <- temp_pc_pred$tar
            names(out)<- row.names(vals$specimen_row_targ)
          }
        }
        if(..(input$warp_comparison_end) == "selected_proj") {
          temp_pc <- ..(pca_rx())$x[,c(as.numeric(..(input$pca_x_axis)), as.numeric(..(input$pca_y_axis)))] 
          temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, tar = vals$projection_row)
          out <- temp_pc_pred$tar
          names(out)<- paste("(", round(vals$projection_row[1], 4), ", ", round(vals$projection_row[2], 4), ")", sep = "")
        }
      }
    }
    if(..(input$warp_var_displayed) == "selected_axes"){
      if(..(input$warp_comparison_end) == "mean") {
        temp_pc <- ..(pca_rx())$x[,as.numeric(..(input$warp_axes_selected))]
        if(is.null(dim(temp_pc))) { 
          temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, tar = mean(temp_pc)) # dimensionality issue as described in ref reactive above
        }else{ temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, tar = colMeans(temp_pc)) }
      }
      if(..(input$warp_comparison_end) == "selected_obs"| ..(input$warp_comparison_end) == "selected_obs_byname") {
        if(!is.null(vals$specimen_row_targ)) {
          temp_pc <- ..(pca_rx())$x[,as.numeric(..(input$warp_axes_selected))] 
          if(is.null(dim(temp_pc))) { # same as above
            which_row <- match(row.names(vals$specimen_row_targ), names(temp_pc)) 
            temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, tar = temp_pc[which_row])
            out <- temp_pc_pred$tar
            names(out) <- row.names(vals$specimen_row_targ)
          }else{
            which_row <- match(row.names(vals$specimen_row_targ), row.names(temp_pc))
            temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, tar = temp_pc[which_row,])
            out <- temp_pc_pred$tar
            names(out) <- row.names(vals$specimen_row_targ)
          }
        } 
      }
      if(..(input$warp_comparison_end) == "selected_proj") {
        included_axes <- intersect(c(as.numeric(..(input$pca_x_axis)), 
                                     as.numeric(..(input$pca_y_axis))), 
                                   as.numeric(..(input$warp_axes_selected)))
        temp_pc <- ..(pca_rx())$x[,included_axes] 
        temp_pc_pred <- shape.predictor(..(gpa_coords_rx()), x= temp_pc, Intercept = FALSE, 
                                        tar = vals$projection_row[na.omit(match(c(as.numeric(..(input$pca_x_axis)), 
                                                                                  as.numeric(..(input$pca_y_axis))), included_axes))])
        out <- temp_pc_pred$tar
        
        names(out) <- paste("(", round(vals$projection_row[1], 4), ", ", round(vals$projection_row[2], 4), ")", sep = "")
      }
      if(is.null(out)) {out <- "none yet selected"}
    }
    out
  })
  
  tryObserveEvent(input$flip_lms_ho, ignoreInit = F, {
    req(vals$lms_rx)
    req(input$flip_lms_ho)
    if(input$flip_lms_ho > 0){
      if(input$flip_lms_ho/2 != round(input$flip_lms_ho/2)){ # all these extra if statements are necessary for bookmarking purposes (not confirmed)
        vals$lms_rx[,1,] <- vals$lms_rx[,1,]*-1
      }
    }
  })
  
  tryObserveEvent(input$flip_lms_vert, ignoreInit = F, {
    req(vals$lms_rx)
    vals$lms_rx[,2,] <- vals$lms_rx[,2,]*-1
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
  
  tryObserveEvent(input$semilms_reset, ignoreInit = T, {
    session$resetBrush("semis_selected")
    vals$semis_brushed <- NULL
    reset("semilms_upload_file")
    vals$curves_final <- NULL
    vals$curves <- NULL
    reset("semilms_manual_input")
    vals$reset_go_next <- "go"
  })
  
  tryObserveEvent(vals$reset_go_next, ignoreInit = T, {
    if(!is.null(vals$reset_go_next)){
      
      vals$curves_final <- NULL
      vals$curves <- NULL
      
      vals$curves_final_anyNAs <- FALSE
      
      if(!is.null(vals$shapesGM) & !is.null(vals$go_run_stereomorph_curves)) {
        newmat1 <- vals$shapesGM$curves
        colnames(newmat1) <- c("Before", "Slide", "After")
        vals$curves_final <- newmat1
      } else {
        newmat1 <- matrix(NA, ncol = 3)
        colnames(newmat1) <- c("Before", "Slide", "After")
      }
      
      updateMatrixInput(session, "semilms_manual_input", value = newmat1)   
      
    }
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
  
  tryObserveEvent(input$semis_stereomorph, ignoreInit = T, {
    if(input$semis_stereomorph > 0) { # need to reformat this information into a links-style df (2 columns)
      req(vals$shapesGM$curves)
      stereomorph_curves <- vals$shapesGM$curves
      stereomorph_links_df <- NULL
      for(i in 1:nrow(stereomorph_curves)) {
        stereomorph_links_df <- rbind(stereomorph_links_df, stereomorph_curves[i,1:2])
        stereomorph_links_df <- rbind(stereomorph_links_df, stereomorph_curves[i,2:3])
      }
      vals$links_df <- unique(rbind(vals$links_df, stereomorph_links_df))
    }
  })
  
  tryObserveEvent(input$navbar, ignoreInit = T, {
    if(input$navbar != "Data Input"){
      if(input$shape_file_type == "StereoMorph") {
        if(is.null(vals$go_run_stereomorph_curves) & vals$file_stereomorph_shape_curven > 0) {
          shinyalert(
            title = "STOP",
            text = "The number of curve points has not yet been applied to the data input. 
          You must press the 'Apply' button in order to initate the semi-landmark sampling 
          scheme.",
            type = "error",
            html = T,
            inputId = "stereomorph_curve_not_applied"
          )
        }
        if(!is.null(vals$go_run_stereomorph_curves) & vals$file_stereomorph_shape_curven > 0) {
          error1 <- vals$stereomorph_curve1_n != input$stereomorph_curve1_n
          error2 <- vals$stereomorph_curve2_n != input$stereomorph_curve2_n
          error3 <- vals$stereomorph_curve3_n != input$stereomorph_curve3_n
          error4 <- vals$stereomorph_curve4_n != input$stereomorph_curve4_n
          error5 <- vals$stereomorph_curve5_n != input$stereomorph_curve5_n
          error6 <- vals$stereomorph_curve6_n != input$stereomorph_curve6_n
          if(any(c(error1, error2, error3, error4, error5, error6))) {
            shinyalert(
              title = "STOP",
              text = "The number of curve points has changed since you last applied the semi-landmark
            sampling scheme. You must press the 'Apply' button again after having selected
            the correct number of curve points to use.",
              type = "error",
              html = T,
              inputId = "stereomorph_curve_not_applied_recently"
            )
          }
          
        }
      }
    }
  })
  
  stereomorph_curve_error_listen <- reactive({list(input$stereomorph_curve_not_applied_recently, input$stereomorph_curve_not_applied)})
  tryObserveEvent(stereomorph_curve_error_listen() , ignoreInit = T, {
    updateNavbarPage(session, "navbar", selected = "Data Input") # doesnt need a bookmarking workaround
  })
  
  tryObserveEvent(vals$file_stereomorph_shape_curven, ignoreInit = T, {
    if(vals$file_stereomorph_shape_curven > 6) {
      shinyalert(
        title = "Too Many Curves",
        text = "There are too many curves in this dataset to allow for full customization within gmShiny. 
            Curves 7 and onward will automatically inherit the same number of Curve Points assigned to curve 6. 
            <br><br> For full customization of curve points for curves 7+, we recommend manipulating your data
            directly in R or RStudio.",
        type = "warning",
        html = T,
        inputId = "stereomorph_too_many_curves"
      )
    }
  })
  
  tryObserveEvent(vals$shapesGM, ignoreInit = F, {
    if(!is.null(vals$shapesGM) & !is.null(vals$go_run_stereomorph_curves)) {
      curves_mat <- vals$shapesGM$curves
      if(length(dim(curves_mat)) == 2) {
        colnames(curves_mat) <- c("Before", "Slide", "After")
        updateMatrixInput(session, "semilms_manual_input", value = curves_mat) 
      }
    }
  })
  
  brushes_links_listen <- reactive({list(vals$semis_brushed, vals$links_df)})
  tryObserveEvent(brushes_links_listen(), ignoreInit = F, {
    
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
          
        } else {
          befores_last <- c(befores_last, NA)}
        if(any(vals$links_df[,1] %in% vals$semis_brushed[i])){
          afters_i <- vals$links_df[which(vals$links_df[,1] == vals$semis_brushed[i]),2]
          afters_last <- c(afters_last, afters_i[length(afters_i)])
          
        } else {afters_last <- c(afters_last, NA)}
      }
      if(ncol(curves_rough) == 2) {
        curves_rough <-  data.frame("Before" = rep(NA, length(befores_last)), 
                                    "Slide" = rep(NA, length(befores_last)), 
                                    "After" = rep(NA, length(befores_last)))
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
    if(nrow(temp_mat)>0) {
      for(i in 1:nrow(temp_mat)) { temp_mat[i,] <- as.numeric(temp_mat[i,])}
    }
    
    if(!is.null(vals$curves_final)) { temp_vals_mat <- matrix(unlist(vals$curves_final), ncol = 3) } else { temp_vals_mat <- NULL }
    
    if(!identical(temp_mat, temp_vals_mat)) { # if there are already manual entries, add the curves to the bottom. labeled at beginning of script
      curves_rough <- rbind(temp_vals_mat, temp_mat)
      delete <- which(curves_rough[,2] == "")
      if(length(delete) > 0) {curves_rough <- curves_rough[-delete,]}
      
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
    if(nrow(vals$curves_final) > 0) {
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
    }
    
  })
  
  tryObserveEvent(input$outlier_selected, ignoreInit = T, { 
    
    if(!is.null(input$outlier_selected)) {
      
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
      
      vals$outlier_row <- nearPoints(outlier_df, input$outlier_selected, xvar = "x", yvar = "y",
                                     threshold = 15, maxpoints = 1) 
      if(nrow(vals$outlier_row)>0) { # only scroll if a point is successfully selected
        delay(300, session$sendCustomMessage(type = "scrollCallback_outlier", 1))
      }
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
    
    vals$outlier_row <- NULL
    if(!input$outlier_group_tf) {
      reset("outlier_group")
      reset("outlier_group_level_plotted")
    }
  })
  
  tryObserveEvent(input$outlier_group, priority = 100, ignoreInit = T, {
    vals$outlier_row <- NULL
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
  
  tryObserveEvent(input$raw_lms_already_aligned, ignoreInit = T, {
    if(input$raw_lms_already_aligned){
      hideTab(inputId = "tab_dataprep", target = "Generalized Procrustes Alignment")
      if(is.null(vals$phy_rx) & is.null(vals$trait_rx)){
        hideTab(inputId = "navbar", target = "Linear Models")
      } else { showTab(inputId = "navbar", target = "Linear Models") }
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
      if(!is.null(vals$two_disc_traits)) {
        showTab(inputId = "tab_linearmodels", "Trajectory Analysis")
      } else {
        hideTab(inputId = "tab_linearmodels", "Trajectory Analysis")
      }
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
      
      updateCheckboxGroupInput(session, "trait_column", label = NULL,
                               choiceNames = vals$col_names_temp[2:vals$trait_column_max_value], 
                               choiceValues = 2:vals$trait_column_max_value, # updating options for all columns available
                               selected = 2, inline = F)
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
      vals$outlier_removed_names <- unique(c(vals$outlier_removed_names, input$remove_outlier_specimen)) 
    }
  })
  
  tryObserve({
    if(!is.null(vals$phy_rx) | !is.null(vals$go_example_1)) { pgls_ols <- "pgls" } else { pgls_ols <- "ols" }
    updateRadioButtons(session, "pgls_ols", label = "Analysis Type:", 
                       choices = c("PGLS" = "pgls", "OLS" = "ols"), selected = pgls_ols)
    
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
        
        updateRadioButtons(session, "outlier_group_level_plotted", label = NULL, 
                           choiceNames = choice_names, choiceValues = choice_vals)
        
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
      
      selected_item <- independent_variable_named[1]
      
      updateCheckboxGroupInput(session, "independent_variables" , 
                               "Independent Variables Tested:",
                               choices = independent_variable_named, # updating options for all columns available
                               selected = selected_item, inline = F) 
      
      updateCheckboxGroupInput(session, "independent_variables_model_1" , 
                               "Independent Variables Tested in Model 1:",
                               choices = independent_variable_named, # updating options for all columns available
                               selected = selected_item, inline = F) 
      
      updateCheckboxGroupInput(session, "independent_variables_model_2" , 
                               "Independent Variables Tested in Model 2:",
                               choices = independent_variable_named, # updating options for all columns available
                               selected = selected_item, inline = F) 
      
      updateCheckboxGroupInput(session, "independent_variables_model_3" , 
                               "Independent Variables Tested in Model 3:",
                               choices = independent_variable_named, # updating options for all columns available
                               selected = selected_item, inline = F) 
      
      
      independent_variable_named_traj <- c("None" = "none1", independent_variable_named)
      selected_item <- independent_variable_named_traj[1]
      
      vals$independent_variable_named_traj <- independent_variable_named_traj
      
      updateRadioButtons(session, "trajectory_independent_var", 
                         choices = independent_variable_named_traj, # updating options for all columns available
                         selected = selected_item, inline = F) 
      
      
      choice_list_color <- c("all_1_col", choice_list)
      names(choice_list_color) <- c("All One Color", names(choice_list))
      
      updateSelectInput(session, inputId = "tip_col_category", label = "Point Color:", 
                        choices = choice_list_color, selected = "all_1_col") 
      
      if((input$trait_1_treatment == "disc" & length(colnames(vals$trait_rx)[-1]) > 0) | 
         (input$trait_2_treatment == "disc" & length(colnames(vals$trait_rx)[-1]) > 1) | 
         (input$trait_3_treatment == "disc" & length(colnames(vals$trait_rx)[-1]) > 2) | 
         is.null(vals$trait_rx)){ #making reduced choice lists of just discrete variables
        
        
        selected_disc_options <- (1:length(colnames(vals$trait_rx)[-1]))[c(input$trait_1_treatment == "disc", 
                                                                           input$trait_2_treatment == "disc", 
                                                                           input$trait_3_treatment == "disc")]
        
        
        if(is.null(vals$trait_rx)) { selected_disc_options <- NULL } 
        
        choice_list_disc <- choice_list[na.omit(selected_disc_options)]
        
        updateRadioButtons(session, "outlier_group", label = NULL, 
                           choices = choice_list_disc, selected = choice_list_disc[1])
        
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
        
        updateRadioButtons(session, "integration_group_by", label = NULL, 
                           choices = choice_list_disc_plus_none, selected = choice_list_disc_plus_none[1] )
        
        
        choice_list_disc_allcol <- c("all_1_col", choice_list_disc)
        names(choice_list_disc_allcol) <- c("All One Color", names(choice_list_disc))
        
        updateSelectInput(session, "allometry_color", label = "Color Points:", 
                          choices = choice_list_disc_allcol, selected = choice_list_disc_allcol[1])
        
        if(length(choice_list_disc)>0) {
          names(choice_list_disc) <- substring(names(choice_list_disc), 4, nchar(names(choice_list_disc)))
          
          updateRadioButtons(session, "trajectory_group", "Trajectory Group:", 
                             choices = choice_list_disc, selected = choice_list_disc[1])
          
          if(length(choice_list_disc) > 1){ 
            updateRadioButtons(session, "trajectory_trait", "Trajectory Points:", 
                               choices = choice_list_disc, selected = choice_list_disc[1])
          }
        } 
        
        choice_list_shape <- c(19, 1, 18, choice_list_disc)
        new_names <- names(choice_list_disc)
        if(length(choice_list_shape)>3) new_names <- paste("By", new_names, sep = " ") # labeling the choices
        names(choice_list_shape) <- c("Filled Circle", "Hollow Circle", "Filled Diamond", new_names)
        
        updateSelectInput(session, inputId = "tip_pch", label = "Point Shape:", 
                          choices = choice_list_shape, selected = choice_list_shape[1])
        
        
        updateCheckboxInput(session, inputId = "show_convex_hull_1", 
                            label = replacement_chull_name[c(input$trait_1_treatment == "disc", 
                                                             input$trait_2_treatment == "disc", 
                                                             input$trait_3_treatment == "disc")][1],
                            value = F) 
        
        
        updateCheckboxInput(session, "show_allometry_convex_hull_1", 
                            label = paste("Show ", replacement_chull_name[c(input$trait_1_treatment == "disc", 
                                                                            input$trait_2_treatment == "disc", 
                                                                            input$trait_3_treatment == "disc")][1], sep = ""),
                            value = F)
        
        two_hulls_dependencies <- (length(which(c(input$trait_1_treatment == "disc", 
                                                  input$trait_2_treatment == "disc", 
                                                  input$trait_3_treatment == "disc"))) > 1)
        if(two_hulls_dependencies) { 
          
          updateCheckboxInput(session, inputId = "show_convex_hull_2", label = replacement_chull_name[c(input$trait_1_treatment == "disc", 
                                                                                                        input$trait_2_treatment == "disc", 
                                                                                                        input$trait_3_treatment == "disc")][2],
                              value = F)  # display a second convex hull option
          updateCheckboxInput(session, "show_allometry_convex_hull_2", 
                              label = paste("Show ", replacement_chull_name[c(input$trait_1_treatment == "disc", 
                                                                              input$trait_2_treatment == "disc", 
                                                                              input$trait_3_treatment == "disc")][2], sep = ""),
                              value = F)
          if(input$trait_1_treatment == "disc" & 
             input$trait_2_treatment == "disc" & 
             input$trait_3_treatment == "disc") { # display an option for a third hull
            
            updateCheckboxInput(session, inputId = "show_convex_hull_3", label = replacement_chull_name[3], 
                                value = F) 
            updateCheckboxInput(session, "show_allometry_convex_hull_3", 
                                label = paste("Show ", replacement_chull_name[c(input$trait_1_treatment == "disc", 
                                                                                input$trait_2_treatment == "disc", 
                                                                                input$trait_3_treatment == "disc")][3], sep = ""),
                                value = F)
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
            
            updateRadioButtons(session, inputId = "phy_signal_input", label = "Test Signal of:",
                               choices = choice_list_phy_sig, selected = choice_list_phy_sig[1])
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
      
      choice_list <- c("by_trait_1", "by_trait_2", "by_trait_3")[1:length(all_selected_trait_names)]
      
      selected_cont_options <- (1:(ncol(vals$trait_rx)-1))[c(input$trait_1_treatment == "cont", 
                                                             input$trait_2_treatment == "cont", 
                                                             input$trait_3_treatment == "cont")]
      
      choice_list_cont <- choice_list[na.omit(selected_cont_options)]
      names(choice_list_cont) <- all_selected_trait_names[na.omit(selected_cont_options)]
      choice_list_cont <- c(choice_list_cont, "Centroid Size" = "csize")
      
      updateRadioButtons(session, inputId = "allometry_predictor", label = "Predictor Variable:",
                         choices = choice_list_cont, selected = choice_list_cont[1])
    }
  })
  
  tryObserveEvent(input$independent_variables, ignoreInit = F, {
    
    independent_variables <- input$independent_variables
    
    req(vals$trait_rx)
    col_names_temp <- names(vals$trait_names) # what are the selected column names from trait file
    for(i in 1:length(col_names_temp)) { col_names_temp[i] <- substring(col_names_temp[i], 4, nchar(col_names_temp[i])) } # cutting off "By " at the beginning
    these_traits <- (1:3)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables]
    
    traits_selected <- col_names_temp[these_traits]
    
    if("csize" %in% independent_variables){
      traits_selected <- c(traits_selected, "Centroid Size")
    }
    
    if(length(traits_selected)>1) {
      vals$trigger_trait_order_hide <- F
      updateOrderInput(session, "independent_variables_order", "Trait Order:", items = traits_selected)
    } else { vals$trigger_trait_order_hide <- T }
    
    
  })
  
  tryObserveEvent(input$independent_variables_model_1, ignoreInit = F, {
    
    independent_variables <- input$independent_variables_model_1
    
    req(vals$trait_rx)
    if(input$navbar == "Linear Models") {
      col_names_temp <- names(vals$trait_names) # what are the selected column names from trait file
      for(i in 1:length(col_names_temp)) { col_names_temp[i] <- substring(col_names_temp[i], 4, nchar(col_names_temp[i])) } # cutting off "By " at the beginning
      these_traits <- (1:3)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables]
      
      traits_selected <- col_names_temp[these_traits]
      
      if("csize" %in% independent_variables){
        traits_selected <- list(traits_selected, "Centroid Size")
      }
      
      vals$model_comparison_model_1 <- c(vals$model_comparison_model_1,"go")
      updateOrderInput(session, "independent_variables_order_model_1", "Model 1 Trait Order:", items = traits_selected)
    }
    
  })
  
  tryObserveEvent(input$independent_variables_model_2, ignoreInit = F, {
    
    independent_variables <- input$independent_variables_model_2
    
    req(vals$trait_rx)
    if(input$navbar == "Linear Models") {
      col_names_temp <- names(vals$trait_names) # what are the selected column names from trait file
      for(i in 1:length(col_names_temp)) { col_names_temp[i] <- substring(col_names_temp[i], 4, nchar(col_names_temp[i])) } # cutting off "By " at the beginning
      these_traits <- (1:3)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables]
      
      traits_selected <- col_names_temp[these_traits]
      
      if("csize" %in% independent_variables){
        traits_selected <- list(traits_selected, "Centroid Size")
      }
      
      vals$model_comparison_model_2 <- c(vals$model_comparison_model_2,"go")
      updateOrderInput(session, "independent_variables_order_model_2", "Model 2 Trait Order:", items = traits_selected)
    }
    
  })
  
  tryObserveEvent(input$independent_variables_model_3, ignoreInit = T, {
    
    independent_variables <- input$independent_variables_model_3
    
    req(vals$trait_rx)
    if(input$navbar == "Linear Models") {
      col_names_temp <- names(vals$trait_names) # what are the selected column names from trait file
      for(i in 1:length(col_names_temp)) { col_names_temp[i] <- substring(col_names_temp[i], 4, nchar(col_names_temp[i])) } # cutting off "By " at the beginning
      these_traits <- (1:3)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables]
      
      traits_selected <- col_names_temp[these_traits]
      
      if("csize" %in% independent_variables){
        traits_selected <- list(traits_selected, "Centroid Size")
      }
      
      vals$model_comparison_model_3 <- c(vals$model_comparison_model_3,"go")
      updateOrderInput(session, "independent_variables_order_model_3", "Model 3 Trait Order:", items = traits_selected)
    }
    
  })
  
  tryObserveEvent(input$trajectory_group, ignoreInit = T, {
    if(input$trajectory_group == input$trajectory_trait){
      choice_list_disc_traj <- vals$trait_names 
      names(choice_list_disc_traj) <- substring(names(choice_list_disc_traj), 4, nchar(names(choice_list_disc_traj)))
      
      selected_disc_options <- (1:length(colnames(vals$trait_rx)[-1]))[c(input$trait_1_treatment == "disc", 
                                                                         input$trait_2_treatment == "disc", 
                                                                         input$trait_3_treatment == "disc")]
      choice_list_disc_traj <- choice_list_disc_traj[na.omit(selected_disc_options)]
      selected_item <- choice_list_disc_traj[-match(input$trajectory_group, choice_list_disc_traj)][1]
      updateRadioButtons(session, "trajectory_trait", choices = choice_list_disc_traj, selected = selected_item)
    }
    
    independent_variable_named_traj <- isolate(vals$independent_variable_named_traj)
    delete_1 <- which(independent_variable_named_traj == isolate(input$trajectory_group))
    if(length(delete_1)>0) independent_variable_named_traj <- independent_variable_named_traj[-delete_1]
    delete_2 <- which(independent_variable_named_traj == isolate(input$trajectory_trait))
    if(length(delete_2)>0) independent_variable_named_traj <- independent_variable_named_traj[-delete_2]
    
    
    updateRadioButtons(session, "trajectory_independent_var" , 
                       choices = independent_variable_named_traj, # updating options for all columns available
                       selected = independent_variable_named_traj[1], inline = F) 
  })
  
  tryObserveEvent(input$trajectory_trait, ignoreInit = T, {
    if(input$trajectory_trait == input$trajectory_group){
      choice_list_disc_traj <- vals$trait_names 
      names(choice_list_disc_traj) <- substring(names(choice_list_disc_traj), 4, nchar(names(choice_list_disc_traj)))
      
      selected_disc_options <- (1:length(colnames(vals$trait_rx)[-1]))[c(input$trait_1_treatment == "disc", 
                                                                         input$trait_2_treatment == "disc", 
                                                                         input$trait_3_treatment == "disc")]
      choice_list_disc_traj <- choice_list_disc_traj[na.omit(selected_disc_options)]
      selected_item <- choice_list_disc_traj[-match(input$trajectory_trait, choice_list_disc_traj)][1]
      updateRadioButtons(session, "trajectory_group", choices = choice_list_disc_traj, selected = selected_item)
    }
    
    independent_variable_named_traj <- isolate(vals$independent_variable_named_traj)
    delete_1 <- which(independent_variable_named_traj == isolate(input$trajectory_group))
    if(length(delete_1)>0) independent_variable_named_traj <- independent_variable_named_traj[-delete_1]
    delete_2 <- which(independent_variable_named_traj == isolate(input$trajectory_trait))
    if(length(delete_2)>0) independent_variable_named_traj <- independent_variable_named_traj[-delete_2]
    
    updateRadioButtons(session, "trajectory_independent_var" , 
                       choices = independent_variable_named_traj, # updating options for all columns available
                       selected = independent_variable_named_traj[1], inline = F) 
    
    
  })
  
  # tryObserveEvent(input$go_trajectory_run, ignoreInit = T, {
  trajectory_metaO <- tryMetaObserve2({
    if(input$go_trajectory_run>0){
      req(isolate(gpa_coords_rx()))
      req(isolate(input$trajectory_trait))
      gpa_coords <- isolate(gpa_coords_rx())
      metaExpr({
        if(isolate(..(input$trajectory_trait)) != "none") {
          this_group <- (2:4)[c(isolate(..(input$trajectory_group)) == "by_trait_1", 
                                isolate(..(input$trajectory_group)) == "by_trait_2", 
                                isolate(..(input$trajectory_group)) == "by_trait_3")]
          group <- as.factor(isolate(vals$trait_rx)[,this_group])
          this_trait <- (2:4)[c(isolate(..(input$trajectory_trait)) == "by_trait_1", 
                                isolate(..(input$trajectory_trait)) == "by_trait_2", 
                                isolate(..(input$trajectory_trait)) == "by_trait_3")]
          trait <- as.factor(isolate(vals$trait_rx)[,this_trait])
          if(!is.null(isolate(vals$go_example_1))) { # if using the example dataset, you have to adjust the 3 level trait so that there are repeats in all 4 'levels'
            if(nlevels(group) > 2) { 
              group[which(group == levels(group)[2])] <- levels(group)[3] # condensing levels for purposes of example
            }
            if(nlevels(trait) > 2) { 
              trait[which(trait == levels(trait)[2])] <- levels(trait)[3] # condensing levels for purposes of example
              trait <- droplevels(trait)
            }
          }
          
          enough_inds_per_group <- min(table(paste(group,trait))) > 1
          
          if(enough_inds_per_group) {
            coords_mat <- two.d.array(gpa_coords)
            if(isolate(..(input$trajectory_independent_var)) == "none1") {
              
              rrppdf <- rrpp.data.frame(coords = coords_mat, trait = trait, group = group)
              vals$trajectory_fit <- lm.rrpp(coords ~ group*trait, data = rrppdf, 
                                             iter = isolate(..(input$traj_perm)), print.progress = F)
              vals$traj_trait_levels <- levels(trait)
              fit <- isolate(vals$trajectory_fit)
              vals$TA <- trajectory.analysis(fit, groups = group, traj.pts = trait, print.progress = FALSE)
            } else {
              this_independent_var <- (2:5)[c(isolate(..(input$trajectory_independent_var)) == "by_trait_1", 
                                              isolate(..(input$trajectory_independent_var)) == "by_trait_2", 
                                              isolate(..(input$trajectory_independent_var)) == "by_trait_3", 
                                              isolate(..(input$trajectory_independent_var)) == "csize")]
              if(this_independent_var == 5) {
                if(!is.null(isolate(vals$csize))){
                  independent_var <- isolate(vals$csize)
                }
              }
              if(this_independent_var %in% 2:4) { independent_var <- isolate(vals$trait_rx)[,this_independent_var]  }
              
              rrppdf <- rrpp.data.frame(coords = coords_mat, independent_var = independent_var, 
                                        group = group, trait = trait)
              
              vals$trajectory_fit <- fit <- lm.rrpp(coords ~ independent_var + group*trait, data = rrppdf, 
                                                    iter = isolate(..(input$traj_perm)), print.progress = F)
              vals$TA <- trajectory.analysis(fit, 
                                             groups = group, 
                                             traj.pts = trait, 
                                             print.progress = FALSE)
            }
            
          } else {
            shinyalert(
              title = "",
              text = "not enough specimens per group"
            )
            vals$trajectory_fit <- NULL
            vals$TA <- NULL
          }
          
        }
      })
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
      
      updateRadioButtons(session, "integration_group_level", "Trait Level Plotted:",
                         choices = trait_levels, selected = trait_levels[1])
    }
  })
  
  tryObserve(priority = 10, {
    req(gpa_coords_rx())
    
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
      
      selected_item <- trait_choices[1]
      
      updateRadioButtons(session, inputId = "trait_pairwise", 
                         choices = trait_choices, 
                         selected = selected_item) 
      
      updateRadioButtons(session, "disparity_groups",
                         "Morphological Disparity Groups Tested:",
                         choices = trait_choices, selected = selected_item)
      
      updateRadioButtons(session, "evol_rate_groups",
                         "Evolutionary Rate Groups Tested:",
                         choices = trait_choices, selected = selected_item)
      
    }
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
    
    selected_item <- c(input$pca_x_axis,input$pca_y_axis)
    
    updateCheckboxGroupInput(session, "warp_axes_selected", label = "Visualize Variation Across PC:",
                             choices = pc_axes_temp, selected = selected_item, inline = T)
    
    axes_options <- NULL # making sure the axes options is new with new datasets
    for(i in 1:length(pc_axes_temp)){
      axes_options <- c(axes_options, pc_axes_temp[i]) # adding and labeling all the axes options for input$pca_x_axis and input$pca_y_axis
      names(axes_options)[i] <- paste("PC", pc_axes_temp[i], sep = "") #  
    }
    
    updateSelectInput(session, "pca_x_axis", label = "X axis", choices = axes_options, selected = selected_item[1])
    updateSelectInput(session, "pca_y_axis", label = "Y axis", choices = axes_options, selected = selected_item[2])
    
  })
  
  tryObserve({ # updating the warp grid comparisons when the morphospace is doubleclicked, adding an option to compare the projected point
    if(!is.null(vals$morpho_dbclicked)) {
      
      updateRadioButtons(session, inputId="warp_comparison_start", label = "Warp Comparison Start (Reference):", 
                         choices = c("Mean Shape" = "mean",
                                     "Observed Point (Clicked)" = "selected_obs", 
                                     "Observed Point (By Name)" = "selected_obs_byname", 
                                     "Selected Projected Point (X)" = "selected_proj"),
                         selected = "mean")
      
      updateRadioButtons(session, inputId="warp_comparison_end", label = "Warp Comparison End (Target):", 
                         choices = c("Mean Shape" = "mean",
                                     "Observed Point (Clicked)" = "selected_obs", 
                                     "Observed Point (By Name)" = "selected_obs_byname", 
                                     "Selected Projected Point (X)" = "selected_proj"),
                         selected = "selected_proj")
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
      
      selected_item <- disc_or_cont
      
      updateRadioButtons(session, inputId = id_name, 
                         choices = c("Discrete" = "disc", "Continuous" = "cont"), 
                         selected = selected_item[i])
      
    }
  })
  
  #### Updating Statistical Outputs ####
  
  physig_meta0 <- tryMetaObserve2({
    vals$filler_d <- input$navbar
    req(isolate(vals$phy_rx))
    req(input$phy_signal_input)
    if(!is.null(gpa_coords_rx())) { gpa_coords <- gpa_coords_rx() }
    metaExpr({
      if(..(datasets_dont_match()) == FALSE) {
        if(..(input$phy_signal_input) == "shape") {
          if(!is.null(gpa_coords)){
            vals$PS_shape <- physignal(A=gpa_coords,phy=isolate(vals$phy_rx),iter=..(input$signal_perm))
          }
        }
        if(..(input$phy_signal_input) == "by_trait_1" | 
           ..(input$phy_signal_input) == "by_trait_2" | 
           ..(input$phy_signal_input) == "by_trait_3" | 
           ..(input$phy_signal_input) == "csize") {
          which_trait <- (2:5)[c(..(input$phy_signal_input) == "by_trait_1", 
                                 ..(input$phy_signal_input) == "by_trait_2", 
                                 ..(input$phy_signal_input == "by_trait_3"), 
                                 ..(input$phy_signal_input == "csize"))]
          if(which_trait == 5) {
            trait_vector <- isolate(vals$csize)
          } else {
            if(!is.null(isolate(vals$trait_rx))){
              trait_vector <- as.numeric(isolate(vals$trait_rx)[,which_trait])
              names(trait_vector) <- isolate(vals$trait_rx)[,1]
            }
          }
          
          if(!anyNA(trait_vector)) {
            vals$PS_shape <- physignal(A=trait_vector, phy=isolate(vals$phy_rx),iter= ..(input$signal_perm))
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
  })
  
  tryObserve({
    vals$trait_1_treatment <- input$trait_1_treatment
    vals$trait_2_treatment <- input$trait_2_treatment
    vals$trait_3_treatment <- input$trait_3_treatment
  })
  
  anova_table_metaO <- tryMetaObserve2({
    vals$trigger <- input$go_run_anova
    req(isolate(input$independent_variables)) # keeps things from crashing if all are unselected
    req(isolate(gpa_coords_rx()))
    vals$independent_variables <- isolate(input$independent_variables)    
    gpa_coords <- isolate(gpa_coords_rx())
    vals$input.pgls_ols <- isolate(input$pgls_ols)
    vals$input.anova_perm <- isolate(input$anova_perm)
    vals$input.ss_type <- isolate(input$ss_type)
    metaExpr({
      independent_variables <- isolate(vals$independent_variables)
      if(..(input$go_run_anova)>0){
        if(..(datasets_dont_match()) == FALSE) {
          if(independent_variables[1] != "init") { # required so that this doesn't try to run before input$independent_variables has been updated
            
            if(length(independent_variables) == 1) {
              if(independent_variables == "csize") { model_order <- "By Centroid Size" } else {
                model_order <- names(isolate(vals$trait_names))[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables] #   
              }
              model_order <- substring(model_order, 4, nchar(model_order))
            } else { model_order <- isolate(..(input$independent_variables_order)) }
            if(model_order[1] != "Trait 1") {
              results <- model_definition(input.independent_variables = independent_variables, 
                                          vals.trait_rx = isolate(vals$trait_rx),
                                          vals.csize = isolate(vals$csize), 
                                          vals.trait_1_treatment = isolate(vals$trait_1_treatment), 
                                          vals.trait_2_treatment = isolate(vals$trait_2_treatment),
                                          vals.trait_3_treatment = isolate(vals$trait_3_treatment), 
                                          vals.trait_names = isolate(vals$trait_names), 
                                          vals.phy_rx = isolate(vals$phy_rx),
                                          gpa_coords_rx_reactive = gpa_coords, 
                                          input.independent_variables_order = model_order,
                                          input.pgls_ols = vals$input.pgls_ols, 
                                          input.anova_perm = vals$input.anova_perm, 
                                          input.ss_type = vals$input.ss_type)#, input.interactions_included = input$interactions_included)
              
              vals$fit <- results 
              sum <- summary(results)
              if(length(independent_variables) > 1) {
                row.names(sum$table)[1:length(independent_variables)] <- isolate(..(input$independent_variables_order)) # maybe add isolate here too
              } else {
                if(is.null(isolate(vals$trait_rx))) { row.names(sum$table)[1] <- 'Centroid Size' } else {
                  colnames_var <- colnames(isolate(vals$trait_rx))[-1] #  
                  these_traits <- (1:4)[c("by_trait_1", "by_trait_2", "by_trait_3", "csize") %in% independent_variables]
                  if(these_traits < 4) row.names(sum$table)[1] <- colnames_var[these_traits] else {
                    row.names(sum$table)[1] <- 'Centroid Size'
                  }
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
              vals$anova_table <-  isolate(sum$table)
              
              if(!is.null(isolate(vals$trait_rx))) {
                trait_3 <- trait_2 <- trait_1 <- isolate(vals$trait_rx)[,2] # filling in trait_2 and 3 so that gdf does not require a bunch of dependencies
                csize <- NULL
                if(!is.null(isolate(vals$csize))) { csize <- isolate(vals$csize) }
                
                if(isolate(vals$trait_1_treatment) == "disc") {  trait_1 <- as.factor(isolate(vals$trait_rx)[,2]) }
                if(isolate(vals$trait_1_treatment) == "cont") {
                  trait_1_c <- as.character(isolate(vals$trait_rx)[,2]) # 
                  trait_1 <- as.numeric(trait_1_c)
                }
                if("by_trait_2" %in% independent_variables) {     
                  if(isolate(vals$trait_2_treatment) == "disc") {trait_2 <- as.factor(isolate(vals$trait_rx)[,3])}
                  if(isolate(vals$trait_2_treatment) == "cont") {
                    trait_2_c <- as.character(isolate(vals$trait_rx)[,3]) # 
                    trait_2 <- as.numeric(trait_2_c)
                  } 
                }
                if("by_trait_3" %in% independent_variables) {     
                  if(isolate(vals$trait_3_treatment) == "disc") {trait_3 <- as.factor(isolate(vals$trait_rx)[,4])}
                  if(isolate(vals$trait_3_treatment) == "cont") {
                    trait_3_c <- as.character(isolate(vals$trait_rx)[,4]) # 
                    trait_3 <- as.numeric(trait_3_c)
                  } 
                }    
              }
              
              if(isolate(..(input$disparity_groups)) != "no_run" & !is.null(isolate(vals$trait_rx))) {
                
                group_selected <- list(trait_1, trait_2, trait_3)[[as.numeric(isolate(..(input$disparity_groups)))]]
                morphol_sum <- morphol.disparity(results, groups = group_selected, print.progress = F, iter = isolate(..(input$anova_perm)))
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
              
              if(isolate(..(input$evol_rate_groups)) != "no_run" & !is.null(isolate(vals$trait_rx))){
                group_selected <- list(trait_1, trait_2, trait_3)[[as.numeric(isolate(..(input$evol_rate_groups)))]]
                names(group_selected) <- isolate(vals$phy_rx$tip.label)
                out <- compare.evol.rates(gpa_coords, phy = isolate(vals$phy_rx), gp = group_selected, print.progress = F, iter = isolate(..(input$anova_perm)))
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
              if(isolate(..(input$trait_pairwise)) != "no_run" & !is.null(isolate(vals$trait_rx))) { 
                columns <- ncol(isolate(vals$trait_rx)) - 1
                treatments <- c(isolate(vals$trait_1_treatment) == "disc", 
                                isolate(vals$trait_2_treatment) == "disc", 
                                isolate(vals$trait_3_treatment) == "disc")[1:columns] 
                pair_trait_disc <- treatments[as.numeric(isolate(..(input$trait_pairwise)))] 
              } else {pair_trait_disc <- FALSE}
              if(any(treatments) & pair_trait_disc) { # stops this from running if only the continuous trait is selected
                pairwise <- pairwise(results, groups = list(trait_1, trait_2, trait_3)[[as.numeric(isolate(..(input$trait_pairwise)))]], 
                                     print.progress = F) 
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
  })
  
  model_comparison_listen <- reactive({list(input$independent_variables_model_1, input$go_run_model_comparison, 
                                            input$independent_variables_order_model_1)})
  
  
  model_comparison_metaO <- tryMetaObserve2({
    if(input$go_run_model_comparison > 0) {
      input$go_run_model_comparison # here to trigger this
      req(isolate(input$independent_variables_model_1)) # keeps things from crashing if all are unselected
      req(isolate(input$independent_variables_model_2)) 
      req(isolate(vals$trait_rx))
      
      gpa_coords <- isolate(gpa_coords_rx())
      metaExpr({
        independent_variables_model_1 <- isolate(..(input$independent_variables_model_1))
        independent_variables_model_2 <- isolate(..(input$independent_variables_model_2))
        independent_variables_model_3 <- isolate(..(input$independent_variables_model_3))
        if(isolate(..(input$independent_variables_order_model_1))[1] != "Trait 1") {
          if(isolate(..(datasets_dont_match())) == FALSE) {
            if(independent_variables_model_1[1] != "init" & independent_variables_model_2[1] != "init") { # required so that this doesn't try to run before input$independent_variables has been updated
              
              if(length(independent_variables_model_1) == 1) {
                if(independent_variables_model_1 == "csize") { model_order_1 <- "By Centroid Size" } else {
                  model_order_1 <- names(isolate(vals$trait_names))[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables_model_1] #   
                }
                model_order_1 <- substring(model_order_1, 4, nchar(model_order_1))
              } else { model_order_1 <- isolate(..(input$independent_variables_order_model_1)) }
              
              if(length(independent_variables_model_2) == 1) {
                if(independent_variables_model_2 == "csize") { model_order_2 <- "By Centroid Size" } else {
                  model_order_2 <- names(isolate(vals$trait_names))[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables_model_2] #   
                }
                model_order_2 <- substring(model_order_2, 4, nchar(model_order_2))
              } else { model_order_2 <- isolate(..(input$independent_variables_order_model_2)) }
              
              if(model_order_2[1] != "Trait 1") {
                model_1 <- model_definition(input.independent_variables = independent_variables_model_1, 
                                            vals.trait_rx = isolate(vals$trait_rx),
                                            vals.csize = isolate(vals$csize), 
                                            vals.trait_1_treatment = isolate(vals$trait_1_treatment), 
                                            vals.trait_2_treatment = isolate(vals$trait_2_treatment),
                                            vals.trait_3_treatment = isolate(vals$trait_3_treatment), 
                                            vals.trait_names = isolate(vals$trait_names), 
                                            vals.phy_rx = isolate(vals$phy_rx),
                                            gpa_coords_rx_reactive = gpa_coords, 
                                            input.independent_variables_order = model_order_1,
                                            input.pgls_ols = isolate(..(input$pgls_ols_model_comparison)), 
                                            input.anova_perm = isolate(..(input$anova_perm_model_comparison)), 
                                            input.ss_type = isolate(..(input$ss_type_model_comparison)))#, input.interactions_included = input$interactions_included_model_comparison)
                
                model_2 <- model_definition(input.independent_variables = independent_variables_model_2, 
                                            vals.trait_rx = isolate(vals$trait_rx), 
                                            vals.csize = isolate(vals$csize), 
                                            vals.trait_1_treatment = isolate(vals$trait_1_treatment), 
                                            vals.trait_2_treatment = isolate(vals$trait_2_treatment),
                                            vals.trait_3_treatment = isolate(vals$trait_3_treatment), 
                                            vals.trait_names = isolate(vals$trait_names), 
                                            vals.phy_rx = isolate(vals$phy_rx),
                                            gpa_coords_rx_reactive = gpa_coords, 
                                            input.independent_variables_order = model_order_2,
                                            input.pgls_ols = isolate(..(input$pgls_ols_model_comparison)), 
                                            input.anova_perm = isolate(..(input$anova_perm_model_comparison)), 
                                            input.ss_type = isolate(..(input$ss_type_model_comparison)))#, input.interactions_included = input$interactions_included_model_comparison)
              }
              if(isolate(..(input$add_third_model))){
                if(length(independent_variables_model_3) == 1) {
                  if(independent_variables_model_3 == "csize") { model_order_3 <- "By Centroid Size" } else {
                    model_order_3 <- names(vals$trait_names)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% independent_variables_model_3] #   
                  }
                  model_order_2 <- substring(model_order_2, 4, nchar(model_order_2))
                } else { model_order_3 <- isolate(..(input$independent_variables_order_model_3)) }
                
                model_3 <- model_definition(input.independent_variables = independent_variables_model_3, 
                                            vals.trait_rx = isolate(vals$trait_rx), 
                                            vals.csize = isolate(vals$csize), 
                                            vals.trait_1_treatment = isolate(vals$trait_1_treatment), 
                                            vals.trait_2_treatment = isolate(vals$trait_2_treatment),
                                            vals.trait_3_treatment = isolate(vals$trait_3_treatment), 
                                            vals.trait_names = isolate(vals$trait_names), 
                                            vals.phy_rx = isolate(vals$phy_rx),
                                            gpa_coords_rx_reactive = gpa_coords, 
                                            input.independent_variables_order = model_order_3,
                                            input.pgls_ols = isolate(..(input$pgls_ols_model_comparison)), 
                                            input.anova_perm = isolate(..(input$anova_perm_model_comparison)), 
                                            input.ss_type = isolate(..(input$ss_type_model_comparison))) # , input.interactions_included = input$interactions_included_model_comparison)
              }
              
              if(!isolate(..(input$add_third_model))){
                vals$model_comparison <- anova(model_1, model_2, print.progress = F)
              } else {
                vals$model_comparison <- anova(model_1, model_2, model_3, print.progress = F)
              }
              
            }
          }
        }
      })
    }
  })
  
  #  tryObserveEvent(input$independent_variables, priority = 100, {
  #    reset("interactions_included")
  #  })
  
  #### Outputs #####
  
  #### Data Input Outputs ####
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
    if(length(which(is.na(vals$trait_rx))) != length(vals$trait_rx)) {vals$trait_rx}
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
  
  #### Data Prep Outputs ####
  
  output$all_specimens <- metaRender2(renderPlot, bg = "transparent", {
    req(gpa_coords_rx())
    gpa_coords <- gpa_coords_rx()
    metaExpr({
      lm_col <- rep(..(input$semilms_color_other), dim(gpa_coords)[1])
      if(!is.null(vals$curves_final)) {
        if(nrow(vals$curves_final)>0) {
          curves_temp <- matrix(unlist(vals$curves_final), ncol = 3)
          if((!anyNA(curves_temp[,2])) & !("" %in% curves_temp[,2])){ # as long as that semis column isn't NAs
            lm_col[as.numeric(unlist(vals$curves_final[,c(1,3)]))] <- ..(input$semilms_color_brackets) 
            lm_col[as.numeric(unlist(vals$curves_final[,2]))] <- ..(input$semilms_color)
          }
        }
      }
      
      if(is.null(vals$links_df)) { go <- T } else { # making it stop before plotting when stereomorph curves havent yet been applied
        if(any(!(vals$links_df %in% 1:dim(gpa_coords)[1]))){ go <- F } else {go <- T}
      } 
      
      if(go) {
        par(mar = c(2,3,2,2))
        plotAllSpecimens(gpa_coords, 
                         label = T, links = vals$links_df,  
                         plot.param = list(txt.cex = 1.5, 
                                           txt.col = ..(input$semilms_color_labels),
                                           pt.bg = ..(input$semilms_color_individlms), 
                                           link.col = ..(input$semilms_color_links),
                                           mean.bg = lm_col))
        
      }
    })
  })
  
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
  
  output$export_plot_all_specimen_code <- downloadHandler(
    filename = function() { paste('all_specimens_aligned-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      
      code <- expandChain(output$all_specimens())
      code <- edit_export_code(code)
      code <- c(
        '# The following lines of code replace the raw landmarks with the generalized procrustes alignment landmarks, which may not have explicitly been done in the app yet (tab Generalized Procrustes Alignment on Data Prep page)
# Although this tab comes before the gpa, the coordinates must be aligned for this type of visualization. 
# The tabs are ordered in this way because semi-landmarks must be defined prior to alignment.
# If youd like to visualize this plot prior to alignment, you can delete the lines marked with *** below.  
# Also note that the semi-landmarks will only be applied to this exported dataset if the "Apply Semilandmark Matrix" button has been pushed prior to data exportation.
if(!is.null(vals$curves)) {
    if(nrow(vals$curves) > 0){
       curves <- as.matrix(vals$curves)
       curve.mat <- matrix(as.integer(curves), ncol = 3, byrow = F)
       colnames(curve.mat) <- c("before", "slide", "after")
     }else { curve.mat <- NULL}
   }else { curve.mat <- NULL} 
gpa_coords <- gpagen(gpa_coords, curves = curve.mat) # ***
gpa_coords <- gpa_coords$coords # ***
        ', code)
      
      code <- c(
        general_notes(),
        prep_code(),
        code) 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx()
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "all_specimens_aligned_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"),
                overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
    },
    contentType = "application/zip"
  )
  
  output$export_semilm_mat <- downloadHandler( 
    filename = function() {paste('semilms_matrix.csv')},
    content = function(file) { write.csv(vals$curves_final, file, row.names = F) }
  )
  
  output$visualize_outliers_all <- metaRender2(renderPlot, bg = "transparent", { 
    req(gpa_coords_rx())
    gpa_coords <- gpa_coords_rx()
    metaExpr({
      grouping <- NULL
      column <- (2:4)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% ..(input$outlier_group)]
      outlier_group_tf <- ..(input$outlier_group_tf)
      
      if(length(column) > 0 & outlier_group_tf) { 
        grouping <- as.factor(vals$trait_rx[,column]) 
        grouping <- grouping[match(dimnames(gpa_coords)[[3]], vals$trait_rx[,1])]
      } else {grouping <- NULL}
      if(outlier_group_tf) {
        which_group_new <- as.numeric(..(input$outlier_group_level_plotted))
      } else { which_group_new <- 1 }
      x <- plotOutliers.ekb1(gpa_coords, groups = grouping)
      
      plotOutliers.ekb2(x, gpa_coords, groups = grouping, 
                        which_group = which_group_new, pt_cex = as.numeric(..(input$outlier_plot_pt_cex)),
                        txt_cex = as.numeric(..(input$outlier_plot_txt_cex)), 
                        show_point_names = ..(input$outlier_plot_show_point_names_tf)) 
      if(!is.null(vals$outlier_row)) { points(x = vals$outlier_row[1], y = vals$outlier_row[2], col = "red", 
                                              cex = as.numeric(..(input$outlier_plot_pt_cex)) + 2, pch = 1) } # this adds a red circle around whichever specimen was selected and visualized
      
    })
  })
  
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
      if(!is.null(vals$outlier_row)) { points(x = vals$outlier_row[1], y = vals$outlier_row[2], col = "red", 
                                              cex = as.numeric(input$outlier_plot_pt_cex) + 2, pch = 1) } # this adds a red circle around whichever specimen was selected and visualized
      dev.off() # copied from above. using a reactive alone doesn't work
      
    }
  )
  
  output$export_visualize_outliers_all_code <- downloadHandler(
    filename = function() { paste('visualize_outliers-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- expandChain(output$visualize_outliers_all())
      code <- edit_export_code(code)
      
      code <- c(
        '# The following lines of code replace the raw landmarks with the generalized procrustes alignment landmarks, which may not have explicitly been done in the app yet (tab Generalized Procrustes Alignment on Data Prep page)
# Although this tab comes before the gpa, the coordinates must be aligned for this type of visualization. 
# The tabs are ordered in this way because outlier specimens must be removed prior to alignment.
# If youd like to visualize this plot prior to alignment, you can delete the lines marked with *** below.  
# Disclaimer: If you choose to visualize this plot prior to alignment, and you have selected a specimen to visualize on its own, the red circle will not appear in the correct spot in the plot. 
# This red circle can be edited in the final 3 lines of exported code.
# Also note that the excluded specimens will only be applied to this exported dataset if the "Remove" button has been pushed prior to data exportation.
if(!is.null(vals$curves)) {
    if(nrow(vals$curves) > 0){
       curves <- as.matrix(vals$curves)
       curve.mat <- matrix(as.integer(curves), ncol = 3, byrow = F)
       colnames(curve.mat) <- c("before", "slide", "after")
     }else { curve.mat <- NULL}
   }else { curve.mat <- NULL} 
gpa_coords <- gpagen(gpa_coords, curves = curve.mat) # ***
gpa_coords <- gpa_coords$coords # ***
        ', code)
      
      code <- c(
        general_notes(),
        prep_code(add.source = T),
        code) 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx()
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "visualize_outliers_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"),
                overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
    },
    contentType = "application/zip"
  )
  
  output$outlier_selected_lms <- metaRender2(renderPlot, bg = "transparent", {
    req(vals$outlier_row)
    if(nrow(vals$outlier_row)>0) {
      gpa_coords <- gpa_coords_rx()
      
      
      grouping <- NULL
      column <- (2:4)[c("by_trait_1", "by_trait_2", "by_trait_3") %in% input$outlier_group]
      if(length(column) > 0 & input$outlier_group_tf) { 
        grouping <- as.factor(vals$trait_rx[,column]) 
        grouping <- grouping[match(dimnames(gpa_coords)[[3]], vals$trait_rx[,1])]
      } else {grouping <- NULL}
      if(input$outlier_group_tf) {
        which_group_new <- as.numeric(input$outlier_group_level_plotted)
      } else { which_group_new <- 1 }
      
      x1 <- plotOutliers.ekb1(gpa_coords, groups = grouping)
      vals$x <- plotOutliers.ekb2(x1, gpa_coords, groups = grouping, 
                                  which_group = which_group_new, pt_cex = as.numeric(input$outlier_plot_pt_cex),
                                  produce_plot = F, txt_cex = as.numeric(input$outlier_plot_txt_cex), 
                                  show_point_names = input$outlier_plot_show_point_names_tf)
      y <- x[order(vals$x, decreasing = TRUE)]
      outlier_df <- data.frame("x" = order(y, decreasing = TRUE), "y" = y)
      
      metaExpr({
        
        selected_outlier <- match(row.names(vals$outlier_row), names(vals$x))
        lm_col <- rep(..(input$vis_outliers_color_other), dim(gpa_coords)[1])
        if(!is.null(vals$curves_final)) {
          temp_mat <- matrix(vals$curves_final, ncol = 3, byrow = T)
          if(!anyNA(temp_mat[,2])){ 
            lm_col[as.numeric(unlist(vals$curves_final[,1]))] <- ..(input$vis_outliers_color_brackets)
            lm_col[as.numeric(unlist(vals$curves_final[,3]))] <- ..(input$vis_outliers_color_brackets)
            lm_col[as.numeric(unlist(vals$curves_final[,2]))] <- ..(input$vis_outliers_color)
          }
        }
        
        par(mar = c(0,2,4,0))
        plot(gpa_coords[,,selected_outlier], col = lm_col, pch = 19, cex = 2,
             main = paste("Selected Specimen: ", row.names(vals$outlier_row), sep = ""), 
             asp = 1, axes = F) # displays the selected specimen landmark placement
        text(x = gpa_coords[,,selected_outlier][,1]-0.01, y = gpa_coords[,,selected_outlier][,2] - 0.01, 
             labels = 1:(nrow(gpa_coords[,,selected_outlier])), col = ..(input$vis_outliers_color_labels)) # adds labels to the landmarks
        if(!is.null(vals$links_df)){
          for (i in 1:nrow(vals$links_df)) {
            seg_df <- gpa_coords[,,selected_outlier]
            xys <- seg_df[vals$links_df[i,],]
            segments(x0 = xys[1], y0 = xys[3],x1 = xys[2], y1 = xys[4], col = ..(input$vis_outliers_color_links))
          }
        }
      })
    }
  })
  
  output$export_outlier_selected_lms <- downloadHandler( 
    filename = function() { paste(row.names(vals$outlier_row), "-landmarks.pdf", sep = "") },
    content = function(file) { 
      pdf(file)
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
          lm_col[as.numeric(unlist(vals$curves_final[,1]))] <- input$vis_outliers_color_brackets
          lm_col[as.numeric(unlist(vals$curves_final[,3]))] <- input$vis_outliers_color_brackets
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
      
      dev.off() # copied from above. using a reactive alone doesn't work
    }
  )
  
  output$export_outlier_selected_lms_code <- downloadHandler(
    filename = function() { paste('outlier_selected_lms-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- expandChain(output$outlier_selected_lms())
      code <- c(
        general_notes(),
        prep_code(),
        edit_export_code(code)) 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "outlier_selected_lms_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"),
                overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
    },
    contentType = "application/zip"
  )
  
  output$outlier_removed_names <- renderText(sep = ", ",{
    if(!is.null(vals$outlier_removed_names)){
      vals$outlier_removed_names <- unique(isolate(vals$outlier_removed_names))
    }
  })
  
  output$outlier_removed_names_tab <- renderText({
    if(is.null(vals$outlier_removed_names)) {
      mat <- matrix("None")
      colnames(mat) <- " "
      mat
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
  
  output$export_aligned_lms <- downloadHandler( 
    filename = function() {paste('lms_aligned.tps')},
    content = function(file) { writeland.tps(gpa_coords_rx(), file) }
  )
  
  output$export_run_gpa_code <- downloadHandler(
    filename = function() { paste('run_gpa-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code_p1 <- c(general_notes(), prep_code(),
                   'if(anyNA(vals$lms_rx)) {
lms_estimated <- estimate.missing(vals$lms_rx, method = "TPS")
} else {lms_estimated <-  vals$lms_rx}

if(!is.null(vals$curves)) {
  curve.mat <- matrix(as.integer(as.matrix(vals$curves)), ncol = 3, byrow = F)
  colnames(curve.mat) <- c("before", "slide", "after")
} else { curve.mat <- NULL }
vals$Data_gpa <- gpagen(lms_estimated, curves = curve.mat, ')
      code_p2 <- if(input$ProcD) { paste('ProcD = T,', sep = '') } else {
        paste('ProcD = F,', sep = '') }
      code_p3 <- if(input$Proj) { paste('Proj = T,', sep = '') } else {
        paste('ProcD = F,', sep = '') }
      code_p4 <- 'print.progress = F)

gpa_coords <- vals$Data_gpa$coords
vals$csize <- vals$Data_gpa$Csize
names(vals$csize) <- dimnames(gpa_coords)[[3]]'
      code <- c(code_p1, code_p2, code_p3, code_p4)
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "run_gpa_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"),
                overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
    },
    contentType = "application/zip"
  )
  
  #### Morphospace Outputs ####
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
  
  output$export_rotation_loadings <- downloadHandler(
    filename = function() { paste("rotation_loadings.csv")},
    content = function(file) { 
      write.csv(pca_rx()$rotation, file)
    }
  )
  
  output$export_component_scores <- downloadHandler(
    filename = function() { paste("component_scores.csv")},
    content = function(file) { 
      write.csv(pca_rx()$x, file)
    }
  )
  
  tryObserveEvent(input$flip_x_axis, ignoreInit = T, {
    
    input.flip_axis <- vals$input.flip_axis
    if(is.null(input$flip_x_axis)) { input.flip_axis <- NULL } else {
      input.flip_axis  <- c(input.flip_axis, 1) 
    }
    vals$input.flip_axis <- as.numeric(input.flip_axis)
    
    if(!is.null(input.flip_axis)){
      if(input.flip_axis[length(input.flip_axis)] == 1) {
        vals$clicked_point[as.numeric(input$pca_x_axis)] <- vals$clicked_point[as.numeric(input$pca_x_axis)]*-1
        if(input$warp_comparison_start == "selected_obs_byname" & !is.null(input$warp_specific_ref_specimen)) { # if input start is by name
          vals$specimen_row_ref[as.numeric(input$pca_x_axis)] <- vals$specimen_row_ref[as.numeric(input$pca_x_axis)]*-1
        }
        if(input$warp_comparison_end == "selected_obs_byname" & !is.null(input$warp_specific_targ_specimen)) { # if input end is by name
          vals$specimen_row_targ[as.numeric(input$pca_x_axis)] <- vals$specimen_row_targ[as.numeric(input$pca_x_axis)]*-1
        }
      } 
    }
  })
  
  tryObserveEvent(input$flip_y_axis, ignoreInit = T, {
    
    input.flip_axis <- vals$input.flip_axis
    if(is.null(input$flip_y_axis)) { input.flip_axis <- NULL } else {
      input.flip_axis  <- c(input.flip_axis, 2) 
    }
    vals$input.flip_axis <- as.numeric(input.flip_axis)
    
    if(!is.null(input.flip_axis)){
      if(input.flip_axis[length(input.flip_axis)] == 2) {
        vals$clicked_point[as.numeric(input$pca_y_axis)] <- vals$clicked_point[as.numeric(input$pca_y_axis)]*-1
        if(input$warp_comparison_start == "selected_obs_byname" & !is.null(input$warp_specific_ref_specimen)) { # if input start is by name
          vals$specimen_row_ref[as.numeric(input$pca_y_axis)] <- vals$specimen_row_ref[as.numeric(input$pca_y_axis)]*-1
        }
        if(input$warp_comparison_end == "selected_obs_byname" & !is.null(input$warp_specific_targ_specimen)) { # if input end is by name
          vals$specimen_row_targ[as.numeric(input$pca_y_axis)] <- vals$specimen_row_targ[as.numeric(input$pca_y_axis)]*-1
        }
      } 
    }
  })
  
  tryObserveEvent(input$pca_x_axis, ignoreInit = T, { # resets x axis flip count when you change the x axis
    if(!is.null(input$flip_x_axis)) { # if the x axis has been flipped
      input.flip_axis <- vals$input.flip_axis
      input.flip_axis <- input.flip_axis[input.flip_axis != 1] # keep all parts of input.flip_axis that are not "1"
      vals$input.flip_axis  <- input.flip_axis
    }
  })
  
  tryObserveEvent(input$pca_y_axis, ignoreInit = T, { # resets y axis flip count when you change the y axis
    if(!is.null(input$flip_y_axis)) { # if the y axis has been flipped
      input.flip_axis <- vals$input.flip_axis
      input.flip_axis <- input.flip_axis[input.flip_axis != 2] # keep all parts of input.flip_axis that are not "2"
      vals$input.flip_axis  <- input.flip_axis
    }
  })
  
  output$morphospace <- metaRender2(renderPlot, bg = scales::alpha("white", 0), {
    req(vals$morphospace)
    pr <- pca_rx() 
    
    gpa_coords <- gpa_coords_rx() # CONSIDER REMOVING AND REPLACING THE 1 SPOT W pr INFO ???
    
    metaExpr({
      
      class(pr) <- "ordinate"
      if(!is.null(vals$tip_pch)) {input_pch <- as.numeric(vals$tip_pch)} else {input_pch <- 19}
      pr.plot <- RRPP:::plot.ordinate(pr, axis1 = as.numeric(..(input$pca_x_axis)), 
                                      axis2 = as.numeric(..(input$pca_y_axis)), 
                                      pch = input_pch, cex = as.numeric(..(input$tip_cex)),
                                      col = vals$tip_col, asp = 1)
      
      abline(h = 0, lty = 2) # these are only here until Mike changes the plot.ordinate function in RRPP
      abline(v = 0, lty = 2) # these are only here until Mike changes the plot.ordinate function in RRPP
      
      if(..(input$include_phylo)) {
        
        z <- vals$morphospace$phylo$phy.pcdata
        edges <- as.matrix(vals$phy_rx$edge)
        
        for (i in 1:NROW(edges)) {
          pts <- z[edges[i, ], ]
          points(pts, type = "l", col = ..(input$edge_col), lwd = as.numeric(..(input$edge_width)), lty = 1) 
        }
        ancs <- z[(length(vals$phy_rx$tip.label)+1):nrow(z),]
        points(ancs, pch = as.numeric(..(input$node_pch)), cex = as.numeric(..(input$node_cex)), col = ..(input$node_col))
        
        if(..(input$show_node_label)) {
          text(ancs, rownames(ancs), adj = c(-(..(input$node_cex))/15,-(..(input$node_cex))/15), 
               cex = as.numeric(..(input$node_txt_cex)), col = ..(input$node_txt_col)) # all these can be changed if we want to make them options
        }
      }
      
      if(..(input$show_tip_label)) {
        text(pr.plot$points, rownames(pr.plot$points), adj = c(-(..(input$tip_cex))/15,-(..(input$tip_cex))/15),
             cex = as.numeric(..(input$tip_txt_cex)), col = vals$tip_col)
      }
      
      if(!is.null(vals$morpho_clicked)) {
        if(..(input$warp_comparison_start == "selected_obs") | ..(input$warp_comparison_start == "selected_obs_byname")) {
          x_start <- vals$specimen_row_ref[,as.numeric(..(input$pca_x_axis))]
          y_start <- vals$specimen_row_ref[,as.numeric(..(input$pca_y_axis))]
          points(x = x_start, y = y_start, col = "red", 
                 cex = as.numeric(..(input$tip_cex)) + 1.2, pch = 1)
        }
        
        if(..(input$warp_comparison_end) == "selected_obs" | ..(input$warp_comparison_end == "selected_obs_byname")) {
          x_end <- vals$specimen_row_targ[,as.numeric(..(input$pca_x_axis))]
          y_end <- vals$specimen_row_targ[,as.numeric(..(input$pca_y_axis))]
          points(x = x_end, y = y_end, col = "red", 
                 cex = as.numeric(..(input$tip_cex)) + 1, pch = 1)
        } 
      }
      
      if(!is.null(vals$morpho_dbclicked)) { 
        x_proj <- vals$projection_row[1]
        y_proj <- vals$projection_row[2]
        points(x = x_proj, y = y_proj, col = "red", 
               cex = as.numeric(..(input$tip_cex)) + 1, pch = 4)
      }
      
      if(..(input$warp_comparison_show_arrow) == TRUE & exists("x_end")) { # seeing if any points have been defined before making an arrow
        if(!is.null(..(input$warp_comparison_end))) {
          if(..(input$warp_comparison_start) == "mean") { start.xy <- c(0,0)}
          if(..(input$warp_comparison_start) == "selected_proj") { start.xy <- c(x_proj, y_proj)} 
          if(..(input$warp_comparison_start) == "selected_obs" |
             ..(input$warp_comparison_start) == "selected_obs_byname") { start.xy <- c(x_start,y_start)} # pulled from the above lines
          if(..(input$warp_comparison_end) == "mean") { end.xy <- c(0,0)}
          if(..(input$warp_comparison_end) == "selected_proj") { end.xy <- c(x_proj, y_proj) } 
          if(..(input$warp_comparison_end) == "selected_obs"| 
             ..(input$warp_comparison_end) == "selected_obs_byname") { end.xy <- c(x_end,y_end) } # pulled from the lines above
          
          
          arrows(x0 = start.xy[1], y0 = start.xy[2], x1 = end.xy[1], y1 = end.xy[2], 
                 col = "red", lty = 1, lwd = 2)
        }
      }
      
      if(..(input$show_convex_hull_1) | 
         ..(input$show_convex_hull_2) | 
         ..(input$show_convex_hull_3)) {
        selected_trait <- (2:4)[c(..(input$show_convex_hull_1), 
                                  ..(input$show_convex_hull_2), 
                                  ..(input$show_convex_hull_3))]
        colored_tips <- (2:4)[c(..(input$tip_col_category) == "by_trait_1", 
                                ..(input$tip_col_category) == "by_trait_2", 
                                ..(input$tip_col_category) == "by_trait_3")]
        
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
          tip_col_fac <- as.character(vals$trait_rx[match(dimnames(gpa_coords)[[3]], vals$trait_rx[,1]),this_col])
          lev <- length(unique(tip_col_fac))
          lev_options <- unique(tip_col_fac)
          col_mat <- unlist(col_list_modified[[i]]) 
          for(j in 1:length(unique(tip_col_fac))) {
            reordered_edges <- pr$x[which(tip_col_fac == lev_options[j]), 
                                    c(as.numeric(..(input$pca_x_axis)), as.numeric(..(input$pca_y_axis)))]
            edge_points <- row.names(reordered_edges)[chull(reordered_edges)]
            polygon(x = pr$x[edge_points, as.numeric(..(input$pca_x_axis))], 
                    y = pr$x[edge_points, as.numeric(..(input$pca_y_axis))], 
                    col = adjustcolor(col_mat[j],alpha.f = .5), 
                    border = col_mat[j]) # copy this code to the export part of the morphospace
          }
        }
      }
    })
  }, width = "auto", height = "auto")
  
  output$export_morphospace <- downloadHandler( # copy this from metaExpr from output$morphospace with each new release, delete the ..s 
    filename = function() { paste("morphospace_plot.pdf") },
    content = function(file) { 
      pdf(file, width = 15, height = 15)
      
      
      req(vals$morphospace)
      
      pr <- pca_rx() 
      class(pr) <- "ordinate"
      if(!is.null(vals$tip_pch)) {input_pch <- as.numeric(vals$tip_pch)} else {input_pch <- 19}
      pr.plot <- RRPP:::plot.ordinate(pr, axis1 = as.numeric((input$pca_x_axis)), 
                                      axis2 = as.numeric((input$pca_y_axis)), 
                                      pch = input_pch, cex = as.numeric((input$tip_cex)),
                                      col = vals$tip_col, asp = 1)
      
      abline(h = 0, lty = 2) # these are only here until Mike changes the plot.ordinate function in RRPP
      abline(v = 0, lty = 2) # these are only here until Mike changes the plot.ordinate function in RRPP
      
      if((input$include_phylo)) {
        
        z <- vals$morphospace$phylo$phy.pcdata
        edges <- as.matrix(vals$phy_rx$edge)
        
        for (i in 1:NROW(edges)) {
          pts <- z[edges[i, ], ]
          points(pts, type = "l", col = (input$edge_col), lwd = as.numeric((input$edge_width)), lty = 1) 
        }
        ancs <- z[(length(vals$phy_rx$tip.label)+1):nrow(z),]
        points(ancs, pch = as.numeric((input$node_pch)), cex = as.numeric((input$node_cex)), col = (input$node_col))
        
        if((input$show_node_label)) {
          text(ancs, rownames(ancs), adj = c(-((input$node_cex))/15,-((input$node_cex))/15), 
               cex = as.numeric((input$node_txt_cex)), col = (input$node_txt_col)) # all these can be changed if we want to make them options
        }
      }
      
      if((input$show_tip_label)) {
        text(pr.plot$points, rownames(pr.plot$points), adj = c(-((input$tip_cex))/15,-((input$tip_cex))/15),
             cex = as.numeric((input$tip_txt_cex)), col = vals$tip_col)
      }
      
      if(!is.null(vals$morpho_clicked)) {
        if((input$warp_comparison_start == "selected_obs") | (input$warp_comparison_start == "selected_obs_byname")) {
          x_start <- vals$specimen_row_ref[,as.numeric((input$pca_x_axis))]
          y_start <- vals$specimen_row_ref[,as.numeric((input$pca_y_axis))]
          points(x = x_start, y = y_start, col = "red", 
                 cex = as.numeric((input$tip_cex)) + 1.2, pch = 1)
        }
        
        if((input$warp_comparison_end) == "selected_obs" | (input$warp_comparison_end == "selected_obs_byname")) {
          x_end <- vals$specimen_row_targ[,as.numeric((input$pca_x_axis))]
          y_end <- vals$specimen_row_targ[,as.numeric((input$pca_y_axis))]
          points(x = x_end, y = y_end, col = "red", 
                 cex = as.numeric((input$tip_cex)) + 1, pch = 1)
        } 
      }
      
      if(!is.null(vals$morpho_dbclicked)) { 
        x_proj <- vals$projection_row[1]
        y_proj <- vals$projection_row[2]
        points(x = x_proj, y = y_proj, col = "red", 
               cex = as.numeric((input$tip_cex)) + 1, pch = 4)
      }
      
      if((input$warp_comparison_show_arrow) == TRUE & exists("x_end")) { # seeing if any points have been defined before making an arrow
        if(!is.null((input$warp_comparison_end))) {
          if((input$warp_comparison_start) == "mean") { start.xy <- c(0,0)}
          if((input$warp_comparison_start) == "selected_proj") { start.xy <- c(x_proj, y_proj)} 
          if((input$warp_comparison_start) == "selected_obs" |
             (input$warp_comparison_start) == "selected_obs_byname") { start.xy <- c(x_start,y_start)} # pulled from the above lines
          if((input$warp_comparison_end) == "mean") { end.xy <- c(0,0)}
          if((input$warp_comparison_end) == "selected_proj") { end.xy <- c(x_proj, y_proj) } 
          if((input$warp_comparison_end) == "selected_obs"| 
             (input$warp_comparison_end) == "selected_obs_byname") { end.xy <- c(x_end,y_end) } # pulled from the lines above
          
          
          arrows(x0 = start.xy[1], y0 = start.xy[2], x1 = end.xy[1], y1 = end.xy[2], 
                 col = "red", lty = 1, lwd = 2)
        }
      }
      
      if((input$show_convex_hull_1) | 
         (input$show_convex_hull_2) | 
         (input$show_convex_hull_3)) {
        selected_trait <- (2:4)[c((input$show_convex_hull_1), 
                                  (input$show_convex_hull_2), 
                                  (input$show_convex_hull_3))]
        colored_tips <- (2:4)[c((input$tip_col_category) == "by_trait_1", 
                                (input$tip_col_category) == "by_trait_2", 
                                (input$tip_col_category) == "by_trait_3")]
        
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
            reordered_edges <- pr$x[which(tip_col_fac == lev_options[j]), 
                                    c(as.numeric((input$pca_x_axis)), as.numeric((input$pca_y_axis)))]
            edge_points <- row.names(reordered_edges)[chull(reordered_edges)]
            polygon(x = pr$x[edge_points, as.numeric((input$pca_x_axis))], 
                    y = pr$x[edge_points, as.numeric((input$pca_y_axis))], 
                    col = adjustcolor(col_mat[j],alpha.f = .5), 
                    border = col_mat[j]) # copy this code to the export part of the morphospace
          }
        }
      }
      dev.off()
    }
  )
  
  output$export_morphospace_code <- downloadHandler(
    filename = function() { paste('morphospace-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- expandChain(output$morphospace())
      code <- c(
        general_notes(),
        "# Legends can also be added to the morphospace plot using the 'legend' function from the graphics package.",
        prep_code(),
        'pr <- pca_rx',
        edit_export_code(code)) 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx }
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "morphospace_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  # Legends 
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
  
  #### Warp Grid Outputs ####
  
  plotRefToTarget_rx <- metaReactive2({ 
    req(ref_rx())
    req(targ_rx())
    req(vals$warp_initiated)
    gridPar_vals <- gridPar_vals()
    ref_rx <- ref_rx()
    targ_rx <- targ_rx()
    
    metaExpr({
      par(mar = c(0,0,0,0))
      plotRefToTarget(ref_rx, targ_rx, mag = ..(input$warp_mag), label = ..(input$warp_labels),
                      links = vals$links_df, bg = "transparent", method = ..(input$warp_type), 
                      gridPars = gridPar_vals)
    })
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
  
  output$export_warp_grid_code <- downloadHandler(
    filename = function() { paste('warp_grid-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- expandChain(
        ref_rx(),
        targ_rx(),
        plotRefToTarget_rx())
      
      code <- c(
        general_notes(),
        prep_code(),
        edit_export_code(code)) 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "warp_grid_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  #### Shape Patterns Outputs ####
  
  output$stats_modularity_test <- metaRender2(renderPrint, {
    req(gpa_coords_rx())
    gpa_coords <- gpa_coords_rx()
    metaExpr({
      if(..(input$modularity_phylo_tf) == F) {
        vals$modularity_test <- modularity.test(A = gpa_coords, partition = vals$modularity_groups, 
                                                iter = ..(input$modularity_perm),
                                                print.progress = F)
      } else {
        if(!is.null(vals$phy_rx)) {
          vals$modularity_test <- phylo.modularity(A = gpa_coords, partition.gp = vals$modularity_groups, 
                                                   phy = vals$phy_rx, iter = ..(input$modularity_perm),
                                                   print.progress = F)
        }
      }
      if(!is.null(vals$modularity_test)){
        summary(vals$modularity_test)
      }
      
    })
  })
  
  output$plot_modularity_visualization <- metaRender2(renderPlot, bg = "transparent", {
    req(vals$mshape_gpacoords_rx) 
    metaExpr({
      coords <- vals$mshape_gpacoords_rx
      point_cols <- as.character(vals$modularity_groups) 
      point_cols[which(point_cols == "1")] <- ..(input$module_1_col)
      point_cols[which(point_cols == "2")] <- ..(input$module_2_col)
      point_cols[which(point_cols == "3")] <- ..(input$module_3_col)
      point_cols[which(point_cols == "4")] <- ..(input$module_4_col)
      point_cols[which(point_cols == "5")] <- ..(input$module_5_col)
      point_cols[which(point_cols == "6")] <- ..(input$module_6_col)
      point_cols[which(point_cols == "7")] <- ..(input$module_7_col)
      point_cols[which(point_cols == "8")] <- ..(input$module_8_col)
      
      plot(matrix(coords, ncol = 2), axes = F, asp = 1, pch = 19, xlab = "", ylab = "", 
           col = point_cols, cex = as.numeric(..(input$plot_modularity_visualization_cex)),
           main = NULL)
      if(!is.null(vals$links_df)) {
        for(i in 1:nrow(vals$links_df)) {
          links <- rbind(coords[vals$links_df[i,1],], coords[vals$links_df[i,2],])
          points(x = links[,1], y = links[,2], type = "l")
        }
      }
      text(matrix(coords, ncol = 2), labels = 1:nrow(coords), pos = 4, col = point_cols, 
           cex = as.numeric(..(input$plot_modularity_visualization_cex)))
    })
  })
  
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
  
  output$export_modularity_visualization_code <- downloadHandler(
    filename = function() { paste('modularity_visualization-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- expandChain(output$plot_modularity_visualization())
      code <- c(
        general_notes(),
        prep_code(),
        edit_export_code(code)) 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "modularity_visualization_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$plot_modularity_test <- renderPlot(bg = "transparent", {
    req(vals$modularity_test)
    plot(vals$modularity_test)
  })
  
  output$export_modularity_test_plot <- downloadHandler(
    filename = function() { paste("modularity_test_plot.pdf") },
    content = function(file) { 
      pdf(file, width = 10, height = 6, pointsize = 8)
      req(vals$modularity_test)
      plot(vals$modularity_test)
      dev.off()
    }
  )
  
  output$export_modularity_test_code <- downloadHandler(
    filename = function() { paste('modularity_test-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- expandChain(output$stats_modularity_test())
      
      code <- c(
        general_notes(),
        prep_code(),
        edit_export_code(code),
        'plot(vals$modularity_test)') 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "modularity_test_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$stats_compare_multi_evol_rates <- metaRender2(renderPrint, {
    req(gpa_coords_rx())
    req(vals$phy_rx)
    req(vals$modularity_groups)
    gpa_coords <- gpa_coords_rx()
    metaExpr({
      modularity_groups <- droplevels(as.factor(vals$modularity_groups))
      vals$compare_multi_evol_rates <- try(compare.multi.evol.rates(A = gpa_coords, 
                                                                    phy = vals$phy_rx, gp = modularity_groups, 
                                                                    iter = ..(input$modularity_perm),
                                                                    print.progress = F), silent = T)
      if(inherits(vals$compare_multi_evol_rates, "try-error")) { vals$compare_multi_evol_rates <- NULL } else {
        summary(vals$compare_multi_evol_rates)
      }
    })
  })
  
  output$export_compare_multi_evol_rates_table <- downloadHandler(
    filename = function() { paste('compare_multi_evol_rates.csv')},
    content = function(file) { write.csv(vals$compare_multi_evol_rates, file) }
  )
  
  output$plot_compare_multi_evol_rates <- renderPlot(bg = "transparent", {
    req(vals$compare_multi_evol_rates)
    plot(vals$compare_multi_evol_rates)
  })
  
  output$export_compare_multi_evol_rates_code <- downloadHandler(
    filename = function() { paste('compare_multi_evol_rates-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- expandChain(output$stats_compare_multi_evol_rates())
      
      code <- c(
        general_notes(),
        prep_code(),
        edit_export_code(code),
        'plot(vals$compare_multi_evol_rates)') 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "compare_multi_evol_rates_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$global_integration_plot <- renderPlot(bg = "transparent", {
    globalIntegration(A = gpa_coords_rx(), ShowPlot = T) 
  })
  
  output$export_global_integration_plot <- downloadHandler(
    filename = function() { paste("global_integration_plot.pdf")},
    content = function(file) {
      pdf(file, width = 18, height = 13)
      globalIntegration(A = gpa_coords_rx(), ShowPlot = T) 
      dev.off()
    }
  )
  
  output$export_global_integration_code <- downloadHandler(
    filename = function() { paste('global_integration-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- c(
        'rm(list = ls())
load("data_current_state.RData") # replace with your data_current_state file (you must press the Export Current Data button AFTER all settings and inputs have been finalized for the desired plot/results)  
library(geomorph)
globalIntegration(A = gpa_coords, ShowPlot = T)')
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "global_integration_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$integration_test_plot <- metaRender2(renderPlot, bg = "transparent", { # groups defined by_trait_2 when both are approrpriate
    req(input$integration_group_by)
    req(vals$modularity_groups)
    req(gpa_coords_rx())
    gpa_coords <- gpa_coords_rx()
    
    metaExpr({
      if(..(input$integration_group_by) == "none"){
        if(..(input$integration_phylo_tf)) {
          if(!is.null(vals$phy_rx)){
            vals$IT <- phylo.integration(gpa_coords, phy = vals$phy_rx, 
                                         partition.gp = vals$modularity_groups, 
                                         iter = ..(input$integration_test_perm), 
                                         print.progress = F)
          }
        } else vals$IT <- integration.test(gpa_coords, partition.gp = vals$modularity_groups, 
                                           iter = ..(input$integration_test_perm), print.progress = F)
        if(!is.null(vals$IT$XScores)) {
          plot(vals$IT)
        }
      } else {
        i <- (2:4)[c(..(input$integration_group_by) == "by_trait_1", 
                     ..(input$integration_group_by) == "by_trait_2", 
                     ..(input$integration_group_by) == "by_trait_3")]
        if(!is.null(vals$trait_rx)) {
          groupings <- as.factor(vals$trait_rx[,i])
          integ.test <- list(NA)
          if(..(input$integration_phylo_tf)) {
            if(!is.null(vals$phy_rx)){
              for(i in 1:nlevels(groupings)){
                coords_subset <- gpa_coords[,,which(groupings == levels(groupings)[i])]
                phy_subset <- keep.tip(vals$phy_rx, dimnames(coords_subset)[[3]])
                integ.test.new <- phylo.integration(coords_subset, phy = phy_subset, partition.gp = vals$modularity_groups,
                                                    iter = ..(input$integration_test_perm), print.progress = F)
                integ.test[[i]] <- integ.test.new
              }
              ..(input$integration_group_level)
              vals$IT <- integ.test[[as.numeric(isolate(..(input$integration_group_level)))]]
            }
          } else {
            for(i in 1:nlevels(groupings)){
              coords.subset <- gpa_coords[,,which(groupings == levels(groupings)[i])]
              integ.test.new <- integration.test(coords.subset, partition.gp = vals$modularity_groups, 
                                                 iter = ..(input$integration_test_perm), 
                                                 print.progress = F)
              integ.test[[i]] <- integ.test.new
            }
            ..(input$integration_group_level) # this is required so that the vals$ bit doesn't get triggered for this to run again if isolate taken out
            
            if(!is.null(isolate(..(input$integration_group_level)))) {
              vals$IT <- integ.test[[as.numeric(isolate(..(input$integration_group_level)))]]
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
  })
  
  output$integration_test_results <- renderPrint({
    summary(vals$IT)
  })
  
  output$export_integration_test_plot <- downloadHandler(
    filename = function() { paste("integration_test_plot.pdf")},
    content = function(file) {
      pdf(file, width = 10, height = 7)
      if(input$integration_group_by == "none"){
        if(input$integration_phylo_tf) {
          req(vals$phy_rx)
          vals$IT <- phylo.integration(gpa_coords_rx(), phy = vals$phy_rx, 
                                       partition.gp = vals$modularity_groups, iter = input$integration_test_perm, print.progress = F)
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
                                                iter = input$integration_test_perm, print.progress = F)
            integ.test[[i]] <- integ.test.new
          }
          input$integration_group_level
          vals$IT <- integ.test[[as.numeric(isolate(input$integration_group_level))]]
        } else {
          for(i in 1:nlevels(groupings)){
            coords.subset <- gpa_coords_rx()[,,which(groupings == levels(groupings)[i])]
            integ.test.new <- integration.test(coords.subset, partition.gp = vals$modularity_groups, 
                                               iter = input$integration_test_perm, print.progress = F)
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
  
  output$export_integration_test_code  <- downloadHandler(
    filename = function() { paste('integration_test-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- expandChain(output$integration_test_plot())
      
      code <- c(
        general_notes(),
        prep_code(extra.libs = "ape"),
        edit_export_code(code),
        'summary(vals$IT)') # this chunk of export code is easily expanded to include results by simply adding this line 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "integration_test_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$symmetry_plot <- renderPlot(bg = "transparent", { 
    req(vals$bilat_symmetry)
    plot(vals$bilat_symmetry)
  })
  
  output$export_symmetry_plot <- downloadHandler(
    filename = function() { paste("symmetry_plot.pdf")},
    content = function(file) {
      pdf(file, width = 18, height = 13)
      req(vals$bilat_symmetry)
      plot(vals$bilat_symmetry)
      dev.off()
    }
  )
  
  output$export_symmetry_code <- downloadHandler(
    filename = function() { paste('symmetry-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- expandChain(bilat_metaO2())
      
      code <- c(
        general_notes(),
        prep_code(),
        'shinyalert <- function(title, text, html, size) { 
print(title) 
print(text) } # This function turns the pop up messages available in gmShiny into R console text. This will only appear if some sort of error is made.',
        edit_export_code(code),
        'plot(vals$bilat_symmetry)
summary(vals$bilat_symmetry)
vals$bilat_symmetry$signed.AI
vals$bilat_symmetry$unsigned.AI') 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      
      writeLines(as.character(code), file.path(temp_directory, "symmetry_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$export_symmetry_tables <- downloadHandler(
    filename = function() { paste("symmetry_anova_table.csv")},
    content = function(file) {
      req(vals$bilat_symmetry)
      sum <- summary(vals$bilat_symmetry)
      table <- sum$shape.anova
      rownames(table) <- NULL
      
      if(!is.null(sum$size.anova)) {
        table <-  rbind(NA, table, NA, NA, sum$size.anova)
        rownames(table)[1] <- "Shape ANOVA"
        rownames(table)[nrow(sum$shape.anova)+3] <- "Centroid Size ANOVA"
      }
      ind_tab <- cbind(c("Individual signed asymmetry index",
                         vals$bilat_symmetry$signed.AI), 
                       c("Individual unsigned asymmetry index",
                         vals$bilat_symmetry$unsigned.AI))
      ind_tab <- cbind(ind_tab, NA, NA, NA, NA, NA)
      colnames(ind_tab) <- colnames(table)
      row_counting <- nrow(table) + 4
      table <- rbind(table, NA, NA, ind_tab)
      rnames <- names(vals$bilat_symmetry$unsigned.AI)
      rnames <- paste("Individual_", rnames, sep = "")
      rownames(table)[(row_counting):(row_counting+nrow(ind_tab)-2)] <- rnames
      write.csv(table, file)
    }
  )
  
  output$symmetry_results <- renderPrint({
    req(vals$bilat_symmetry)
    summary(vals$bilat_symmetry)
    
    if(!is.null(vals$bilat_symmetry$signed.AI)){
      print("")
      print("Individual signed asymmetry index:")
      print(vals$bilat_symmetry$signed.AI)
    }
    if(!is.null(vals$bilat_symmetry$unsigned.AI)){
      print("")
      print("Individual unsigned asymmetry index:")
      print(vals$bilat_symmetry$unsigned.AI)
    }
  })
  
  output$symmetry_landpair_plot <- metaRender2(renderPlot, bg = "transparent", {
    req(gpa_coords_rx())
    gpa_coords <- gpa_coords_rx()
    vals$symmetry_land_pairs <- as.matrix(input$symmetry_landpairs_definitions)
    
    metaExpr({
      
      shape <- mshape(gpa_coords)
      
      class(shape) <- "array"
      
      symmetry_land_pairs <- vals$symmetry_land_pairs
      
      if(!is.null(symmetry_land_pairs)) {
        lm_col_vec <- rep('black', dim(gpa_coords)[1])
        lm_pch_vec <- rep('19', dim(gpa_coords)[1])
        if(nrow(symmetry_land_pairs) > 2) {
          ncolors <- floor(dim(gpa_coords)[1]/2)
          if(ncolors < 12) {
            col_spec <- c("red", brewer.pal(name = "Spectral", n = ncolors)[-1])
          } else { col_spec <- colorRampPalette(colors = c("red", "blue", "darkgreen", "grey60"), alpha = TRUE)(ncolors) }
          
          pch_spec <- c('15', '17')
          
          for(i in 1:nrow(symmetry_land_pairs)) {
            lm_col_vec[as.numeric(symmetry_land_pairs[i,])] <- col_spec[i]
          }
          for(i in 1:ncol(symmetry_land_pairs)){
            lm_pch_vec[as.numeric(symmetry_land_pairs[,i])] <- pch_spec[i]
          }
        } else {
          lm_col_vec[as.numeric(symmetry_land_pairs[1,])] <- "red"
          lm_pch_vec[as.numeric(symmetry_land_pairs[,1])] <- "15"
          lm_pch_vec[as.numeric(symmetry_land_pairs[,2])] <- "17"
        }
      } else { 
        lm_col_vec <- "black"
        lm_pch_vec <- '19'
      }
      
      plot(shape, col = lm_col_vec, pch = as.numeric(lm_pch_vec), cex = 2, asp = 1, axes = F, xlab = "", ylab = "") # displays the selected specimen landmark placement
      text(x = shape[,1]-0.01, y = shape[,2] - 0.01, labels = 1:(nrow(shape))) # adds labels to the landmarks
      if(!is.null(vals$links_df)){
        for (i in 1:nrow(vals$links_df)) {
          seg_df <- shape
          xys <- seg_df[vals$links_df[i,],]
          segments(x0 = xys[1], y0 = xys[3],x1 = xys[2], y1 = xys[4])
        }
      }
    })
  })
  
  output$export_symmetry_landpair_code <- downloadHandler(
    filename = function() { paste('symmetry_landpair-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- expandChain(output$symmetry_landpair_plot())
      
      code <- c(
        general_notes(),
        prep_code(extra.libs = c("RColorBrewer")),
        edit_export_code(code))
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "symmetry_landpair_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$export_symmetry_landpair_plot <- downloadHandler(
    filename = function() { paste("symmetry_landpairs_plot.pdf")},
    content = function(file) {
      pdf(file, width = 10, height = 6)
      req(gpa_coords_rx())
      req(input$symmetry_landpairs_definitions)
      
      shape <- mshape(gpa_coords_rx())
      class(shape) <- "array"
      
      symmetry_land_pairs <- as.matrix(input$symmetry_landpairs_definitions)
      
      if(!is.null(symmetry_land_pairs)) {
        lm_col_vec <- rep('black', dim(gpa_coords_rx())[1])
        lm_pch_vec <- rep('19', dim(gpa_coords_rx())[1])
        if(nrow(symmetry_land_pairs) > 2) {
          ncolors <- floor(dim(gpa_coords_rx())[1]/2) + 1
          if(ncolors < 12) {
            col_spec <- c("red", brewer.pal(name = "Spectral", n = ncolors)[-1])
          } else { col_spec <- colorRampPalette(colors = c("red", "blue", "darkgreen", "grey60"), alpha = TRUE)(ncolors) }
          
          pch_spec <- c('15', '17')
          
          for(i in 1:nrow(symmetry_land_pairs)) {
            lm_col_vec[as.numeric(symmetry_land_pairs[i,])] <- col_spec[i]
          }
          for(i in 1:ncol(symmetry_land_pairs)){
            lm_pch_vec[as.numeric(symmetry_land_pairs[,i])] <- pch_spec[i]
          }
        } else {
          lm_col_vec[as.numeric(symmetry_land_pairs[1,])] <- "red"
          lm_pch_vec[as.numeric(symmetry_land_pairs[,1])] <- "15"
          lm_pch_vec[as.numeric(symmetry_land_pairs[,2])] <- "17"
        }
      } else { 
        lm_col_vec <- "black"
        lm_pch_vec <- '19'
      }
      
      plot(shape, col = lm_col_vec, pch = as.numeric(lm_pch_vec), cex = 2, asp = 1, axes = F, xlab = "", ylab = "") # displays the selected specimen landmark placement
      text(x = shape[,1]-0.01, y = shape[,2] - 0.01, labels = 1:(nrow(shape))) # adds labels to the landmarks
      if(!is.null(vals$links_df)){
        for (i in 1:nrow(vals$links_df)) {
          seg_df <- shape
          xys <- seg_df[vals$links_df[i,],]
          segments(x0 = xys[1], y0 = xys[3],x1 = xys[2], y1 = xys[4])
        }
      }
      
      dev.off()
      
    }
  )
  
  output$phy_signal_results <- renderPrint({
    req(vals$PS_shape)
    summary(vals$PS_shape)
  })  
  
  output$export_symmetry_useoutput <- downloadHandler(
    filename = function() { paste("symmetric_shape_variation.tps")},
    content = function(file) {
      writeland.tps(vals$bilat_symmetry$symm.shape, file = file)
    }
  )
  
  output$export_asymmetry_useoutput <- downloadHandler(
    filename = function() { paste("asymmetric_shape_variation.tps")},
    content = function(file) {
      writeland.tps(vals$bilat_symmetry$asymm.shape, file = file)
    }
  )
  
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
  
  output$export_phy_signal_code <- downloadHandler(
    filename = function() { paste('phylogenetic_signal-', Sys.Date(), '.zip', sep = "") },
    content = function(file) {
      code <- expandChain(physig_meta0())
      code <- c(
        general_notes(),
        prep_code(),
        edit_export_code(code),
        "# The below plotting code is a simplified version of the code used in gmShiny. To edit the plot further, 
        # you can access and modify the internal geomorph code by entering 'geomorph:::plot.physignal' in the console.",
        'plot(vals$PS_shape)
summary(vals$PS_shape)') 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "phylogenetic_signal_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  #### Linear Models Outputs ####
  
  output$model_tested <- renderText({
    name <- "Model Tested: Shape ~ "
    vars <- NULL
    
    independent_variables <- input$independent_variables 
    
    if("by_trait_1" %in% independent_variables) {vars <- c(vars, substring(names(vals$trait_names)[1], 4, nchar(names(vals$trait_names)[1])))}
    if("by_trait_2" %in% independent_variables) {vars <- c(vars, substring(names(vals$trait_names)[2], 4, nchar(names(vals$trait_names)[2])))}
    if("by_trait_3" %in% independent_variables) {vars <- c(vars, substring(names(vals$trait_names)[3], 4, nchar(names(vals$trait_names)[3])))}
    if("csize" %in% independent_variables) {vars <- c(vars, "Centroid Size")}
    
    if(length(vars)>1) { 
      vars <- input$independent_variables_order 
    }
    
    name <- paste(name, vars[1], sep = "")
    if(length(vars)>1){
      for (i in 2:length(vars)){
        name <- paste(name, vars[i], sep = " + ")
      }
    }
    vals$mod_name <- name
    name
  })
  
  output$model_tested_2 <- renderText({ vals$mod_name })
  
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
  
  output$export_aov_table <- downloadHandler(
    filename = function() { paste('ANOVA_stats_table.csv') },
    content = function(file) { write.csv(vals$anova_table, file) }
  )
  
  output$export_aov_table_code <- downloadHandler(
    filename = function() { paste('aov_table-', Sys.Date(), '.zip', sep = "") },
    content = function(file){
      code <- expandChain(anova_table_metaO())
      
      code <- c(
        general_notes(),
        prep_code(add.source = T),
        edit_export_code(code),
        'vals$anova_table # ANOVA Table
vals$pairwise_table # Pairwise ANOVA table

if(exists("morphol_sum")){
  morphol_sum # Group Variance Results
  vals$morphol_disp_table
} 

if(exists("out")){
  out # Evolutionary Rate Results
  vals$evol_rates_pairwise
        }') 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "aov_table_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$stats_shape_trait_pairwise <- renderTable(rownames = T, na = "", digits = 4, {
    req(vals$pairwise_table)
    vals$pairwise_table
  })
  
  output$export_pairwise_table <- downloadHandler(
    filename = function() { paste('pairwise_stats_table.csv')},
    content = function(file) { write.csv(vals$pairwise_table, file) }
  )
  
  output$stats_morphol_disp_variances <- renderTable(rownames = T, digits = 8, {
    req(vals$proc_vars)
    matrix(vals$proc_vars, dimnames = list(names(vals$proc_vars), "Var"))
  })
  
  output$export_proc_variances <- downloadHandler( 
    filename = function() { paste('procrustes_group_variances.csv')},
    content = function(file) { write.csv(vals$proc_vars, file) }
  )
  
  output$stats_morphol_disp_pairwise <- renderTable(rownames = T, na = "", digits = 4, {
    req(vals$morphol_disp_table)
    vals$morphol_disp_table
  })
  
  output$export_disparity_pairwise_table <- downloadHandler(
    filename = function() { paste('pairwise_disparity_table.csv')},
    content = function(file) { write.csv(vals$morphol_disp_table, file) }
  )
  
  output$stats_evol_rates <- renderTable(rownames = T, digits = 8, {
    req(vals$evol_rates)
    matrix(vals$evol_rates, dimnames = list(names(vals$evol_rates), " "))
  })
  
  output$export_evol_rates <- downloadHandler(
    filename = function() { paste('evol_rates.csv')},
    content = function(file) { write.csv(vals$evol_rates, file) }
  )
  
  output$stats_evol_rates_pairwise <- renderTable(rownames = T, digits = 4, {
    req(vals$evol_rates_pairwise)
    vals$evol_rates_pairwise
  })
  
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
  
  tryObserve({
    if(input$allometry_color != "all_1_col" | input$show_allometry_convex_hull_1 | 
       input$show_allometry_convex_hull_2 | input$show_allometry_convex_hull_3) {
      color_options_all <- c(input$allom_color_1, input$allom_color_2, input$allom_color_3,
                             input$allom_color_4, input$allom_color_5, input$allom_color_6)
      i <- 0
      if(input$allometry_color == "by_trait_1") { i <- 1 }
      if(input$allometry_color == "by_trait_2") { i <- 2 }
      if(input$allometry_color == "by_trait_3") { i <- 3 }
      all_treatments <- c(input$trait_1_treatment, input$trait_2_treatment, input$trait_3_treatment)
      hulls <- (1:3)[c(input$show_allometry_convex_hull_1, 
                       input$show_allometry_convex_hull_2, 
                       input$show_allometry_convex_hull_3)] # the last part of this is a placeholder so that hull exists as an item for if 
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
      
      if(input$show_allometry_convex_hull_1 |
         input$show_allometry_convex_hull_2 | 
         input$show_allometry_convex_hull_3) {
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
          if(hulls[1] == i & hulls[2] != i) {
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
      
      vals$allometry_color_options_list <- list("tips" = tip_col_options, 
                                                "hull1" = hull_1_col_options, 
                                                "hull2" = hull_2_col_options, 
                                                "hull3" = hull_3_col_options)
      
    }
  })
  
  output$allometry_plot <- metaRender2(renderPlot, bg = "transparent", {
    req(input$allometry_predictor)
    gpa_coords <- gpa_coords_rx()
    metaExpr({
      if(!is.null(..(input$allometry_predictor))) {
        this_trait <- (2:6)[c(..(input$allometry_predictor) == "by_trait_1", 
                              ..(input$allometry_predictor) == "by_trait_2", 
                              ..(input$allometry_predictor) == "by_trait_3", 
                              ..(input$allometry_predictor) == "csize",
                              ..(input$allometry_predictor) == "none")]
        if(this_trait != 6) {
          if(this_trait == 5) { 
            if(!is.null(vals$csize)){
              if(..(input$allometry_log_csize)) {
                xlab_overwrite <- "Centroid Size (log transformed)"
                predictor <- log(vals$csize)
              } else { predictor <- vals$csize 
              xlab_overwrite <- "Centroid Size"}
              
            } 
          } else { 
            if(!is.null(vals$trait_rx)){
              predictor <- vals$trait_rx[,this_trait]
              xlab_overwrite <- colnames(vals$trait_rx)[this_trait]
            }
          }
          
          this_color <- (1:4)[c(..(input$allometry_color) == "all_1_col", 
                                ..(input$allometry_color) == "by_trait_1", 
                                ..(input$allometry_color) == "by_trait_2", 
                                ..(input$allometry_color) == "by_trait_3")]
          
          if(this_color == 1) { color <- rep("black", dim(gpa_coords)[3])} else {
            color <- as.character(vals$trait_rx[,this_color])
            levs <- unique(vals$trait_rx[,this_color])
            color[which(color == levs[1])] <- ..(input$allom_color_1)
            color[which(color == levs[2])] <- ..(input$allom_color_2)
            if(length(levs)>2){
              color[which(color == levs[3])] <- ..(input$allom_color_3)
              if(length(levs)>3){
                color[which(color == levs[4])] <- ..(input$allom_color_4)
                if(length(levs)>4){
                  color[which(color == levs[5])] <- ..(input$allom_color_5)
                  if(length(levs)>5){
                    color[which(color == levs[6])] <- ..(input$allom_color_6)
                  }
                }
              }
            }
          }
          
          out <- geomorph:::plot.procD.lm(vals$fit, type = ..(input$allometry_type), 
                                          reg.type = ..(input$allometry_reg_type), 
                                          predictor = predictor, 
                                          col = color, xlab = xlab_overwrite,
                                          pch = 19, cex = ..(input$allometry_pt_size) )
          
          if(..(input$show_allometry_tip_label)) { 
            if(input$allometry_predictor == "csize") {
              labels_overwrite <- names(out$plot.args$x)
            } else { 
              if(input$allometry_reg_type == "RegScore") { 
                labels_overwrite <- rownames(out$plot.args$y) 
              } else { labels_overwrite <- names(out$plot.args$y) }
            } # labels_overwrite are necessary because of the naming scheme of the plot.procD.lm output
            if(input$allometry_reg_type == "PredLine") { ys_for_text <- out$PredLine } else { 
              ys_for_text <- out$RegScore } 
            if(class(out$plot.args$x) == "character") { out$plot.args$x <- as.numeric(out$plot.args$x) } # necessary when predictor = continuous variable
            range_x <- max(out$plot.args$x)-min(out$plot.args$x)
            range_y <- max(ys_for_text)-min(ys_for_text)
            adj_x <- (..(input$allometry_pt_size)-1)*range_x/275
            adj_y <- (..(input$allometry_pt_size)-1)*range_y/275
            text(x = out$plot.args$x + adj_x, 
                 y = ys_for_text,
                 pos = 4, cex = sqrt(..(input$allometry_pt_size)),
                 labels = labels_overwrite) 
          }
          
          if(..(input$show_allometry_convex_hull_1) | 
             ..(input$show_allometry_convex_hull_2) | 
             ..(input$show_allometry_convex_hull_3)) {
            selected_trait <- (2:4)[c(..(input$show_allometry_convex_hull_1), 
                                      ..(input$show_allometry_convex_hull_2), 
                                      ..(input$show_allometry_convex_hull_3))]
            colored_tips <- (2:4)[c(..(input$allometry_color) == "by_trait_1",  
                                    ..(input$allometry_color) == "by_trait_2", 
                                    ..(input$allometry_color) == "by_trait_3")]
            
            col_list_modified <- list(vals$allometry_color_options_list$hull1, 
                                      vals$allometry_color_options_list$hull2, 
                                      vals$allometry_color_options_list$tips)
            
            if(length(colored_tips) == 0) {
              col_list_modified <-  list(vals$allometry_color_options_list$hull1, 
                                         vals$allometry_color_options_list$hull2, 
                                         vals$allometry_color_options_list$hull3)
            }
            if(length(colored_tips) > 0) { 
              if(colored_tips == selected_trait[1]) {
                col_list_modified <- list(vals$allometry_color_options_list$tips, 
                                          vals$allometry_color_options_list$hull1, 
                                          vals$allometry_color_options_list$hull2)
              } else {
                if(length(selected_trait) > 1) {
                  if(colored_tips == selected_trait[2]) { 
                    col_list_modified <- list(vals$allometry_color_options_list$hull1, 
                                              vals$allometry_color_options_list$tips, 
                                              vals$allometry_color_options_list$hull2 )
                  }
                }
              }
            }
            for (i in 1:length(selected_trait)) {
              this_col <- selected_trait[i]
              tip_col_fac <- as.character(vals$trait_rx[,this_col]) 
              lev <- length(unique(tip_col_fac))
              lev_options <- unique(tip_col_fac)
              col_mat <- unlist(col_list_modified[[i]]) 
              
              if(input$allometry_reg_type == "PredLine") {
                for(j in 1:length(unique(tip_col_fac))) {
                  selected_edges <- chull(x = out$plot.args$x[which(tip_col_fac == lev_options[j])], 
                                          y = out$PredLine[which(tip_col_fac == lev_options[j])])
                  polygon(x = out$plot.args$x[which(tip_col_fac == lev_options[j])][selected_edges], 
                          y = out$PredLine[which(tip_col_fac == lev_options[j])][selected_edges], 
                          col = adjustcolor(col_mat[j],alpha.f = .5), 
                          border = col_mat[j])
                }
              } else {
                for(j in 1:length(unique(tip_col_fac))) {
                  selected_edges <- chull(x = out$plot.args$x[which(tip_col_fac == lev_options[j])], 
                                          y = out$RegScore[which(tip_col_fac == lev_options[j])])
                  polygon(x = out$plot.args$x[which(tip_col_fac == lev_options[j])][selected_edges],
                          y = out$RegScore[which(tip_col_fac == lev_options[j])][selected_edges], 
                          col = adjustcolor(col_mat[j],alpha.f = .5), 
                          border = col_mat[j])
                }
              } 
            }
          }
        }
      }
    })
  })
  
  output$allometry_lev_1 <- renderText({ 
    if(length(vals$allometry_color_lev_names)>0) { vals$allometry_color_lev_names[1] } else { NULL }
  })
  
  output$allometry_lev_2 <- renderText({ 
    if(length(vals$allometry_color_lev_names)>1) { vals$allometry_color_lev_names[2] } else { NULL }
  })
  
  output$allometry_lev_3 <- renderText({ 
    if(length(vals$allometry_color_lev_names)>2) { vals$allometry_color_lev_names[3] } else { NULL }
  })
  
  output$allometry_lev_4 <- renderText({ 
    if(length(vals$allometry_color_lev_names)>3) { vals$allometry_color_lev_names[4] } else { NULL }
  })
  
  output$allometry_lev_5 <- renderText({ 
    if(length(vals$allometry_color_lev_names)>4) { vals$allometry_color_lev_names[5] } else { NULL }
  })
  
  output$allometry_lev_6 <- renderText({ 
    if(length(vals$allometry_color_lev_names)>5) { vals$allometry_color_lev_names[6] } else { NULL }
  })
  
  output$export_allometry_plot <- downloadHandler(
    filename = function() { paste("allometry_plot.pdf") },
    content = function(file) { 
      if(!is.null(input$allometry_predictor)) {
        pdf(file, width = 10, height = 6, pointsize = 8)
        req(vals$fit) 

        gpa_coords <- gpa_coords_rx()

          if(!is.null(input$allometry_predictor)) {
            this_trait <- (2:6)[c(input$allometry_predictor == "by_trait_1", 
                                  input$allometry_predictor == "by_trait_2", 
                                  input$allometry_predictor == "by_trait_3", 
                                  input$allometry_predictor == "csize",
                                  input$allometry_predictor == "none")]
            if(this_trait != 6) {
              if(this_trait == 5) { 
                if(!is.null(vals$csize)){
                  if(input$allometry_log_csize) {
                    xlab_overwrite <- "Centroid Size (log transformed)"
                    predictor <- log(vals$csize)
                  } else { predictor <- vals$csize 
                  xlab_overwrite <- "Centroid Size"}
                  
                } 
              } else { 
                if(!is.null(vals$trait_rx)){
                  predictor <- vals$trait_rx[,this_trait]
                  xlab_overwrite <- colnames(vals$trait_rx)[this_trait]
                }
              }
              
              this_color <- (1:4)[c(input$allometry_color == "all_1_col", 
                                    input$allometry_color == "by_trait_1", 
                                    input$allometry_color == "by_trait_2", 
                                    input$allometry_color == "by_trait_3")]
              
              if(this_color == 1) { color <- rep("black", dim(gpa_coords)[3])} else {
                color <- as.character(vals$trait_rx[,this_color])
                levs <- unique(vals$trait_rx[,this_color])
                color[which(color == levs[1])] <- input$allom_color_1
                color[which(color == levs[2])] <- input$allom_color_2
                if(length(levs)>2){
                  color[which(color == levs[3])] <- input$allom_color_3
                  if(length(levs)>3){
                    color[which(color == levs[4])] <- input$allom_color_4
                    if(length(levs)>4){
                      color[which(color == levs[5])] <- input$allom_color_5
                      if(length(levs)>5){
                        color[which(color == levs[6])] <- input$allom_color_6
                      }
                    }
                  }
                }
              }
              
              out <- geomorph:::plot.procD.lm(vals$fit, type = input$allometry_type, 
                                              reg.type = input$allometry_reg_type, 
                                              predictor = predictor, 
                                              col = color, xlab = xlab_overwrite,
                                              pch = 19, cex = input$allometry_pt_size )
              
              if(input$show_allometry_tip_label) { 
                if(input$allometry_predictor == "csize") {
                  labels_overwrite <- names(out$plot.args$x)
                } else { 
                  if(input$allometry_reg_type == "RegScore") { 
                    labels_overwrite <- rownames(out$plot.args$y) 
                  } else { labels_overwrite <- names(out$plot.args$y) }
                } # labels_overwrite are necessary because of the naming scheme of the plot.procD.lm output
                if(input$allometry_reg_type == "PredLine") { ys_for_text <- out$PredLine } else { 
                  ys_for_text <- out$RegScore } 
                if(class(out$plot.args$x) == "character") { out$plot.args$x <- as.numeric(out$plot.args$x) } # necessary when predictor = continuous variable
                range_x <- max(out$plot.args$x)-min(out$plot.args$x)
                range_y <- max(ys_for_text)-min(ys_for_text)
                adj_x <- (input$allometry_pt_size-1)*range_x/275
                adj_y <- (input$allometry_pt_size-1)*range_y/275
                text(x = out$plot.args$x + adj_x, 
                     y = ys_for_text,
                     pos = 4, cex = sqrt(input$allometry_pt_size),
                     labels = labels_overwrite) 
              }
              
              if(input$show_allometry_convex_hull_1 | 
                 input$show_allometry_convex_hull_2 | 
                 input$show_allometry_convex_hull_3) {
                selected_trait <- (2:4)[c(input$show_allometry_convex_hull_1, 
                                          input$show_allometry_convex_hull_2, 
                                          input$show_allometry_convex_hull_3)]
                colored_tips <- (2:4)[c(input$allometry_color == "by_trait_1",  
                                        input$allometry_color == "by_trait_2", 
                                        input$allometry_color == "by_trait_3")]
                
                col_list_modified <- list(vals$allometry_color_options_list$hull1, 
                                          vals$allometry_color_options_list$hull2, 
                                          vals$allometry_color_options_list$tips)
                
                if(length(colored_tips) == 0) {
                  col_list_modified <-  list(vals$allometry_color_options_list$hull1, 
                                             vals$allometry_color_options_list$hull2, 
                                             vals$allometry_color_options_list$hull3)
                }
                if(length(colored_tips) > 0) { 
                  if(colored_tips == selected_trait[1]) {
                    col_list_modified <- list(vals$allometry_color_options_list$tips, 
                                              vals$allometry_color_options_list$hull1, 
                                              vals$allometry_color_options_list$hull2)
                  } else {
                    if(length(selected_trait) > 1) {
                      if(colored_tips == selected_trait[2]) { 
                        col_list_modified <- list(vals$allometry_color_options_list$hull1, 
                                                  vals$allometry_color_options_list$tips, 
                                                  vals$allometry_color_options_list$hull2 )
                      }
                    }
                  }
                }
                for (i in 1:length(selected_trait)) {
                  this_col <- selected_trait[i]
                  tip_col_fac <- as.character(vals$trait_rx[,this_col]) 
                  lev <- length(unique(tip_col_fac))
                  lev_options <- unique(tip_col_fac)
                  col_mat <- unlist(col_list_modified[[i]]) 
                  
                  if(input$allometry_reg_type == "PredLine") {
                    for(j in 1:length(unique(tip_col_fac))) {
                      selected_edges <- chull(x = out$plot.args$x[which(tip_col_fac == lev_options[j])], 
                                              y = out$PredLine[which(tip_col_fac == lev_options[j])])
                      polygon(x = out$plot.args$x[which(tip_col_fac == lev_options[j])][selected_edges], 
                              y = out$PredLine[which(tip_col_fac == lev_options[j])][selected_edges], 
                              col = adjustcolor(col_mat[j],alpha.f = .5), 
                              border = col_mat[j])
                    }
                  } else {
                    for(j in 1:length(unique(tip_col_fac))) {
                      selected_edges <- chull(x = out$plot.args$x[which(tip_col_fac == lev_options[j])], 
                                              y = out$RegScore[which(tip_col_fac == lev_options[j])])
                      polygon(x = out$plot.args$x[which(tip_col_fac == lev_options[j])][selected_edges],
                              y = out$RegScore[which(tip_col_fac == lev_options[j])][selected_edges], 
                              col = adjustcolor(col_mat[j],alpha.f = .5), 
                              border = col_mat[j])
                    }
                  }
                }
              }
            }
          }
        dev.off()
      }
    }
  )
  
  output$export_allometry_code <- downloadHandler(
    filename = function() { paste('allometry-', Sys.Date(), '.zip', sep = "") },
    content = function(file) {
      code <- expandChain(output$allometry_plot())
      code <- c(
        general_notes(),
        "# Recall that the allometry analyses depend on the Model designed and tested in the Model Design tab of the Linear Models page.
# See code exported from that tab to understand how vals$fit was calculated.",
        prep_code(),
        edit_export_code(code)) 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "allometry_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$model_comparison_model_1 <- renderText({ # copied from model_tested
    name <- "Model 1: Shape ~ "
    vars <- NULL
    
    independent_variables_model_1 <- input$independent_variables_model_1 
    
    if("by_trait_1" %in% independent_variables_model_1) {vars <- c(vars, substring(names(vals$trait_names)[1], 4, nchar(names(vals$trait_names)[1])))}
    if("by_trait_2" %in% independent_variables_model_1) {vars <- c(vars, substring(names(vals$trait_names)[2], 4, nchar(names(vals$trait_names)[2])))}
    if("by_trait_3" %in% independent_variables_model_1) {vars <- c(vars, substring(names(vals$trait_names)[3], 4, nchar(names(vals$trait_names)[3])))}
    if("csize" %in% independent_variables_model_1) {vars <- c(vars, "Centroid Size")}
    
    if(length(vars)>1) { vars <- input$independent_variables_order_model_1  }
    
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
    
    independent_variables_model_2 <- input$independent_variables_model_2 
    
    if("by_trait_1" %in% independent_variables_model_2) {vars <- c(vars, substring(names(vals$trait_names)[1], 4, nchar(names(vals$trait_names)[1])))}
    if("by_trait_2" %in% independent_variables_model_2) {vars <- c(vars, substring(names(vals$trait_names)[2], 4, nchar(names(vals$trait_names)[2])))}
    if("by_trait_3" %in% independent_variables_model_2) {vars <- c(vars, substring(names(vals$trait_names)[3], 4, nchar(names(vals$trait_names)[3])))}
    if("csize" %in% independent_variables_model_2) {vars <- c(vars, "Centroid Size")}
    
    if(length(vars)>1) {  vars <- input$independent_variables_order_model_2 }
    
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
      
      independent_variables_model_3 <- input$independent_variables_model_3 
      
      if("by_trait_1" %in% independent_variables_model_3) {vars <- c(vars, substring(names(vals$trait_names)[1], 4, nchar(names(vals$trait_names)[1])))}
      if("by_trait_2" %in% independent_variables_model_3) {vars <- c(vars, substring(names(vals$trait_names)[2], 4, nchar(names(vals$trait_names)[2])))}
      if("by_trait_3" %in% independent_variables_model_3) {vars <- c(vars, substring(names(vals$trait_names)[3], 4, nchar(names(vals$trait_names)[3])))}
      if("csize" %in% independent_variables_model_3) {vars <- c(vars, "Centroid Size")}
      
      if(length(vars)>1) { 
        vars <- input$independent_variables_order_model_3
        
      }
      
      name <- paste(name, vars[1], sep = "")
      if(length(vars)>1){
        for (i in 2:length(vars)){
          name <- paste(name, vars[i], sep = " + ")
        }
      }
      name
    }
  })
  
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
  
  output$export_model_comparison_code <- downloadHandler(
    filename = function() { paste('model_comparison-', Sys.Date(), '.zip', sep = "") },
    content = function(file) {
      code <- expandChain(model_comparison_metaO())
      code <- c(
        general_notes(),
        "# Recall that the allometry analyses depend on the Model designed and tested in the Model Design tab of the Linear Models page.
# See code exported from that tab to understand how vals$fit was calculated.",
        prep_code(add.source = T),
        edit_export_code(code),
        'vals$model_comparison') 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "model_comparison_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$model_trajectory <- renderText({
    req(vals$trait_rx)
    
    if(input$trajectory_group[1] != "Trait 1") {
      name <- "Model Tested: Shape ~ "
      vars <- NULL
      if("by_trait_1" %in% input$trajectory_independent_var) {vars_ind <- substring(names(vals$trait_names)[1], 4, nchar(names(vals$trait_names)[1]))}
      if("by_trait_2" %in% input$trajectory_independent_var) {vars_ind <- substring(names(vals$trait_names)[2], 4, nchar(names(vals$trait_names)[2]))}
      if("by_trait_3" %in% input$trajectory_independent_var) {vars_ind <- substring(names(vals$trait_names)[3], 4, nchar(names(vals$trait_names)[3]))}
      if(input$trajectory_independent_var == "csize") { vars_ind <- "Centroid Size" }
      if(input$trajectory_independent_var == "none1") {vars_ind <- NULL }
      if(!is.null(vars_ind)) name <- paste(name, vars_ind,  " + ", sep = "")
      
      if("by_trait_1" %in% input$trajectory_group) {vars <- c(vars, substring(names(vals$trait_names)[1], 4, nchar(names(vals$trait_names)[1])))}
      if("by_trait_2" %in% input$trajectory_group) {vars <- c(vars, substring(names(vals$trait_names)[2], 4, nchar(names(vals$trait_names)[2])))}
      if("by_trait_3" %in% input$trajectory_group) {vars <- c(vars, substring(names(vals$trait_names)[3], 4, nchar(names(vals$trait_names)[3])))}
      if("by_trait_1" %in% input$trajectory_trait) {vars <- c(vars, substring(names(vals$trait_names)[1], 4, nchar(names(vals$trait_names)[1])))}
      if("by_trait_2" %in% input$trajectory_trait) {vars <- c(vars, substring(names(vals$trait_names)[2], 4, nchar(names(vals$trait_names)[2])))}
      if("by_trait_3" %in% input$trajectory_trait) {vars <- c(vars, substring(names(vals$trait_names)[3], 4, nchar(names(vals$trait_names)[3])))}
      
      name <- paste(name, vars[1], sep = "")
      if(length(vars)>1){
        for (i in 2:length(vars)){
          name <- paste(name, vars[i], sep = " * ")
        }
      }
      name
    }
  })
  
  output$traj_trait_level_1 <- renderText({
    req(vals$traj_trait_levels)
    vals$traj_trait_levels[1]
  })
  
  output$traj_trait_level_2 <- renderText({
    req(vals$traj_trait_levels)
    vals$traj_trait_levels[2]
  })
  
  output$traj_trait_level_3 <- renderText({
    req(vals$traj_trait_levels)
    if(length(vals$traj_trait_levels) > 2) vals$traj_trait_levels[3]
  })
  
  output$traj_trait_level_4 <- renderText({
    req(vals$traj_trait_levels)
    if(length(vals$traj_trait_levels) > 3) vals$traj_trait_levels[4]
  })
  
  output$traj_trait_level_5 <- renderText({
    req(vals$traj_trait_levels)
    if(length(vals$traj_trait_levels) > 4) vals$traj_trait_levels[5]
  })
  
  output$traj_trait_level_6 <- renderText({
    req(vals$traj_trait_levels)
    if(length(vals$traj_trait_levels) > 5) vals$traj_trait_levels[6]
  })
  
  output$trajectory_results <- renderPrint({
    req(vals$trajectory_fit)
    sum <- summary(vals$TA, attribute = input$trajectory_attribute, show.trajectories = T)
    sum
  })
  
  output$export_trajectory_results <- downloadHandler(
    filename = function() { paste("trajectory.csv")},
    content = function(file) { 
      req(vals$trajectory_fit)
      sum <- summary(vals$TA, attribute = input$trajectory_attribute, show.trajectories = T)
      out <- sum$summary.table
      out <- rbind(rep(NA, ncol(out)), colnames(out), out)
      colnames(out) <- NULL
      out.obs <- vals$TA$PD$obs
      if(length(out.obs)<ncol(out)) { out.obs <- c(out.obs, rep(NA, ncol(out)-length(out.obs))) }
      if(length(out.obs)>ncol(out)) { out <- cbind(out, matrix(NA, ncol = length(out.obs)-ncol(out), nrow = nrow(out)))}
      out <- rbind(out.obs, out)
      colnames(out)[1:length(vals$TA$PD$obs)] <- names(vals$TA$PD$obs)
      write.csv(out, file)
    }
  )
  
  output$trajectory_plot <- metaRender2(renderPlot, bg = "transparent", {
    req(vals$TA)
    
    this_group <- (1:4)[c(isolate(input$trajectory_group) == "noneg",
                          isolate(input$trajectory_group) == "by_trait_1", 
                          isolate(input$trajectory_group) == "by_trait_2", 
                          isolate(input$trajectory_group) == "by_trait_3")]
    if(this_group != 1) group <- as.factor(isolate(vals$trait_rx)[,this_group])
    this_trait <- (1:4)[c(isolate(input$trajectory_trait) == "nonet",
                          isolate(input$trajectory_trait) == "by_trait_1", 
                          isolate(input$trajectory_trait) == "by_trait_2", 
                          isolate(input$trajectory_trait) == "by_trait_3")]
    if(this_trait != 1) {trait <- as.factor(isolate(vals$trait_rx)[,this_trait])} else {trait <- NULL}
    if(!is.null(isolate(vals$go_example_1))) { # if using the example dataset, you have to adjust the 3 level trait so that there are repeats in all 4 'levels'
      if(!is.null(group)){
        if(nlevels(group) > 2) { 
          group[which(group == levels(group)[2])] <- levels(group)[3] # condensing levels for purposes of example
          group <- droplevels(group)
        }
      }
      if(!is.null(trait)){
        if(nlevels(trait) > 2) { 
          trait[which(trait == levels(trait)[2])] <- levels(trait)[3] # condensing levels for purposes of example
          trait <- droplevels(trait)
        }
      }
    }
    
    metaExpr({
      if(!is.null(group)){
        if(nlevels(group) < 8) { 
          gs <- c(21:25, 8, 7)[1:nlevels(group)]
        } else { gs <- c(21:25, 8, 7, rep(1, nlevels(group) - 7))[1:nlevels(group)] } # all higher levels than 8 get pch = 1}
        pchs <- as.character(group)
        for(i in 1:nlevels(group)){ 
          pchs[which(pchs == levels(group)[i])] <- gs[i]}
      } else gs <- NULL
      
      if(!is.null(trait)){
        tr_col_vec <- c(..(input$traj_trait_1_col), ..(input$traj_trait_2_col), ..(input$traj_trait_3_col),
                        ..(input$traj_trait_4_col), ..(input$traj_trait_5_col), ..(input$traj_trait_6_col), "black")
        bgs <- as.numeric(trait)
        bgs[which(bgs == 1)] <- tr_col_vec[1]
        bgs[which(bgs == 2)] <- tr_col_vec[2]
        bgs[which(bgs == 3)] <- tr_col_vec[3]
        bgs[which(bgs == 4)] <- tr_col_vec[4]
        bgs[which(bgs == 5)] <- tr_col_vec[5]
        bgs[which(bgs == 6)] <- tr_col_vec[6]
        bgs[which(!(bgs %in% tr_col_vec))] <- tr_col_vec[7] # putting all other trait levels as black
      } else bgs <- NULL
      
      TP <- plot(vals$TA, pch = as.numeric(pchs), bg = bgs, cex = ..(input$trajectory_specimen_cex), col = bgs)
      add.trajectories.ekb(TP, 
                           traj.pch = gs, 
                           traj.gp.bg = tr_col_vec[1:nlevels(trait)], 
                           traj.cex = ..(input$trajectory_traj_cex))
      
      leg_cols <- c(rep("black", nlevels(group)), tr_col_vec[1:nlevels(trait)])
      legend("topright", c(levels(group), levels(trait)), pch = c(gs, rep(20, nlevels(trait))), 
             col = leg_cols, pt.bg = leg_cols,  cex = ..(input$trajectory_traj_cex)*(2/3))
    })
  })
  
  output$export_trajectory_plot <- downloadHandler(
    filename = function() { paste("trajectory.pdf")},
    content = function(file) { 
      pdf(file, width = 10, height = 6, pointsize = 8)
      req(vals$TA)
      this_group <- (1:4)[c(isolate(input$trajectory_group) == "noneg",
                            isolate(input$trajectory_group) == "by_trait_1", 
                            isolate(input$trajectory_group) == "by_trait_2", 
                            isolate(input$trajectory_group) == "by_trait_3")]
      if(this_group != 1) group <- as.factor(isolate(vals$trait_rx)[,this_group])
      this_trait <- (1:4)[c(isolate(input$trajectory_trait) == "nonet",
                            isolate(input$trajectory_trait) == "by_trait_1", 
                            isolate(input$trajectory_trait) == "by_trait_2", 
                            isolate(input$trajectory_trait) == "by_trait_3")]
      if(this_trait != 1) trait <- as.factor(isolate(vals$trait_rx)[,this_trait])
      if(!is.null(isolate(vals$go_example_1))) { # if using the example dataset, you have to adjust the 3 level trait so that there are repeats in all 4 'levels'
        if(!is.null(group)){
          if(nlevels(group) > 2) { 
            group[which(group == levels(group)[2])] <- levels(group)[3] # condensing levels for purposes of example
            group <- droplevels(group)
          }
        }
        if(!is.null(trait)){
          if(nlevels(trait) > 2) { 
            trait[which(trait == levels(trait)[2])] <- levels(trait)[3] # condensing levels for purposes of example
            trait <- droplevels(trait)
          }
        }
      }
      
      if(!is.null(group)){
        if(nlevels(group) < 8) { 
          gs <- c(21:25, 8, 7)[1:nlevels(group)]
        } else { gs <- c(21:25, 8, 7, rep(1, nlevels(group) - 7))[1:nlevels(group)] }# all higher levels than 8 get pch = 1}
      } else gs <- NULL
      
      if(!is.null(trait)){
        tr_col_vec <- c(input$traj_trait_1_col, input$traj_trait_2_col, input$traj_trait_3_col,
                        input$traj_trait_4_col, input$traj_trait_5_col, input$traj_trait_6_col, "black")
        bgs <- as.numeric(trait)
        bgs[which(bgs == 1)] <- tr_col_vec[1]
        bgs[which(bgs == 2)] <- tr_col_vec[2]
        bgs[which(bgs == 3)] <- tr_col_vec[3]
        bgs[which(bgs == 4)] <- tr_col_vec[4]
        bgs[which(bgs == 5)] <- tr_col_vec[5]
        bgs[which(bgs == 6)] <- tr_col_vec[6]
        bgs[which(!(bgs %in% tr_col_vec))] <- tr_col_vec[7] # putting all other trait levels as black
      } else bgs <- NULL
      
      TP <- plot(vals$TA, pch = gs, bg = bgs, cex = input$trajectory_specimen_cex, col = bgs)
      add.trajectories.ekb(TP, 
                           traj.pch = gs, 
                           traj.gp.bg = tr_col_vec[1:nlevels(trait)], 
                           traj.cex = input$trajectory_traj_cex)
      
      leg_cols <- c(rep("black", nlevels(group)), tr_col_vec[1:nlevels(trait)])
      legend("topright", c(levels(group), levels(trait)), pch = c(gs, rep(20, nlevels(trait))), 
             col = leg_cols, pt.bg = leg_cols,  cex = input$trajectory_traj_cex*(2/3))
      dev.off()
    }
  )
  
  output$export_trajectory_code <- downloadHandler(
    filename = function() { paste('trajectory-', Sys.Date(), '.zip', sep = "") },
    content = function(file) {
      code <- expandChain(trajectory_metaO())
      code_2 <- expandChain(output$trajectory_plot())
      code <- c(
        general_notes(),
        prep_code(add.source = T),
        'shinyalert <- function(title, text, html, size) { 
print(title) 
print(text) } # This function turns the pop up messages available in gmShiny into R console text. This will only appear if some sort of error is made.',
        edit_export_code(code),
        edit_export_code(code_2),
        paste('summary(vals$TA, attribute = "', as.character(input$trajectory_attribute), '", show.trajectories = T)', sep = "")) 
      
      ### copied from output$export_data_current_state
      phy_rx <- phy_rx() 
      if(!is.null(vals$go_run_gpa)) {
        gpa_coords <- gpa_coords_rx()
      } else {  gpa_coords <- vals$lms_rx}
      trait_rx <- trait_rx() 
      
      pca_rx <- pca_rx() 
      ref_rx <- ref_rx()
      if(!is.null(vals$warp_initiated)){ targ_rx <- targ_rx()  } else { targ_rx <- NULL } # necessary to protect a failed download
      gridPar_vals <- gridPar_vals()
      
      if(is.null(vals$go_example_1)) {tpsnts_file <- input$file_tps } else { tpsnts_file <- "plethspecies$land"}
      
      if(input$shape_file_type == "TPSorNTS") {
        shape_input_args <- list("file" = tpsnts_file,
                                 "negNA" = input$neg_lms, "specID"= input$spec_id, 
                                 "bypass_gpa_TF" = input$raw_lms_already_aligned)
      } 
      if(input$shape_file_type == "StereoMorph") {
        shape_input_args <- list("nCurvePts" = vals$curve_n_vec)
      } 
      
      vals2 <- list()
      for(i in names(vals)) vals2[[i]] <- vals[[i]]
      
      
      # creating a temporary directory to write things in
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      # writing both the RData file and the code file in this temp directory
      save(gpa_coords, phy_rx, trait_rx, 
           pca_rx,  
           ref_rx, 
           targ_rx,
           gridPar_vals,
           shape_input_args, vals2,
           file = file.path(temp_directory, "data_current_state.RData")
      )
      writeLines(as.character(code), file.path(temp_directory, "trajectory_code.R"))
      file.copy("support.functions.R", file.path(temp_directory, "support.functions.R"), overwrite = T)
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  #### Extras Outputs ####
  
  output$news <- renderUI({
    HTML("
    <li> 03.07.2023 - v0.1.4 release </li>
    <li> 06.13.2022 - v0.1.3 release </li>
    <li> 05.13.2022 - v0.1.2 release </li>
    <li> 05.11.2022 - v0.1.1 release </li>
    <li> 02.17.2022 - v0.1.0 release </li>
    <li> 09.17.2021 - gmShiny v0.0.1 launch! </li>")
  })
  
  output$upcoming_features <- renderUI({
    HTML("<p>Here is what we're working on adding to the app: <br>
      <li> Expanding server capacity and server idle time </li>
      <li> Add options for various methods of missing landmark estimation </li>
      <li> Click-and-drag ability to define modules </li>
      <li> Allowing new upload of trait data to test against symmetrical data once the symmetric or asymmetric component has replaced the original shape data </li>
      <li> Including interaction of terms for linear models </li>
      <li> Landmark rotation </li>
    </p>")
  })
  
  output$server_capacity <- renderUI({
    HTML("<p>Server specifications:<br>
    <li> Limit of 100 concurrent users</li>
    <li> Session will automatically terminate after 60 idle minutes</li>
    <li> App does not accept files greater than 10 MB</li>
    <li> 2D TPS data on 2,000+ specimens, 3D TPS data on 500+ specimens, and 
         Stereomorph files of 300+ specimens will cause substantial slow-downs in processsing. 
         Use of geomorph directly in R or R Studio is recommended in these cases.</li>
         </p>")
  })
  
  output$citation_info <- renderText({
    "Baken, EK, ML Collyer, A Kaliontzopoulou, and DC Adams. 2021. gmShiny and geomorph 4.0: new graphical interface and enhanced analytics for a comprehensive morphometric experience. Methods in Ecology and Evolution 12(12):2355-2363."
  })
  
  output$github_link <- renderText({
    "This GitHub repository houses the source code for this App: www.github.com/geomorphR/gmShiny/"
  })
  
  output$export_demo_tps <- downloadHandler(
    filename = function() { paste('land.tps') },
    content = function(file) {
      file.copy("demo_files/land-usable.tps", file)
    })
  
  output$export_demo_phy <- downloadHandler(
    filename = function() { paste('phy.tre') },
    content = function(file) {
      file.copy("demo_files/phy-usable.tre", file)
    })
  
  output$export_demo_trait <- downloadHandler(
    filename = function() { paste('trait.csv') },
    content = function(file) {
      file.copy("demo_files/trait.csv", file)
    })
  
  output$export_demo_symmetry <- downloadHandler(
    filename = function() { paste('sym.zip') },
    content = function(file) {
      file.copy("demo_files/sym.zip", file)
    },
    contentType = "application/zip"
  ) 
  
  #### End ####
  
  #session$onSessionEnded(stopApp) 
}

