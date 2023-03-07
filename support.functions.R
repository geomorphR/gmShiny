# Support Functions

# keeping the app from crashing with errors
tryObserve <- function(x, priority=0, suspended = F) {
  x <- substitute(x)
  env <- parent.frame()
  observe(priority = priority, suspended = suspended, {
    tryCatch(
      eval(x, env),
      error = function(e) {
        if(nchar(e$message) > 0){
          call_start <- capture.output(e$call)
          showNotification(paste("Error: ", e$message,"; Call began: ", call_start[1], sep = ""), type = "error", duration = NULL) # shinyalert(title = "Error",
        }
      }
    )
  })
}

tryMetaObserve2 <- function(x, priority=0, suspended = F) {
  x <- substitute(x)
  env <- parent.frame()
  metaObserve2({ # priority = priority, suspended = suspended, 
    tryCatch(
      eval(x, env),
      error = function(e) {
        if(nchar(e$message) > 0){
          call_start <- capture.output(e$call)
          showNotification(paste("Error: ", e$message,"; Call began: ", call_start[1], sep = ""), type = "error", duration = NULL) # shinyalert(title = "Error",
        }
      }
    )
  })
}

tryObserveEvent <- function(eventExpr,  handlerExpr, ignoreInit = F, priority=0) {
  eventExpr <- substitute(eventExpr)
  y <- substitute(handlerExpr)
  
  env <- parent.frame()
  observeEvent(ignoreInit = ignoreInit, priority = priority, eventExpr= tryCatch(
    eval(eventExpr, env),
    error = function(e) {
      if(nchar(e$message) > 0){
        call_start <- capture.output(e$call)
        showNotification(paste("Error in Event Expression: ", e$message,"; Call began: ", call_start[1], sep = ""), type = "error", duration = NULL)
      }
    }
  ),
  {
    tryCatch(
      eval(y, env),
      error = function(e) {
        if(nchar(e$message) > 0){
          call_start <- capture.output(e$call)
          showNotification(paste("Error: ", e$message, "; Call began: ", call_start[1], sep = ""), type = "error", duration = NULL) # shinyalert(title = "Error",
        }
      }
    )
  })
}

# export code helper functions



general_notes <- function(x) {
return("# Notes:
# Before running any of the below code, set your working directory to wherever the .RData file exists. Without having moved it yourself, 
# this will be in the unzipped folder. The function 'setwd()' can be used for this step. 
       
# The below code includes 'if else' statements that might appear unnecessary (e.g., 'if(TRUE){}').
# These are the result of gmShiny pulling information from settings and other dependencies within the app to
# fill in T/F statements as is appropriate for your data and current settings.
"
       )
}

prep_code <- function(add.source = F, extra.libs = NULL){
  if(add.source) {
    output <- 'rm(list = ls())
load("data_current_state.RData")
source("support.functions.R") 
vals <- vals2
library(geomorph)'
  } else {
  output <- 'rm(list = ls())
load("data_current_state.RData") 
vals <- vals2
library(geomorph)'
  }
  if(!is.null(extra.libs)) {
    for(i in 1:length(extra.libs)) {
    output <- c(output, paste('library(', extra.libs[i], ")", sep = ""))
    }
  }
  return(output)
}

edit_export_code <- function(code) {
  code_as_vector <- strsplit(as.character(code), "isolate")
  delete_this_line <- NULL
  for (i in 1:length(code_as_vector)) {
    if(length(code_as_vector[[i]]) > 1) { # these parts of the code were chopped up based on the function 'isolate'. the following lines of code work to delete that function from the output code.
      for(j in 2:length(code_as_vector[[i]])){
        code_as_vector[[i]][j] <- str_replace(code_as_vector[[i]][j], "[(]", "") # this takes out the first forward parenthesis
        code_as_vector[[i]][j] <- str_replace(code_as_vector[[i]][j], "[)]", "") # this takes out the first backward parenthesis
      }
      code_as_vector[[i]] <- paste(code_as_vector[[i]], collapse = "")
    }
    # the following lines find code where the word 'else' is put on a new line, which causes errors in the exported code
    # it then pastes that line back to the previous line and marks it for future deletion
    temp_string <- str_replace_all(code_as_vector[[i]], " ", "")
    start_char <- substr(temp_string, 1, 4)
    if(start_char == "else"){
      code_as_vector[[i-1]] <- paste(code_as_vector[[i-1]], code_as_vector[[i]], collapse = " ")
      delete_this_line <- c(delete_this_line, i)
    }
  }
  if(is.null(delete_this_line)) {code <- unlist(code_as_vector) } else {
    code <- unlist(code_as_vector)[-delete_this_line] # deleting the now redundant else lines
  }
  return(code)
}

# Random data generation for pleth example data
#rand_trait_func_numb <- function(size) { # set it here
#  #dd <- c(1,1,1,2,2,2,3,3,3)
#  #return(dd)
#  success <- "start_point"
#  while(length(success) > 0){
#    temp_trait <- as.factor(sample(1:3, size = size, replace = T))
#    success <- which(table(temp_trait)<2)}
#  return(temp_trait)
#} # this function helps guarantee that the sample dataset generated for the Plethodon Example dataset has at least 2 observations at each level.
#
#rand_trait_func_lett <- function(size) {
#  #dd <- c("A", "B", "C", "A", "B", "C","A", "B", "C")
#  #return(dd)
#  success <- "start_point"
#  while(length(success) > 0){
#    temp_trait <- as.factor(sample(c("A", "B", "C"), size = size, replace = T))
#    success <- which(table(temp_trait)<2)}
#  return(temp_trait)
#}

# plotOutlier edited

plotOutliers.ekb1 <- function (A, groups = NULL, inspect.outliers = FALSE) 
{
  if (length(dim(A)) != 3) {
    stop("Data matrix not a 3D array (see 'arrayspecs').")
  }
  if (is.null(groups)) {
    groups = factor(rep("All Specimens", dim(A)[3]))
  }
  res <- lapply(levels(groups), function(j) {
    temp_array <- array(A[, , which(groups == j)], dim = c(dim(A)[[1]], dim(A)[[2]], length(which(groups == j))))
    mn <- matrix(t(mshape(temp_array)), nrow = 1)
    A.d <- two.d.array(temp_array)
    d <- NULL
    for (i in 1:nrow(A.d)) {
      d <- c(d, as.vector(dist(rbind(mn, A.d[i, ]))))
    }
    if (is.null(dimnames(A.d)[[1]])) {
      dimnames(A.d)[[1]] <- as.character(seq(1:nrow(A.d))) #this is not throwing the error
    }
    names(d) <- dimnames(A.d)[[1]]
    #D <- d[order(d, decreasing = TRUE)]
    
    #ordered <- match(D, d)
    #names(ordered) <- names(D)
    return(d) # this was added
    # return(ordered)
  })
  names(res) <- levels(groups)
  if (length(levels(groups)) == 1) {
    res <- res$`All Specimens`
  }
  return(res)
} 

plotOutliers.ekb2 <- function (plotOutliers.ekb1_output, A, groups = NULL, inspect.outliers = FALSE, 
                               which_group = 1, produce_plot = T, pt_cex = 1,
                               txt_cex = 0.5, show_point_names = TRUE) 
{
  if (is.null(groups)) {
    groups = factor(rep("All Specimens", dim(A)[3]))
    d <- plotOutliers.ekb1_output
  } else {
    d <- unlist(plotOutliers.ekb1_output[which_group])
  }
  
  j <- levels(groups)[which_group]
  mn <- matrix(t(mshape(A[, , which(groups == j)])), nrow = 1)
  names(d) <- dimnames(A)[[3]][which(groups == j)]
  D <- d[order(d, decreasing = TRUE)]
  Q <- summary(D)
  Med <- as.numeric(summary(D)[3])
  LL <- as.numeric(Q[2] - 1.5 * (Q[5] - Q[2]))
  UL <- as.numeric(Q[5] + 1.5 * (Q[5] - Q[2]))
  if(produce_plot){
    par(mar = c(5, 3 + txt_cex, 4, 2))
    plot(D, type = "p", ylab = "Procrustes Distance from Mean", cex = pt_cex,
         pch = 19, xlab = "", xaxt = "n", main = j, 
         cex.lab=txt_cex, cex.axis=txt_cex, cex.main=txt_cex + 0.5)
    abline(a = LL, b = 0, lty = 2, col = "blue")
    abline(a = Med, b = 0, col = "blue")
    abline(a = UL, b = 0, lty = 2, col = "blue")
    text(x = length(d)-txt_cex/3, y = LL, labels = "lower quartile", 
         col = "blue", cex = txt_cex, adj = c(0.5, 1))
    text(x = length(d)-txt_cex/3, y = Med, labels = "median", col = "blue", 
         cex = txt_cex, adj = c(0.5, -0.5))
    text(x = length(d)-txt_cex/3, y = UL, labels = "upper quartile", 
         col = "blue", cex = txt_cex, adj = c(0.5, -0.5))
    if (any(D >= UL)) {
      points(D[which(D >= UL)], pch = 19, col = "red", cex = pt_cex)
    }
    if(show_point_names){
      if (any(D >= UL)) {
        diff_cols <- rep("black", length(D))
        diff_cols[which(D >= UL)] <- "red"
        text(D, labels = names(D), adj = c(0.5, 0.1), pos = 1, col = diff_cols,
             cex = txt_cex)
      }
      else {
        text(D, labels = names(D), adj = c(0.5, 0.1), pos = 1, 
             cex = txt_cex)
      }
    }
  }
  return(d)
}


# Color Selector Options


library(RColorBrewer)
all_color_options <- c(brewer.pal(name = "Spectral", n = 8), "black",
                       brewer.pal(name = "Reds", n = 9),
                       brewer.pal(name = "YlOrBr", n = 9),
                       brewer.pal(name = "Greens", n = 9),
                       brewer.pal(name = "Blues", n = 9),
                       brewer.pal(name = "Purples", n = 9),
                       brewer.pal(name = "Greys", n = 9))

colorSelectorDrop.ekb <- function(inputId, label, choices=all_color_options, display_label = FALSE, dropdownside = "left",
                                  ncol = 9, width = "180px", circle = F, size = "xs", up = FALSE, selected = all_color_options[9]) 
{
  size <- match.arg(arg = size, choices = c("default", "lg", 
                                            "sm", "xs"))
  dropdownside <- match.arg(arg = dropdownside, choices = c("left", "right"))
  btnId <- paste("btn", inputId, sep = "-")
  funButton <- if (circle) 
    shinyWidgets:::circleButton
  else shinyWidgets:::squareButton
  btn <- funButton(inputId = btnId, icon = NULL, status = "default", style = paste0("background-color: ", selected, ";", sep = ""), 
                   size = size, class = "dropdown-toggle", `data-toggle` = "dropdown")
  if(dropdownside == "right") { ddclass <- "dropdown-menu dropdown-menu-right" }
  if(dropdownside == "left") {ddclass <- "dropdown-menu" }
  
  dropTag <- htmltools::tags$ul(class = ddclass , style = if (!is.null(width)) 
    paste0("width: ", htmltools::validateCssUnit(width), 
           ";"), colorSelectorInput(inputId = inputId, label = label, 
                                    choices = choices, selected = selected, mode = "radio", 
                                    display_label = display_label, ncol = ncol))
  js <- paste0("$(document).on(\"change\",\"input[name='", 
               inputId, "']\",function(){\n      var v = $(\"input[name='", 
               inputId, "']:checked\").val();\n      $(\"#", btnId, 
               "\").css(\"background-color\", v);\n    });")
  htmltools::tags$div(class = ifelse(up, "dropup", "dropdown"), 
                      btn, dropTag, htmltools::tags$script(HTML(js)))
}

add.trajectories.ekb <- function (TP, traj.pch = 21, traj.col = 1, traj.lty = 1, traj.lwd = 1, 
                                  traj.cex = 1.5, traj.bg = 1, start.bg = 3, end.bg = 2, traj.gp.bg = NULL) 
{
  traj <- TP$trajectories
  nt <- length(traj)
  np <- NROW(traj[[1]])
  if (length(traj.pch) != 1 && length(traj.pch) != nt) 
    stop("For add.trajectories, traj.pch must be equal in length \n         to the number of trajectories or just one value\n", 
         call. = FALSE)
  else if (length(traj.pch) == 1) 
    traj.pch <- rep(traj.pch, nt)
  if (length(traj.col) != 1 && length(traj.col) != nt) 
    stop("For add.trajectories, traj.col must be equal in length \n         to the number of trajectories or just one value\n", 
         call. = FALSE)
  else if (length(traj.col) == 1) 
    traj.col <- rep(traj.col, nt)
  if (length(traj.lty) != 1 && length(traj.lty) != nt) 
    stop("For add.trajectories, traj.lty must be equal in length \n         to the number of trajectories or just one value\n", 
         call. = FALSE)
  else if (length(traj.lty) == 1) 
    traj.lty <- rep(traj.lty, nt)
  if (length(traj.lwd) != 1 && length(traj.lwd) != nt) 
    stop("For add.trajectories, traj.lwd must be equal in length \n         to the number of trajectories or just one value\n", 
         call. = FALSE)
  else if (length(traj.lwd) == 1) 
    traj.lwd <- rep(traj.lwd, nt)
  if (length(traj.cex) != 1 && length(traj.cex) != nt) 
    stop("For add.trajectories, traj.cex must be equal in length \n         to the number of trajectories or just one value\n", 
         call. = FALSE)
  else if (length(traj.cex) == 1) 
    traj.cex <- rep(traj.cex, nt)
  if (length(traj.bg) != 1 && length(traj.bg) != nt) 
    stop("For add.trajectories, traj.bg must be equal in length \n         to the number of trajectories or just one value\n", 
         call. = FALSE)
  else if (length(traj.bg) == 1) 
    traj.bg <- rep(traj.bg, nt)
  if(!is.null(traj.gp.bg)) {
    if(length(traj.gp.bg) != np) stop("For add.trajectories, traj.gp.bg must be equal in length \n to the length of the trajectories.")
  }
  if (length(start.bg) != 1 && length(start.bg) != nt) 
    stop("For add.trajectories, start.bg must be equal in length \n         to the number of trajectories or just one value\n", 
         call. = FALSE)
  else if (length(start.bg) == 1) 
    start.bg <- rep(start.bg, nt)
  if (length(end.bg) != 1 && length(end.bg) != nt) 
    stop("For add.trajectories, end.bg must be equal in length \n         to the number of trajectories or just one value\n", 
         call. = FALSE)
  else if (length(end.bg) == 1) 
    end.bg <- rep(end.bg, nt)
  for (i in 1:nt) {
    x <- traj[[i]][, 1]
    y <- traj[[i]][, 2]
    lines(x, y, col = traj.col[i], lwd = traj.lwd[i], lty = traj.lty[i])
    
    if(!is.null(traj.gp.bg)){
      for(j in 1:np) {
        points(x[j], y[j], col = traj.gp.bg[j], pch = traj.pch[i], lwd = traj.lwd[i], 
               cex = traj.cex[i], bg = traj.gp.bg[j])
      }
    } else {
      points(x, y, col = 1, pch = traj.pch[i], lwd = traj.lwd[i], 
             cex = traj.cex[i], bg = traj.bg[i])
    }
    
    
  }
}


model_definition <- function(input.independent_variables, vals.trait_rx, vals.csize, 
                             vals.trait_1_treatment, vals.trait_2_treatment, vals.trait_3_treatment,
                             vals.trait_names, vals.phy_rx, gpa_coords_rx_reactive, 
                             input.independent_variables_order,
                             input.pgls_ols, input.anova_perm, input.ss_type ) { #,
  #input.interactions_included
  input.interactions_included <- F       
  
  if(!is.null(vals.trait_rx)) {
    trait_3 <- trait_2 <- trait_1 <- vals.trait_rx[,2] # filling in trait_2 and 3 so that gdf does not require a bunch of dependencies
    names(trait_3) <- names(trait_2) <- vals.trait_rx[,2]
  } else { trait_1 <- trait_2 <- trait_3 <- NULL }
  
  
  csize <- NULL
  if(!is.null(vals.csize)) { 
    csize <- vals.csize
  }
  
  if(!is.null(trait_1)) {
    if(vals.trait_1_treatment == "disc") {  
      trait_1 <- as.factor(vals.trait_rx[,2])
      names(trait_1) <- vals.trait_rx[,1]
    }
    if(vals.trait_1_treatment == "cont") {
      trait_1_c <- as.character(vals.trait_rx[,2]) 
      trait_1 <- as.numeric(trait_1_c)
      names(trait_1) <- vals.trait_rx[,1]
    }
    if("by_trait_2" %in% input.independent_variables) {     
      if(vals.trait_2_treatment == "disc") {
        trait_2 <- as.factor(vals.trait_rx[,3])
        names(trait_2) <- vals.trait_rx[,1]
      }
      if(vals.trait_2_treatment == "cont") {
        trait_2_c <- as.character(vals.trait_rx[,3]) 
        trait_2 <- as.numeric(trait_2_c)
        names(trait_2) <- vals.trait_rx[,1]
      } 
    }
    if("by_trait_3" %in% input.independent_variables) {     
      if(vals.trait_3_treatment == "disc") {
        trait_3 <- as.factor(vals.trait_rx[,4])
        names(trait_3) <- vals.trait_rx[,1]
      }
      if(vals.trait_3_treatment == "cont") {
        trait_3_c <- as.character(vals.trait_rx[,4]) 
        trait_3 <- as.numeric(trait_3_c)
        names(trait_3) <- vals.trait_rx[,1]
      } 
    }
  }
  
  if(length(input.independent_variables) == 1) { 
    independent_variables_order_renamed <- input.independent_variables } else {
      independent_variables_order_renamed <- input.independent_variables_order
      
      all_possible_names <- vals.trait_names
      
      for(i in 1:length(all_possible_names)){
        names(all_possible_names)[i] <- substring(names(all_possible_names)[i], 4, nchar(names(all_possible_names)[i]))
      }
      for(i in 1:length(independent_variables_order_renamed)){
        independent_variables_order_renamed[i] <- all_possible_names[match(independent_variables_order_renamed[i], names(all_possible_names))]
      }
    }
  if(input.pgls_ols == "pgls" & !is.null(vals.phy_rx)) { 
    if(is.null(trait_1) & !is.null(csize)) { 
      
      gdf <- geomorph.data.frame(shape = gpa_coords_rx_reactive, phy = vals.phy_rx, csize = csize)
      
    } else {
      if(!is.null(csize)) {
        gdf <- geomorph.data.frame(shape = gpa_coords_rx_reactive, phy = vals.phy_rx, 
                                   trait_1 = trait_1, trait_2 = trait_2, trait_3 = trait_3,
                                   csize = csize) 
      } else {
        gdf <- geomorph.data.frame(shape = gpa_coords_rx_reactive, phy = vals.phy_rx, 
                                   trait_1 = trait_1, trait_2 = trait_2, trait_3 = trait_3) 
      }
      
    }
    
    # 1 trait selected
    if(identical(independent_variables_order_renamed, "by_trait_1")) { 
      results <- procD.pgls(shape ~ trait_1, phy = phy, data = gdf, print.progress = F, 
                            iter = input.anova_perm, SS.type = as.character(input.ss_type)) } 
    if(identical(independent_variables_order_renamed, "by_trait_2")) { 
      results <- procD.pgls(shape ~ trait_2, phy = phy, data = gdf, print.progress = F, 
                            iter = input.anova_perm, SS.type = as.character(input.ss_type)) } 
    if(identical(independent_variables_order_renamed, "by_trait_3")) { 
      results <- procD.pgls(shape ~ trait_3, phy = phy, data = gdf, print.progress = F, 
                            iter = input.anova_perm, SS.type = as.character(input.ss_type)) } 
    if(identical(independent_variables_order_renamed, "csize")) { 
      results <- procD.pgls(shape ~ csize, phy = phy, data = gdf, print.progress = F, 
                            iter = input.anova_perm, SS.type = as.character(input.ss_type))} 
    
    # 2 traits selected with interaction options
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_2"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ trait_1*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_1"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ trait_2*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_3"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ trait_2*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.pgls(shape ~ trait_2 + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_2"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ trait_3*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.pgls(shape ~ trait_3 + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_3"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ trait_1*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.pgls(shape ~ trait_1 + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_1"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ trait_3*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.pgls(shape ~ trait_3 + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_1", "csize"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ trait_1*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.pgls(shape ~ trait_1 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_2", "csize"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ trait_2*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.pgls(shape ~ trait_2 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_3", "csize"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ trait_3*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.pgls(shape ~ trait_3 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_1"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ csize*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.pgls(shape ~ csize + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_2"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ csize*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.pgls(shape ~ csize + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_3"))){
      if(input.interactions_included) {
        results <- procD.pgls(shape ~ csize*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.pgls(shape ~ csize + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    
    
    # 3 traits selected with interaction options
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_2", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*trait_2*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + trait_2 + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_2", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*trait_2*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + trait_2 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_3", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*trait_3*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + trait_3 + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_3", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*trait_3*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + trait_3 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "csize", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*csize*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + csize + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "csize", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*csize*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + csize + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_1", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*trait_1*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + trait_1 + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_1", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*trait_1*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + trait_1 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_3", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*trait_3*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + trait_3 + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_3", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*trait_3*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + trait_3 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "csize", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*csize*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + csize + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "csize", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*csize*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + csize + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_1", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*trait_1*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + trait_1 + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_1", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*trait_1*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + trait_1 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_2", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*trait_2*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + trait_2 + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_2", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*trait_2*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + trait_2 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "csize", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*csize*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + csize + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "csize", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*csize*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + csize + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_1", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_1*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_1 + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_1", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_1*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_1 + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_2", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_2*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_2 + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_2", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_2*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_2 + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_3", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_3*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_3 + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_3", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_3*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_3 + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    
    # 4 traits
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_2", "by_trait_3", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*trait_2*trait_3*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + trait_2 + trait_3 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_2", "csize", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*trait_2*csize*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + trait_2 + csize + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "csize", "by_trait_2", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*csize*trait_2*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + csize + trait_2 + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "csize", "by_trait_3", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*csize*trait_3*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + csize + trait_3 + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_3", "by_trait_2", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*trait_3*trait_2*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + trait_3 + trait_2 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_3", "csize", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_1*trait_3*csize*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_1 + trait_3 + csize + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_1", "by_trait_3", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*trait_1*trait_3*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + trait_1 + trait_3 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_1", "csize", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*trait_1*csize*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + trait_1 + csize + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_3", "by_trait_1", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*trait_3*trait_1*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + trait_3 + trait_1 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_3", "csize", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*trait_3*csize*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + trait_3 + csize + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "csize", "by_trait_1", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*csize*trait_1*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + csize + trait_1 + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "csize", "by_trait_3", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_2*csize*trait_3*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_2 + csize + trait_3 + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_1", "by_trait_2", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*trait_1*trait_2*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + trait_1 + trait_2 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_1", "csize", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*trait_1*csize*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + trait_1 + csize + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_2", "by_trait_1", "csize"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*trait_2*trait_1*csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + trait_2 + trait_1 + csize, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_2", "csize", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*trait_2*csize*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + trait_2 + csize + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "csize", "by_trait_1", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*csize*trait_1*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + csize + trait_1 + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "csize", "by_trait_2", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ trait_3*csize*trait_2*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ trait_3 + csize + trait_2 + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_1", "by_trait_2", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_1*trait_2*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_1 + trait_2 + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_1", "by_trait_3", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_1*trait_3*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_1 + trait_3 + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_2", "by_trait_1", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_2*trait_1*trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_2 + trait_1 + trait_3, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_2", "by_trait_3", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_2*trait_3*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_2 + trait_3 + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_3", "by_trait_1", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_3*trait_1*trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_3 + trait_1 + trait_2, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_3", "by_trait_2", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.pgls(shape ~ csize*trait_3*trait_2*trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.pgls(shape ~ csize + trait_3 + trait_2 + trait_1, phy = phy, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    
  }
  if(input.pgls_ols == "ols") { 
    if(is.null(trait_1) & !is.null(csize)) { 
      gdf <- geomorph.data.frame(shape = gpa_coords_rx_reactive, csize = csize)
    } else {
      if(!is.null(csize)) {
        gdf <- geomorph.data.frame(shape = gpa_coords_rx_reactive, 
                                   trait_1 = trait_1, trait_2 = trait_2, trait_3 = trait_3,
                                   csize = csize) 
      } else { 
        gdf <- geomorph.data.frame(shape = gpa_coords_rx_reactive, 
                                   trait_1 = trait_1, trait_2 = trait_2, trait_3 = trait_3) 
      }
      
      
    }
    
    if(identical(independent_variables_order_renamed, "by_trait_1")) { 
      results <- procD.lm(shape ~ trait_1, data = gdf, print.progress = F, 
                          iter = input.anova_perm, SS.type = as.character(input.ss_type)) } 
    if(identical(independent_variables_order_renamed, "by_trait_2")) { 
      results <- procD.lm(shape ~ trait_2, data = gdf, print.progress = F, 
                          iter = input.anova_perm, SS.type = as.character(input.ss_type)) } 
    if(identical(independent_variables_order_renamed, "by_trait_3")) { 
      results <- procD.lm(shape ~ trait_3, data = gdf, print.progress = F, 
                          iter = input.anova_perm, SS.type = as.character(input.ss_type)) } 
    if(identical(independent_variables_order_renamed, "csize")) { 
      results <- procD.lm(shape ~ csize, data = gdf, print.progress = F, 
                          iter = input.anova_perm, SS.type = as.character(input.ss_type)) } 
    
    # 2 traits selected with interaction options
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_2"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ trait_1*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_1"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ trait_2*trait_1,  data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + trait_1,  data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_3"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ trait_2*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.lm(shape ~ trait_2 + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_2"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ trait_3*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.lm(shape ~ trait_3 + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_3"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ trait_1*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.lm(shape ~ trait_1 + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_1"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ trait_3*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.lm(shape ~ trait_3 + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_1", "csize"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ trait_1*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.lm(shape ~ trait_1 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_2", "csize"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ trait_2*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.lm(shape ~ trait_2 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("by_trait_3", "csize"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ trait_3*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.lm(shape ~ trait_3 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_1"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ csize*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.lm(shape ~ csize + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_2"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ csize*trait_2,  data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.lm(shape ~ csize + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_3"))){
      if(input.interactions_included) {
        results <- procD.lm(shape ~ csize*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type)) 
      } else {
        results <- procD.lm(shape ~ csize + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }}
    
    # 3 traits selected with interaction options
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_2", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*trait_2*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + trait_2 + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_2", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*trait_2*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + trait_2 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_3", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*trait_3*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + trait_3 + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_3", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*trait_3*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + trait_3 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "csize", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*csize*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + csize + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "csize", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*csize*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + csize + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_1", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*trait_1*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + trait_1 + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_1", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*trait_1*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + trait_1 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_3", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*trait_3*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + trait_3 + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_3", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*trait_3*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + trait_3 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "csize", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*csize*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + csize + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "csize", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*csize*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + csize + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_1", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*trait_1*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + trait_1 + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_1", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*trait_1*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + trait_1 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_2", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*trait_2*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + trait_2 + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_2", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*trait_2*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + trait_2 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "csize", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*csize*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + csize + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "csize", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*csize*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + csize + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_1", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_1*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_1 + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_1", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_1*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_1 + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_2", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_2*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_2 + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_2", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_2*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_2 + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_3", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_3*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_3 + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_3", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_3*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_3 + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    
    # 4 traits
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_2", "by_trait_3", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*trait_2*trait_3*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + trait_2 + trait_3 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_2", "csize", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*trait_2*csize*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + trait_2 + csize + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "csize", "by_trait_2", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*csize*trait_2*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + csize + trait_2 + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "csize", "by_trait_3", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*csize*trait_3*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + csize + trait_3 + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_3", "by_trait_2", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*trait_3*trait_2*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + trait_3 + trait_2 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_1", "by_trait_3", "csize", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_1*trait_3*csize*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_1 + trait_3 + csize + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_1", "by_trait_3", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*trait_1*trait_3*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + trait_1 + trait_3 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_1", "csize", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*trait_1*csize*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + trait_1 + csize + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_3", "by_trait_1", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*trait_3*trait_1*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + trait_3 + trait_1 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "by_trait_3", "csize", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*trait_3*csize*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + trait_3 + csize + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "csize", "by_trait_1", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*csize*trait_1*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + csize + trait_1 + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_2", "csize", "by_trait_3", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_2*csize*trait_3*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_2 + csize + trait_3 + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_1", "by_trait_2", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*trait_1*trait_2*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + trait_1 + trait_2 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_1", "csize", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*trait_1*csize*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + trait_1 + csize + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_2", "by_trait_1", "csize"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*trait_2*trait_1*csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + trait_2 + trait_1 + csize, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "by_trait_2", "csize", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*trait_2*csize*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + trait_2 + csize + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "csize", "by_trait_1", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*csize*trait_1*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + csize + trait_1 + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("by_trait_3", "csize", "by_trait_2", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ trait_3*csize*trait_2*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ trait_3 + csize + trait_2 + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_1", "by_trait_2", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_1*trait_2*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_1 + trait_2 + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_1", "by_trait_3", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_1*trait_3*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_1 + trait_3 + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_2", "by_trait_1", "by_trait_3"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_2*trait_1*trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_2 + trait_1 + trait_3, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_2", "by_trait_3", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_2*trait_3*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_2 + trait_3 + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_3", "by_trait_1", "by_trait_2"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_3*trait_1*trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_3 + trait_1 + trait_2, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    if(identical(independent_variables_order_renamed, c("csize", "by_trait_3", "by_trait_2", "by_trait_1"))){
      if(input.interactions_included){
        results <- procD.lm(shape ~ csize*trait_3*trait_2*trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      } else {
        results <- procD.lm(shape ~ csize + trait_3 + trait_2 + trait_1, data = gdf, print.progress = F, iter = input.anova_perm, SS.type = as.character(input.ss_type))
      }
    }
    
  }
  return(results)
  
}

# updateOrderInput


updateOrderInput <- function (session, inputId, label = NULL,
                              items = NULL, connect = NULL,
                              item_class = NULL) {
  item_class = match.arg(item_class,
                         c("default", "primary", "success",
                           "info", "warning", "danger"))
  if(!is.null(items)) {items <- digestItems(items)}
  if(!is.null(connect)){
    if(connect[1] == FALSE) { 
      connect <- "false"
    } else {
      connect <- paste0("#", connect, collapse = ", ")
    }
  }
  message <- list(label      = label,
                  items      = items,
                  connect    = connect,
                  item_class = item_class)
  message <- Filter(Negate(is.null), message)
  session$sendInputMessage(inputId, message)
} # copied verbatim from https://github.com/Yang-Tang/shinyjqui/blob/master/R/orderInput.R on 11/8/20
    # edited line to: if(connect[1] == FALSE) { # I added the [1] on 3/7/23

digestItems <- function(items) {
  if (length(items) == 0 || (!is.vector(items) && !is.factor(items))) {
    item_values <- list()
    item_labels <- list()
  } else if(is.vector(items)) {
    item_values <- unlist(items, recursive = FALSE, use.names = TRUE)
    nms <- names(item_values)
    item_labels <- `if`(is.null(nms) || any(nms == "") || any(is.na(nms)),
                        item_values, nms)
  } else if (is.factor(items)) {
    item_values <- as.numeric(items)
    item_labels <- as.character(items) 
  }
  return(list(values = item_values, labels = item_labels))
}

pruning_script_function <- function(vals.lms_rx, vals.phy_rx, vals.trait_rx, vals.outlier_removed_names) {
  '
  drop_these <- NULL
  if(!is.null(vals.outlier_removed_names)) { 
    
    drop_these <- c(drop_these, vals.outlier_removed_names) 
    
    if(!is.null(vals.phy_rx)) {
      vals.phy_rx <- drop.tip(vals.phy_rx, tip = drop_these)
      if(!is.null(vals.trait_rx)) {
        vals.trait_rx <- vals.trait_rx[match(vals.phy_rx$tip.label, vals.trait_rx[,1]),]
      }
      if(!is.null(vals.lms_rx)) {
        vals.lms_rx <- vals.lms_rx[,,match(vals.phy_rx$tip.label,dimnames(vals.lms_rx)[[3]])]
      }
    } else {
      if(!is.null(vals.lms_rx)) {
        vals.lms_rx <- vals.lms_rx[,,-match(drop_these,dimnames(vals.lms_rx)[[3]])] 
        if(!is.null(vals.trait_rx)) {
          vals.trait_rx <- vals.trait_rx[match(dimnames(vals.lms_rx)[[3]], vals.trait_rx[,1]),] 
        }
        
      } else {
        if(!is.null(vals.trait_rx)) {
          vals.trait_rx <- vals.trait_rx[-match(drop_these, vals.trait_rx[,1]),]
        }
      } 
    }
    
  } 
  datasets <- c(!is.null(vals.trait_rx), !is.null(vals.phy_rx), !is.null(vals.lms_rx))
  
  if(length(which(datasets == T)) > 1) { # if there is more than one trait file
    if(is.null(vals.trait_rx)) { # if no trait file
      keep_these <- intersect(dimnames(vals.lms_rx)[[3]], vals.phy_rx$tip.label) 
      if(!is.null(vals$outlier_removed_names)) {
        keep_these <- keep_these[-match(vals$outlier_removed_names,keep_these)]
      }
      
      if(length(keep_these) != 0) {
        vals.phy_rx <- keep.tip(vals.phy_rx, tip = keep_these)
        vals.lms_rx <- vals.lms_rx[,,match(vals.phy_rx$tip.label, dimnames(vals.lms_rx)[[3]])] 
      }
    }
    if(is.null(vals.phy_rx)) { # if no phy file
      keep_these <- intersect(vals.trait_rx[,1], dimnames(vals.lms_rx)[[3]]) 
      
      if(length(keep_these) != 0) {
        vals.trait_rx <- vals.trait_rx[match(keep_these, vals.trait_rx[,1]),]
        vals.lms_rx <- vals.lms_rx[,,match(keep_these, dimnames(vals.lms_rx)[[3]])] 
      } 
      
    }
    if(is.null(vals.lms_rx)) { # if no lm file
      keep_these <- intersect(vals.trait_rx[,1], vals.phy_rx$tip.label)
      
      if(length(keep_these) != 0) {
        vals.phy_rx <- keep.tip(vals.phy_rx, tip = keep_these)
        vals.trait_rx <- vals.trait_rx[match(vals.phy_rx$tip.label, vals.trait_rx[,1]),]
      } 
      
    }
    
    if(!is.null(vals.trait_rx) & !is.null(vals.phy_rx) & !is.nullvals.lms_rx)) {
      keep_these <- Reduce(intersect, list(dimnames(vals.lms_rx)[[3]], vals.phy_rx$tip.label, vals.trait_rx[,1])) #  
      
      if(length(keep_these) != 0) {
        vals.phy_rx <- keep.tip(vals.phy_rx, tip = keep_these)
        vals.trait_rx <- vals.trait_rx[match(vals.phy_rx$tip.label, vals.trait_rx[,1]),]
        vals.lms_rx <- vals.lms_rx[,,match(vals.phy_rx$tip.label, dimnames(vals.lms_rx)[[3]])]  
        
      }
    }
    
  }
  '
}

#orderInput <- function(inputId, label, items,
#                       as_source = FALSE, connect = NULL,
#                       item_class = c("default", "primary", "success",
#                                      "info", "warning", "danger"),
#                       placeholder = NULL, width = "500px",
#                       legacy = FALSE, ...) {
#  
#  if(legacy) {
#    return(orderInputLegacy(inputId, label, items, as_source, connect,
#                            item_class, placeholder, width, ...))
#  }
#  
#  connect <- `if`(is.null(connect),
#                  "false", paste0("#", connect, collapse = ", "))
#  
#  item_class <- sprintf("btn btn-%s", match.arg(item_class))
#  
#  width <- shiny::validateCssUnit(width)
#  
#  
#  dep <- htmltools::htmlDependency(
#    name    = "orderInputBinding",
#    version = "0.3.3",
#    package = "shinyjqui",
#    src     = "www",
#    script  = "orderInputBinding.js"
#  )
#  
#  placeholder <- sprintf('#%s:empty:before{content: "%s"; font-size: 14px; opacity: 0.5;}',
#                         inputId, placeholder)
#  placeholder <- shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML(placeholder))))
#  
#  
#  label <- shiny::tags$label(label, `for` = inputId)
#  
#  items <- digestItems(items)
#  
#  tagsBuilder <- function(value, label) {
#    tag <- shiny::tags$div(
#      `data-value` = value,
#      class = item_class,
#      style = "margin: 1px",
#      label
#    )
#    if (as_source) {
#      options <- list(connectToSortable = connect,
#                      helper            = "clone",
#                      cancel            = "")
#      tag <- jqui_draggable(tag, options = options)
#    }
#    return(tag)
#  }
#  item_tags <- mapply(tagsBuilder, items$values, items$labels,
#                      SIMPLIFY = FALSE, USE.NAMES = FALSE)
#  
#  style <- sprintf("width: %s; font-size: 0px; min-height: 25px;", width)
#  container <- shiny::tags$div(
#    id    = inputId,
#    style = style,
#    # `data-order` = list(a = 1, b = 2),
#    class = ifelse(as_source,
#                   "jqui-orderInput-source",
#                   "jqui-orderInput"),
#    ...
#  )
#  container <- shiny::tagSetChildren(container, list = item_tags)
#  if (!as_source) {
#    cb <- "function(e, ui){if(!$(e.target).children().length)$(e.target).empty();}"
#    options <- list(connectWith = connect,
#                    remove      = htmlwidgets::JS(cb))
#    container <- jqui_sortable(container, options = options)
#  }
#  
#  return(shiny::tagList(dep, placeholder, label, container))
#  
#}

orderInputLegacy <- function(inputId, label, items,
                             as_source = FALSE, connect = NULL,
                             item_class = c(
                               "default", "primary", "success",
                               "info", "warning", "danger"
                             ),
                             placeholder = NULL,
                             width = "500px", ...) {
  if (is.null(connect)) {
    connect <- "false"
  } else {
    connect <- paste0("#", connect, collapse = ", ")
  }
  item_class <- sprintf("btn btn-%s", match.arg(item_class))
  
  if (length(items) == 0 || (!is.vector(items) && !is.factor(items))) {
    item_tags <- list()
  } else {
    if (is.vector(items)) {
      item_values <- unlist(items, recursive = FALSE, use.names = TRUE)
      nms <- names(item_values)
      item_html <- `if`(
        is.null(nms) || any(nms == "") || any(is.na(nms)),
        item_values, nms
      )
    } else if (is.factor(items)) {
      item_values <- as.numeric(items)
      item_html <- as.character(items)
    }
    item_tags <- lapply(1:length(item_values), function(i) {
      tag <- shiny::tags$div(
        item_html[i],
        `data-value` = item_values[i],
        class = item_class, style = "margin: 1px"
      )
      if (as_source) {
        options <- list(connectToSortable = connect, helper = "clone", cancel = "")
        tag <- jqui_draggable(tag, options = options)
      }
      return(tag)
    })
  }
  
  style <- sprintf(
    "width: %s; font-size: 0px; min-height: 25px;",
    shiny::validateCssUnit(width)
  )
  container <- shiny::tagSetChildren(
    shiny::tags$div(id = inputId, style = style, ...),
    list = item_tags
  )
  if (!as_source) {
    cb <- "function(e, ui){if(!$(e.target).children().length)$(e.target).empty();}"
    func <- 'function(event, ui){
      return $(event.target).children().map(function(i, e){
        return $(e).attr("data-value");
      }).get();
    }'
    options <- list(
      connectWith = connect,
      remove = htmlwidgets::JS(cb),
      shiny = list(
        order = list(
          sortcreate = htmlwidgets::JS(func),
          sortupdate = htmlwidgets::JS(func)
        )
      )
    )
    container <- jqui_sortable(container, options = options)
  }
  
  if (!is.null(placeholder)) {
    css <- '#%s:empty:before{content: "%s"; font-size: 14px; opacity: 0.5;}'
    placeholder <- shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            sprintf(css, inputId, placeholder)
          )
        )
      )
    )
  }
  
  shiny::tagList(
    placeholder,
    shiny::tags$label(label, `for` = inputId),
    container
  )
}
