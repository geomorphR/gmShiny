install.packages("StereoMorph")
library(StereoMorph)
library(geomorph)

# 1 Curve
digitizeImages(image.file = "Documents/School/Projects/ShinyMorph/testing_datasets/Stereomorph/pics",
               shapes.file = "Documents/School/Projects/ShinyMorph/testing_datasets/Stereomorph/output", 
               landmarks.ref = paste("LM", 1:8, sep=""),
               curves.ref = "Documents/School/Projects/ShinyMorph/testing_datasets/Stereomorph/example.curves.ekb.txt")

shapes <- readShapes("~/Documents/School/Projects/ShinyMorph/testing_datasets/Stereomorph/output")
length(shapes$curves.control[[1]])
shapesGM <- readland.shapes(shapes, nCurvePts = 4) # can change 10 to a different number
shapesGM$landmarks

Y.gpa <- gpagen(shapesGM)
plot(Y.gpa)
shapesGM$fixed
shapesGM$curves

# can't do images if the bracketing landmarks are missing.

# No Curves
digitizeImages(image.file = "~/Documents/School/Projects/ShinyMorph/testing_datasets/Stereomorph/pics-no_curves",
               shapes.file = "~/Documents/School/Projects/ShinyMorph/testing_datasets/Stereomorph/output-no_curves", 
               landmarks.ref = paste("LM", 1:4, sep=""))

setwd("~/Documents/School/Projects/ShinyMorph/testing_datasets/Stereomorph/output-no_curves")
shapes <- readShapes(c("KU-66178-H.txt", "KU-66179-H.txt", "KU-66183-H.txt", "KU-66184-H.txt"))
names(shapes$scaling.units)
shapesGM <- readland.shapes(shapes, nCurvePts = NULL) # can change 10 to a different number

names(shapesGM$landmarks)
# Perform GPA
anyNA(unlist(shapesGM$landmarks))
shapesGM_missing_estimated <- estimate.missing(shapesGM)
names(shapesGM_missing_estimated$landmarks) <- names(shapesGM$landmarks) # this is correct


Y.gpa <- gpagen(shapesGM_missing_estimated, print.progress = FALSE)
plot(Y.gpa)
plotOutliers(Y.gpa$coords)

# 4 Curves
setwd("~/Documents/")
digitizeImages(image.file = "School/Projects/ShinyMorph/testing_datasets/Stereomorph/pics-4_curves",
               shapes.file = "School/Projects/ShinyMorph/testing_datasets/Stereomorph/output-4_curves", 
               landmarks.ref = paste("LM", 1:10, sep=""),
                 curves.ref = "School/Projects/ShinyMorph/testing_datasets/Stereomorph/example.curves.4_curves.txt")

setwd("~/Documents/School/Projects/ShinyMorph/testing_datasets/Stereomorph/output-no_curves")

shapes <- readShapes("~/Documents/School/Projects/ShinyMorph/testing_datasets/Stereomorph/output-4_curves")
shapesGM <- readland.shapes(shapes, nCurvePts = c(4,3,4,4)) # can change 10 to a different number
shapesGM$curves


drawCheckerboard(nx=8, ny=6, square.size=180, filename='Checkerboard 8x6 (180px).JPG')