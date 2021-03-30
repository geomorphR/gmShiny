library(geomorph)
# help file example

data(Pupfish)
dd <- gm.prcomp(Pupfish$coords)
plot(dd)
fit <- lm.rrpp(coords ~Pop*Sex, data = Pupfish, iter = 199)
reveal.model.designs(fit)
TA <- trajectory.analysis(fit, groups = Pupfish$Pop, 
                          traj.pts = Pupfish$Sex, print.progress = FALSE)

# Magnitude difference (absolute difference between path distances)
sum <- summary(TA, attribute = "MD") 

# Correlations (angles) between trajectories
summary(TA, attribute = "TC", angle.type = "deg") 

# No shape differences between vectors
summary(TA, attribute = "SD") 

# Retain results
TA.summary <- summary(TA, attribute = "MD")
TA.summary
TA.summary$summary.table

# Plot results
TP <- plot(TA, pch = as.numeric(Pupfish$Pop) + 20, bg = as.numeric(Pupfish$Sex),
           cex = 0.7, col = "gray")
add.trajectories(TP, traj.pch = c(21, 22), start.bg = 1, end.bg = 2)
legend("topright", levels(Pupfish$Pop), pch =  c(21, 22), pt.bg = 1)




# vignette example
data("larvalMorph")
writeland.tps(larvalMorph$tailcoords, "~/Documents/School/Projects/ShinyMorph/testing_datasets/Trajectory_tab/land_vig.tps")
write.csv(larvalMorph$tail.sliders, "~/Documents/School/Projects/ShinyMorph/testing_datasets/Trajectory_tab/vig_sliders.csv", row.names = F)
larv_mat <- cbind(as.character(larvalMorph$treatment), as.character(larvalMorph$family))
row.names(larv_mat) <- dimnames(larvalMorph$tailcoords)[[3]]
colnames(larv_mat) <- c("treatment", "family")
write.csv(larv_mat, "~/Documents/School/Projects/ShinyMorph/testing_datasets/Trajectory_tab/vig_trait.csv")
new_sliders <- read.csv("~/Downloads/semilms_matrix-45.csv")
Y.gpa <- gpagen(larvalMorph$tailcoords, curves = larvalMorph$tail.sliders,
                ProcD = T, print.progress = FALSE)

gdf <- geomorph.data.frame(Y.gpa, treatment = larvalMorph$treatment, 
                           family = larvalMorph$family)

fit.common <- procD.lm(coords ~ Csize + treatment*family,  # edited from log(Csize) to Csize
                       data = gdf, print.progress = FALSE, iter = 199)

TA <- trajectory.analysis(fit.common, 
                          groups = gdf$treatment, 
                          traj.pts = gdf$family,
                          pca = TRUE, print.progress = FALSE)
dd <- summary(TA, attribute = "MD")

length(dd$pairwise.tables)


TP <- plot(TA, pch = 19, cex = 0.7, col = as.numeric(gdf$treatment))
add.trajectories(TP, traj.bg = 1:nlevels(gdf$treatment), 
                 start.bg = 1:nlevels(gdf$treatment),
                 end.bg = 1:nlevels(gdf$treatment))


