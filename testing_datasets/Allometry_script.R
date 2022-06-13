library(geomorph)
data(plethspecies) 
trait_rx <- cbind(plethspecies$phy$tip.label, 
                  c(1,3,1,1,3,1,2,2,1),
                  c("A", "A", "B", "A", "B", "B", "B", "A", "A"), 
                  c(0.45, 0.22, 1.43, 3.79, 4.01, 0.95, 0.60, 1.32, 2.03))
colnames(trait_rx) <- c("Species", "Discrete Example Data A", "Discrete Example Data B", "Continuous Example Data")

pleth_gpa <- gpagen(plethspecies$land)
pleth_coords <- pleth_gpa$coords
phy <- plethspecies$phy
csize <- pleth_gpa$Csize
csize <- csize[match(phy$tip.label, names(csize))]

# changes
this_trait <- 4 # 3, 4
trait_1 <- trait_rx[match(dimnames(plethspecies$land)[[3]], trait_rx[,1]),this_trait]
if(this_trait == 4){
  trait_1 <- as.numeric(trait_1)
} else { trait_1 <- as.factor(trait_1) }
names(trait_1) <- dimnames(plethspecies$land)[[3]]

t1 <- trait_rx[match(phy$tip.label, trait_rx[,1]),2]
t1 <- as.factor(t1)
names(t1) <- phy$tip.label
t2 <- trait_rx[match(phy$tip.label, trait_rx[,1]),4]
t2 <- as.numeric(t2)
names(t2) <- phy$tip.label

type <- "regression"
reg.type <- "PredLine"
predictor <- csize #as.numeric(trait_rx[match(phy$tip.label, trait_rx[,1]),4]) # csize
xlab_overwrite <- colnames(trait_rx[,this_trait])
allometry_pt_size <- 1

input.anova_perm = 999 # 99
input.ss_type <- "I" # "II # "III"

gdf <- geomorph.data.frame(coords_to_use = pleth_gpa$coords[,,match(phy$tip.label, dimnames(pleth_gpa$coords)[[3]])], 
                           phy = phy, 
                           #trait_to_use = trait_1[match(phy$tip.label, names(trait_1))],
                           t1 = t1, t2 = t2,
                           csize = csize)
fit <- procD.pgls(coords_to_use ~ t2+t1, 
                data = gdf, 
                print.progress = F, 
                phy = phy, 
                iter = input.anova_perm, 
                SS.type = as.character(input.ss_type))
summary(fit)

dd <- plot(fit, type = type, 
     reg.type = reg.type, 
     predictor = log(predictor), 
     #col = color, 
     xlab = xlab_overwrite,
     pch = 19, 
     cex = allometry_pt_size)

sort(dd$PredLine)

out <- geomorph:::plot.procD.lm(fit, type = type, 
                                reg.type = reg.type, 
                                predictor = predictor, 
                                col = "black", xlab = xlab_overwrite,
                                pch = 19, 
                                cex = allometry_pt_sz)

out$PredLine







