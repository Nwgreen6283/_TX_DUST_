{r}
#Determine the LOD and LOQ by plotting the Normal QQ-Plot
qqnorm.ct.n1 <- qqnorm(wbe$ct[which(wbe$target=="N1")], plot.it = T) %>% as.data.frame()
qqnorm.ct.n2 <- qqnorm(wbe$ct[which(wbe$target=="N2")], plot.it = T) %>% as.data.frame()
tiff(filename = "figures/LML/detection_lims.tiff", height = 9, width = 8, units = "in", res = 600)
qqnorm.Explorer.ct <- function(qqnorm.ct){
  qqnorm.ct <- qqnorm.ct[which(complete.cases(qqnorm.ct)),]
  qqnorm.ct <- qqnorm.ct[order(qqnorm.ct$x),]
  qqnorm.ct <- cbind(qqnorm.ct, rbind(NA, qqnorm.ct[-nrow(qqnorm.ct),])) %>% setNames(., nm = c("x", "y", "x-1", "y-1"))
  qqnorm.ct %<>% mutate(rise = y-`y-1`, run = x-`x-1`) %>% mutate(slope = rise / run)
  qqnorm.ct$lod <- NA
  qqnorm.ct$loq <- NA
  prev.slope <- 1
  lod.found <- 0
  for(i in nrow(qqnorm.ct):2){
    if(lod.found==0){
      if(qqnorm.ct$slope[i]<1 & prev.slope <1){
        qqnorm.ct$lod[i] <- 1
        lod.found <- 1
      }else{
        prev.slope <- qqnorm.ct$slope[i]
      }
    }
    if(lod.found==1){
      if(qqnorm.ct$slope[i]>1){
        qqnorm.ct$loq[i] <- 1
        break
      }else{
        prev.slope <- qqnorm.ct$slope[i]
      }
    }
  }
  lod.ct <- qqnorm.ct$y[which(qqnorm.ct$lod==1)]
  loq.ct <- qqnorm.ct$y[which(qqnorm.ct$loq==1)]
  return(list(qqnorm.dataset = qqnorm.ct, lod = lod.ct, loq = loq.ct))
}
qqnorm.ct.n1 <- qqnorm.Explorer.ct(qqnorm.ct.n1)
qqnorm.ct.n2 <- qqnorm.Explorer.ct(qqnorm.ct.n2)
n1_lod <- qqnorm.ct.n1$lod
n1_loq <- qqnorm.ct.n1$loq
n2_lod <- qqnorm.ct.n2$lod
n2_loq <- qqnorm.ct.n2$loq