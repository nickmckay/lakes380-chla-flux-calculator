library(lipdR)
library(ggplot2)
library(readxl)

output_folder <- "~/Download"

D <- readLipd("~/Dropbox/lipdverse/Lakes380National/LakeWaihau_40590.Lakes380.lpd")

D$paleoData[[3]]$measurementTable[[1]]$

#get DBD data:
dbdData <- read_excel("~/Download/Waihau_ICPMS_Cd_flux.xlsx")

HSIData <- read_excel("~/Download/WAIHAU - HSI COMP TEST.xlsx",skip = 1)


cmHsi <- bin(HSIData$dblf,HSIData$RABD660670,bin.vec = seq(0,484))
names(cmHsi) <- c("Depth (cm)","RABD660670")
cmHsi <- as.data.frame(cmHsi)

dbdData$`Depth (cm)` <- as.numeric(dbdData$`Depth (cm)` )

dbdData2 <- dplyr::left_join(dbdData,cmHsi)


op <- estimate_chla_flux_lipd(D$LakeOporoa_17286.Lakes380,smooth = 0)

plot_flux(op) + ggtitle("Oporoa") + xlim(c(1900,2000))

#or

plot_flux(op) + ggtitle("Oporoa") + scale_x_continuous(breaks = seq(1000,2000,by = 100))


op_data <- chla_flux_to_tibble(op)
write_csv(x = op_data,file.path(output_folder,"OporoaData.csv"))


ggsave(op_plot,filename = file.path(output_folder,"OporoaFluxPlot.pdf"))

wi <- estimate_chla_flux_lipd(D$LakeWiritoa_18934.Lakes380)

wi_plot <- plot_flux(wi) + ggtitle("Wiritoa")

wi_data <- chla_flux_to_tibble(wi)
write_csv(x = wi_data,file.path(output_folder,"WiritoaData.csv"))

ggsave(wi_plot,filename = file.path(output_folder,"WiritoaFluxPlot.pdf"))

