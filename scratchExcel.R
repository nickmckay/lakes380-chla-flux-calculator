library(readxl)
library(ggplot2)
library(readr)
library(Lakes380ChlaFluxCalculator)


input_file <- "~/Download/WIRIT_LC3U_Data.xlsx"
output_folder <- "~/Download"

xl_hsi <- read_excel(input_file,sheet = "HSI")

depth_bins <- 1:ceiling(max(xl_hsi$`composite dblf (mm)`,na.rm = TRUE)) #HSI depth column here


#bin to mm
RABD660670 <- bin(xl_hsi$`composite dblf (mm)`, #HSI depth column here
                  xl_hsi$RABD660670, #RABD660670 c0olumn here
                  bin.vec = depth_bins)

depth_mid <- RABD660670$x

#get ages
xl_chron_head <- names(read_excel(input_file,
                       sheet = "Chronology",
                       skip = 0))

xl_chron <- read_excel(input_file,
                                  sheet = "Chronology",
                                  skip = 1)

names(xl_chron) <- xl_chron_head

chronDepths <- xl_chron$ShCal20_t #chronology Year AD column here

agesOnHsiDepths <- Hmisc::approxExtrap(x = xl_chron$`z (dblf)`*10, #chronology depth column here (adjusted by 10 for mm)
                                       y = 1950-xl_chron$ShCal20_t, #chronology Year AD column here
                                       xout = depth_mid)$y

plot(agesOnHsiDepths,RABD660670$y)


wirit <- estimate_chla_flux(depth = depth_mid,
                         time = agesOnHsiDepths,
                         rabd660670 = RABD660670$y,
                         smooth = TRUE,
                         max.time = 400)

alicePlot <- plot_flux(alice) + ggtitle("Alice from excel") + xlim(c(1600,2010))

aliceOut <- chla_flux_to_tibble(alice)

ggsave(alicePlot,filename = file.path(output_folder,"AliceFluxPlot.pdf"))

write_csv(x = aliceOut,file.path(output_folder,"alice.csv"))

wiritPlot <- plot_flux(wirit) + ggtitle("Wirit from excel")

ggsave(wiritPlot,filename = file.path(output_folder,"WiritoaFluxPlot.pdf"))


wiritOut <- chla_flux_to_tibble(wirit)
write_csv(x = wiritOut,file.path(output_folder,"wirit.csv"))

ggsave(op_plot,filename = file.path(output_folder,"OporoaFluxPlot.pdf"))


# Calibrate only if there's no age model -----------------------------------


#if time is excluded (or set to NA) then it should still work
op_notime <- estimate_chla_flux(depth = depth_mid,
                         rabd660670 = RABD660670$y,
                         smooth = TRUE)

op_plot_notime <- plot_flux(op_notime) + ggtitle("Oporoa from excel, no time")

op_data_notime <- chla_flux_to_tibble(op_notime)
write_csv(x = op_data_notime,file.path(output_folder,"OporoaData_notime.csv"))

ggsave(op_plot_notime,filename = file.path(output_folder,"OporoaFluxPlot_notime.pdf"))



