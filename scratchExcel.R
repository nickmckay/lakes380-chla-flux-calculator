library(readxl)
library(ggplot2)
library(readr)
input_file <- "~/Downloads/POROA_LC4U_Data.xlsx"
output_folder <- "~/Downloads"

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

agesOnHsiDepths <- Hmisc::approxExtrap(x = xl_chron$`z (dblf)`, #chronology depth column here
                                       y = 1950-xl_chron$ShCal20_t, #chronology Year AD column here
                                       xout = depth_mid)$y

plot(agesOnHsiDepths,RABD660670$y)


op <- estimate_chla_flux(depth = depth_mid,
                         time = agesOnHsiDepths,
                         rabd660670 = RABD660670$y,
                         smooth = TRUE)

op_plot <- plot_flux(op) + ggtitle("Oporoa from excel") + xlim(c(0,2020))

op_data <- chla_flux_to_tibble(op)
write_csv(x = op_data,file.path(output_folder,"OporoaData.csv"))


ggsave(op_plot,filename = file.path(output_folder,"OporoaFluxPlot.pdf"))
