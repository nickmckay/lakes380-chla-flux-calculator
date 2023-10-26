library(readxl)
library(ggplot2)
library(readr)
library(Lakes380ChlaFluxCalculator)


input_file <- "~/Download/WaihauWithRABD.xlsx"
output_folder <- "~/Download"

xl_hsi <- read_excel(input_file,col_types = "numeric")

WAIHA <- estimate_chla_flux(depth = xl_hsi$`Depth (cm)`,
                         time = 1950-xl_hsi$ShCal20_t,
                         rabd660670 = xl_hsi$RABD660670,
                         dbd = xl_hsi$`Dry density (g.cm-3)`,
                         smooth = TRUE)

waihauPlot <- plot_flux(WAIHA) + ggtitle("Waihau from excel")
waihauPlot


