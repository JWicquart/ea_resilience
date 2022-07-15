# 1. Required packages ----

require(extrafont) # For fonts

# 2. Set the default font family ----

windowsFonts("Open Sans" = windowsFont("Open Sans"))

font_choose_graph <- "Open Sans"
font_choose_map <- "Open Sans"

# 3. Set the colors ----

col_fill_graph <- "#89C4F4"
col_color_graph <- "#446CB3"
col_fill_map <- "#f2caae"
col_color_map <- "#888888"
col_background_map <- "#e4f1fe"

palette_regions <- c("#446cb3", "#3498db", "#16a085", "#26c281", 
                     "#0db4b9", "#f2784b", "#d64541", "#af4154", 
                     "#9b59b6", "#714d69")

# 4. Define a common crs ----

crs_defined <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
