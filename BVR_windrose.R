# Install openair if not installed
#install.packages("openair")

# Load library
library(openair)
library(tidyverse)
library(openair)
library(magick)
library(grid)

# Read CSV file (Assuming columns: 'u', 'v', 'w')
wind_data <- read_csv("https://raw.github.com/FLARE-forecast/BVRE-data/bvre-platform-data/bvre-wind.csv", skip = 1)

# Remove specific rows (e.g., row 3 and row 7)
wind_data <- wind_data[-c(1,2), ]

# View first few rows
head(wind_data)

# Compute Wind Speed and Direction
wind_data$ws <- sqrt(wind_data$U_ms_Avg^2 + wind_data$V_ms_Avg^2)  # Wind Speed
wind_data$wd <- (270 - (atan2(wind_data$U_ms_Avg, -wind_data$V_ms_Avg) * (180 / pi))) %% 360 # Meteorological direction clockwise of true N counted to 270 and mathematical angle subtracted using the law of opposite angles and declination 8.833 degrees subtracted


#There's a data point where the windspeed is 87 m/s and speed of sound 259 m/s, hence, removing this data as a part of qaqc (windspeed < 10 m/s)
wind_data <- wind_data |> 
  mutate(Date = as.Date(TIMESTAMP)) |> 
  filter(ws <= 10)

# Convert negative directions to positive
wind_data$wd[wind_data$wd < 0] <- wind_data$wd[wind_data$wd < 0] + 360

# Create Wind Rose Plot
windRose(mydata = wind_data, ws = "ws", wd = "wd", key = TRUE, paddle = FALSE, 
         key.header = 'BVR Wind speed (m/s)', labels = 8, angle = 10,
         key.footer = ' ', dig.lab = 2, annotate = TRUE,
         angle.scale = 45, ws.int = 1, breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))


#THIS CHUNK PRODUCES WINDROSE FOR EACH DAY
#Plot windrose for each day
# Calculate wind direction and speed
wind_data <- wind_data %>%
  mutate(
    wd = (270 - (atan2(wind_data$U_ms_Avg, -wind_data$V_ms_Avg) * (180 / pi))) %% 360,  # Meteorological direction
    ws = sqrt(U_ms_Avg^2 + V_ms_Avg^2),
    wd_bin = cut(wd, breaks = seq(0, 360, by = 30), include.lowest = TRUE),
    ws_bin = cut(ws, breaks = c(0, 1, 2, 3, 4, 6, 8, 10),
                 labels = c("0-1", "1-2", "2-3", "3-4", "4-6", "6-8", "8-10"))
  )

# Count observations in each bin
rose_data <- wind_data %>%
  group_by(Date, wd_bin, ws_bin) %>%
  summarise(count = n(), .groups = "drop")

#reverse the ws_bin factor levels
rose_data$ws_bin <- factor(rose_data$ws_bin, levels = rev(levels(rose_data$ws_bin)))

# Plot faceted wind rose
ggplot(rose_data, aes(x = wd_bin, y = count, fill = ws_bin)) +
  geom_col(position = "stack", width = 1, color = "white") +
  coord_polar(start = -pi/12) +  # rotate so North is up
  facet_wrap(~ Date) +
  scale_fill_brewer("Wind Speed (m/s)", palette = "YlOrRd", direction = 1) +
  theme_minimal(base_size = 10) +
  labs(title = "Daily Wind Rose Plots",
       x = "Wind Direction",
       y = "Frequency") +
  theme(
    axis.text.x = element_text(size = 8),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(0.5, "lines"),           # Add space between panels
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Border around each panel
    panel.background = element_rect(fill = "white")
  )
##########



#THIS CHUNK OVERLAYS WINDROSE WITH BVR MAP
###########
# Create and save windRose as PNG
png("windrose_no_borders.png", width = 500, height = 500, res = 200, bg = "transparent")

# Plot WindRose
windRose(
  wind_data,
  ws = "ws",                   # Wind speed column
  wd = "wd",                   # Wind direction column
  main = "",                   # No title at the top
  paddle = FALSE,              # No paddle
  annotate = FALSE,
  cols = "viridis",
  breaks = c(0, 1, 2, 3, 4, 6, 8, 10),
  key.position = "none",       # Remove the key (legend) entirely
  key = list(
    title = "",                # Remove legend title (e.g., "Wind Speed")
    labels = rep("", 5),       # Empty labels (removes text from legend)
    height = 0,                # Hide the key (legend) height
    width = 0,                 # Hide the key (legend) width
    space = "right",           # Place the key in the right space
    units = ""                 # Remove units like "m/s"
  ),
  par.settings = list(
    axis.text = list(col = "white"),       # Set text (e.g., compass directions) to white
    add.text = list(col = "white"),        # Set other text (labels, etc.) to white
    strip.background = list(col = "blue"),  # Transparent background for facet strip
    strip.text = list(col = "transparent"), # Remove strip text (like “frequency of”)
    
    # Remove central lines (axes) and borders
    panel.border = list(col = "black"),  # Remove border
    panel.grid = list(col = "black"),    # Remove gridlines
    layout.widths = list(
      key.left = 0.001, key.right = 0.001, # Minimize key panel width to essentially zero
      left.padding = 0, right.padding = 0   # No padding
    ),
    layout.heights = list(
      top.padding = 0,                    # No padding on top
      bottom.padding = 0             # Minimized bottom padding to hide any remaining space
    )
  )
)

dev.off()

# Load background image
background <- image_read("https://raw.github.com/bee-bake/EddyFlux_data/main/BVR.png")  # Replace with your actual image path

# Load windrose image (just saved)
overlay <- image_read("windrose_no_borders.png")

# Composite windrose on top of background
final <- image_composite(background, overlay, offset = "+278+522")  # adjust position as needed

# Show result
print(final)

# Optional: Save result
image_write(final, "final_overlay.png")
#########





















