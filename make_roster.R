library(officer)
library(flextable)
library(dplyr)

# Create the data
dates <- seq.Date(from = as.Date("2025-03-04"), by = "week", length.out = 20)
roster_data <- data.frame(
  Week_Starting = format(dates, "%b %d"),
  Bathroom = c("Dom", "Ruby", "Brian", "Jarrah") %>% rep(5),
  Bathroom_Check = "□",
  Kitchen = c("Jarrah", "Dom", "Ruby", "Brian") %>% rep(5),
  Kitchen_Check = "□",
  Living = c("Brian", "Jarrah", "Dom", "Ruby") %>% rep(5),
  Living_Check = "□",
  Week_Off = c("Ruby", "Brian", "Jarrah", "Dom") %>% rep(5)
)

# Create and format the table
doc <- read_docx()
ft <- flextable(roster_data)

# Define colors for each person
colors <- list(
  Dom = "#DAEEF3",    # Light Blue
  Ruby = "#FFE6E6",   # Light Pink
  Brian = "#E6FFE6",  # Light Green
  Jarrah = "#FFFFD9"  # Light Yellow
)

# Apply background colors based on names
ft <- ft %>%
  bg(j = "Bathroom", bg = function(x) sapply(x, function(name) colors[[name]])) %>%
  bg(j = "Kitchen", bg = function(x) sapply(x, function(name) colors[[name]])) %>%
  bg(j = "Living", bg = function(x) sapply(x, function(name) colors[[name]])) %>%
  bg(j = "Week_Off", bg = function(x) sapply(x, function(name) colors[[name]]))

# Format the table
ft <- ft %>%
  set_header_labels(
    Week_Starting = "Week Starting",
    Bathroom = "Bathroom",
    Bathroom_Check = "✓",
    Kitchen = "Kitchen",
    Kitchen_Check = "✓",
    Living = "Living Areas & Laundry",
    Living_Check = "✓",
    Week_Off = "Week Off"
  ) %>%
  theme_box() %>%
  autofit()

# Add content to document
doc <- doc %>%
  body_add_par("Sharehouse Cleaning Roster", style = "heading 1") %>%
  body_add_par("March - June 2025", style = "Normal") %>%
  body_add_flextable(ft)

# Save the document
print(doc, target = "cleaning_roster.docx")
