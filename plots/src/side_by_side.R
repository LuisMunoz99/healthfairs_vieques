# Plots for posters
# Authors:     LM
# Maintainers: LM
# Date: 9-oct-23
# =========================================

# --- libs --- 
if(!require(pacman))install.packages("pacman")
p_load(here,
       data.table,
       dplyr, 
       ggplot2,
       stringr,
       gridExtra)


# args {{{
args <- list(input = here("import/output/visits_vieques.csv"),
             output = here("plots/output/side_by_side_vieques.png"))

# Useful functions
wrap_labels <- function(labels) {
  str_wrap(labels, width = 10)
}


# --- importing vistis level data --- 
visits_pts <- fread(args$input)


# Renaming specialties (Make this easy editable) 
visits_pts <- visits_pts %>%
  mutate(specialties = str_to_title(specialties)) %>%
  mutate(specialties = case_when(
    specialties == "Internal Medicine" ~ "\nInternal\nMed",
    specialties == "Funcion Pulmonar" ~ "Pneumo",
    specialties == "Pneumology" ~ "Pneumo",
    specialties == "Rheumatology" ~ "Rheuma",
    specialties == "Cardiology" ~ "Cardio",
    specialties == "Pediatrics" ~ "Peds",
    specialties == "Chiropractic" ~ "Chiro",
    specialties == "Family Medicine" ~ "\nInternal\nMed",
    specialties == "Obgyn" ~ "OBGYN",
    specialties == "Cirujano" ~ "Surgery",
    specialties == "Vitales" ~ "Checkup",
    specialties == "Ent" ~ "ENT",
    specialties == "Psychology" ~ "Psych",
    specialties == "Hematologo" ~ "Onco",
    specialties == "Oncology" ~ "Onco",
    
    TRUE ~ specialties  
  ))



# --- Plots --- 
visits_count <- visits_pts %>% count(fair,specialties)  

# Add specific vjust numbers to accomodate levels appropriate
fair1 <- visits_count %>% filter(fair == "First Health Fair") %>% 
  mutate(vjust.text.x = ifelse(specialties == "\nInternal\nMed", 0.2, 0.6)) 


fair2 <- visits_count %>% filter(fair == "Second Health Fair") %>% 
  mutate(vjust.text.x = ifelse(specialties == "\nInternal\nMed", 0.3, 0.5)) %>% 
  filter(specialties != "Brigada") # Exclude brigades visits


# First health fair plot 
plot1 <- ggplot(fair1, aes(x = reorder(specialties, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#096192") +
  labs(
    title = "First Health Fair",
    x = "Stations",
    y = "Number of Visits") +
  geom_text(aes(label = wrap_labels(n)), size = 10, color = "white", fontface = "bold", vjust = 1.5) +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0, 60), expand = c(0, 0)) +
  scale_x_discrete() +
  theme_minimal() +
  theme(
    plot.margin = margin(l = 40, r = 0, b = 0, t = 0),
    plot.title = element_text(size = 30, margin = margin(10, 0, 20, 0), face = "bold"),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 20, color = "black", hjust = 0.5, vjust = 0.5),  # Middle align y-axis text
    axis.text.x = element_text(size = 15, 
                               color = "black", 
                               angle = 90,
                               vjust = fair1$vjust.text.x,
                               hjust = .5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 23, color = "black", vjust = 4.2, hjust = .6)
  )


plot1

# Second health fair plot 
plot2 <- ggplot(fair2, aes(x = reorder(specialties, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#0E8C7f") +
  labs(
    title = "Second Health Fair",
    x = "Stations",
    y = "Number of Visits"
  ) +
  geom_text(data = fair2[fair2$n > 4, ],aes(label = wrap_labels(n)), size = 8, color = "white", fontface = "bold", vjust = 1.5) +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0, 60), expand = c(0, 0)) +
  scale_x_discrete() +
  theme_minimal() +
  theme(
    plot.margin = margin(l = 40, r = 0, b = 0, t = 0),
    plot.title = element_text(size = 30, margin = margin(10, 0, 20, 0), face = "bold"),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 13,
                               color = "black",
                               angle = 90,
                               vjust = fair2$vjust.text.x,                                
                               hjust = 0.5),  # Center x-axis labels
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
)

plot2

# Create a multi-plot layout
side_by_side <- grid.arrange(plot1, plot2, ncol = 2)


# --- Export --- 
ggsave(args$output, side_by_side, width = 12, height = 8)



