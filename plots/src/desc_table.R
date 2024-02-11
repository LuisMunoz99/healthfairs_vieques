# Plots for posters
# Authors:     LM
# Maintainers: LM
# Date: 9-oct-23
# =========================================

# --- libs --- 
if(!require(pacman))install.packages("pacman")
p_load(here,
       data.table,
       table1,
       stringr,
       rvest,
       dplyr)

# args {{{
args <- list(input = here("import/output/patients_vieques.csv"),
             output = here("plots/output/desc_table.png"))


# --- Importing patient level data --- 
all_pts <- fread(args$input)

# Descriptive table 
desc <- all_pts %>% 
  mutate(Gender = str_to_title(gender),
         Age = as.numeric(age)) %>% 
  mutate(Gender = case_when(
    Gender == "F" ~ "Female",
    Gender == "F " ~ "Female",
    Gender == "M" ~ "Male",
    TRUE ~ Gender))

desc$Gender <- as.factor(desc$Gender) 

desc_table <- table1(~ Age + Gender | fair, data=desc, render.missing = NULL)

desc_table


