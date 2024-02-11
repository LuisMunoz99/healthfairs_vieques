# Authors:     LM
# Maintainers: LM
# Date: 9-oct-23
# =========================================

# --- libs --- 

if(!require(pacman))install.packages("pacman")
p_load(argparse,
       here,
       openxlsx,
       dplyr,
       stringr,
       tidyr)


# args {{{
args <- list(input1 = here("import/input/vieques1.xlsx"),
             input2 = here("import/input/vieques2.xlsx"),
             output1 = here("import/output/patients_vieques.csv"),
             output2 = here("import/output/visits_vieques.csv"))

# --- import --- 
vieques1 <- read.xlsx(args$input1)
vieques2 <- read.xlsx(args$input2)

vieques1 <- vieques1 %>% 
  mutate(across(everything(), tolower)) %>% 
  rename_all(tolower) %>% 
  select(c("patient.name", "gender", "age", starts_with("specialist"))) %>% 
  mutate(fair = "First Health Fair")

vieques2 <- vieques2 %>% mutate(across(everything(), tolower)) %>% 
  rename_all(tolower) %>% 
  select(c("patient.name", "gender", "age", starts_with("specialist"))) %>% 
  mutate(specialist6 = NA,
         fair = "Second Health Fair")


all_pts <- rbind(vieques1,vieques2) # by number of healthfair

rm(vieques1,vieques2)



# --- cleaning --- 
# Keep only specialties (fair stations) columns
specialties_pts <- all_pts %>% select(c("patient.name", starts_with("specialist"), "fair"))  %>%
  mutate_all(
    ~str_replace_all(.,c(
      "\\brheuma\\w*" = "rheumatology",
      "\\bcardio\\w*" = "cardiology",
      "\\bintern\\w*" = "internal medicine",
      "\\bpneumo\\w*" = "pneumology",
      "\\bpsy\\w*" = "psychology",
      "\\bchiro\\w*" = "chiropractic",
      "\\bonco\\w*" = "oncology",
      "\\bobg\\w*" = "OBGYN",
      "\\bped\\w*" = "pediatrics"))) %>%
  mutate_at(vars(starts_with("specialist")), ~str_trim(.))

# Data is currently into patient level (159 patients)
# Get specialties into a single column making the data into "visits level" data
visits_pts <- specialties_pts %>%
  pivot_longer(cols = starts_with("specialist"), names_to = NULL, values_to = "specialties") %>%
  drop_na() %>% arrange(specialties)


# --- Export --- 
fwrite(all_pts, args$output1) # Patient level
fwrite(visits_pts, args$output2) # Visits level

