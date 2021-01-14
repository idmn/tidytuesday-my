library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(forcats)

tuesdata <- tidytuesdayR::tt_load('2021-01-12')
artwork <- tuesdata$artwork
artists <- tuesdata$artists

# pre-processing ------------------------------------------
aw <- 
  artwork %>% 
  select(-id, -accession_number, -dateText, -creditLine, -dimensions,
         -inscription, -thumbnailCopyright, -thumbnailUrl, -url) %>% 
  filter(artistRole == "artist")

at <-
  artists %>% 
  select(-dates, -url)

# ---------------------------------------------------------
# Half of the works is from the guy called
# Turner, Joseph Mallord William
# so let's just analyse him

df <- 
  at %>% 
  filter(name == "Turner, Joseph Mallord William") %>% 
  inner_join(aw, by = c("id" = "artistId")) %>% 
  mutate(yo = year - yearOfBirth) %>% 
  filter(!is.na(yo)) %>% 
  filter(!is.na(medium)) %>% 
  separate(
    medium, c("materials", "surface"), sep = " on ", extra = "merge",
    remove = FALSE
  ) %>% 
  filter(surface == "paper") %>% 
  filter(!is.na(materials)) %>% 
  mutate(materials_lst = str_split(materials, "(, (and )?| and )"))


#
tmp <- 
  df %>% 
  select(materials, materials_lst) %>% 
  filter(str_detect(materials, "and gouache")) %>% 
  distinct()


#
df_mat <- 
  df %>% 
  select(yo, materials_lst) %>% 
  unnest(materials_lst) %>% 
  rename(material = materials_lst) %>% 
  mutate(material = str_to_lower(material)) %>% 
  mutate(material = case_when(
    material == "waercolour" ~ "watercolour",
    TRUE ~ material
  )) %>% 
  mutate(mat = fct_lump_min(material, 1000))



df_mat %>% 
  count(material, sort = TRUE)

df_mat %>% 
  count(yo, mat, .drop = FALSE) %>% 
  #filter(mat != "graphite") %>% 
  ggplot(aes(yo, n, fill = mat)) +
  geom_area() +
  theme_minimal()





# simple plot to explain how much of all works is by this guy
# how much of this guy's works in on paper
# how much of this guy's works on paper are by graphite
