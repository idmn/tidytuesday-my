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
df %>% 
  count(year, sort = TRUE)


#
tmp <- 
  df %>% 
  select(materials, materials_lst) %>% 
  filter(str_detect(materials, "and gouache")) %>% 
  distinct()


#
df_mat <- 
  df %>% 
  select(yo, year, materials_lst) %>% 
  unnest(materials_lst) %>% 
  rename(material = materials_lst) %>% 
  mutate(material = str_to_lower(material)) %>% 
  mutate(material = case_when(
    material == "waercolour" ~ "watercolour",
    TRUE ~ material
  )) %>% 
  # mutate(mat = fct_lump_min(material, 1000) %>% fct_infreq())
  mutate(mat = fct_lump_min(material, 3000) %>%
           fct_infreq() %>% 
           fct_relevel("Other", after = Inf))



df_mat %>% 
  count(material, sort = TRUE)

df_mat %>% 
  mutate(mat = fct_rev(mat)) %>% 
  count(yo, mat, .drop = FALSE) %>% 
  # filter(mat != "graphite") %>% 
  # filter(mat == "watercolour") %>% 
  ggplot(aes(yo, n, fill = mat)) +
  geom_area(position = "stack") +
  #facet_wrap(~mat, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(
    "graphite" = "#f94144",
    "watercolour" = "#f3722c",
    "gouache" = "#f9c74f",
    "ink" = "#90be6d",
    "chalk" = "#43aa8b",
    "pen" = "#4d908e",
    "Other" = "#277da1"
  )) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray30"),
    panel.grid.major = element_line(color = "gray70"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "grey80")
  )

# colors <- c("#f3722c","#f8961e","#f9c74f","#90be6d","#43aa8b","#4d908e","#277da1")
colors <- c("f3722c","43aa8b","f8961e","f9c74f","277da1","90be6d","4d908e")
colors <- paste0("#", colors)
names(colors) <- rev(levels(df_mat$mat))

colors <- c("9D8189","84A98C","577590")
colors <- paste0("#", colors)
names(colors) <- rev(levels(df_mat$mat))
text_color <- "grey90"

df_mat %>% 
  mutate(mat = fct_rev(mat)) %>% 
  ggplot(aes(year, fill = mat, color = mat)) +
  geom_bar(width = 1) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("J. M. W. Turner artworks on paper") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray20"),
    panel.grid.major = element_line(color = "gray31"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = text_color),
    legend.text = element_text(color = text_color),
    legend.title = element_blank(),
    plot.title = element_text(color = text_color)
  )

# need two axes here: one for the guy's age, one for year
# additional marks for high years
# what about counts? some works are counted twice


# simple plot to explain how much of all works is by this guy
# how much of this guy's works in on paper
# how much of this guy's works on paper are by graphite
