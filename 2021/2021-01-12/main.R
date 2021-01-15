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
# so let's just analyze him

# year of birth
yob <- 
  at %>% 
  filter(name == "Turner, Joseph Mallord William") %>% 
  pull(yearOfBirth)


df <- 
  at %>% 
  filter(name == "Turner, Joseph Mallord William") %>% 
  inner_join(aw, by = c("id" = "artistId")) %>% 
  filter(!is.na(year)) %>% 
  filter(!is.na(medium)) %>% 
  separate(
    medium, c("materials", "surface"), sep = " on ", extra = "merge",
    remove = FALSE
  ) %>% 
  filter(!is.na(materials)) %>% 
  mutate(materials_lst = str_split(materials, "(, (and )?| and )"))

# material counts by year
df_mat_by_year <- 
  df %>% 
  select(year, materials_lst) %>% 
  rowwise() %>% 
  mutate(w = 1/length(materials_lst)) %>% 
  ungroup() %>% 
  unnest(materials_lst) %>% 
  rename(material = materials_lst) %>% 
  mutate(material = str_to_lower(material)) %>% 
  mutate(material = case_when(
    material == "waercolour" ~ "watercolour",
    TRUE ~ material
  )) %>% 
  mutate(mat = fct_lump_min(material, 3000) %>%
           fct_infreq() %>% 
           fct_relevel("Other", after = Inf)) %>% 
  group_by(year, mat) %>% 
  summarise(across(w, sum))

# construct x axis labels
range(df_mat_by_year$year)
x_brks <- c(1787, 1800, 1820, 1840, 1856)
x_lbls <- paste0(x_brks, "\n", x_brks - yob, " y.o.")

colors <- c("9D8189","84A98C","577590")
colors <- paste0("#", colors)
names(colors) <- rev(levels(df_mat$mat))
text_color <- "gray70"
text_color_darker <- "gray60"
line_color <- "gray31"
bg_color <- "gray17"


df_mat_by_year %>% 
  mutate(mat = fct_rev(mat)) %>% 
  ggplot(aes(year, w, fill = mat, color = mat)) +
  geom_col(width = 1) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_x_continuous(
    breaks = x_brks,
    labels = x_lbls
  ) +
  scale_y_continuous(
    breaks = c(0, 1000, 2000),
    expand = c(0, 0)
  ) +
  xlab(NULL) +
  ylab("# of\nworks") +
  ggtitle("J. M. W. Turner artwork materials") +
  labs(caption = "Some works were created using multiple materials.
       If an artwork e.g. was created with watercolour and graphite, 0.5 is added to both materials counts.") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = bg_color),
    panel.grid.major = element_line(color = line_color),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = text_color),
    axis.title.y = element_text(
      size = 8, angle = 0, vjust = 0.37,
      color = text_color_darker
    ),
    text = element_text(color = text_color),
    legend.title = element_blank(),
    plot.title = element_text(color = text_color),
    plot.caption = element_text(color = text_color_darker, size = 8),
    legend.position=c(0,0.95), 
    legend.justification=c(0, 1), 
    legend.key.width=unit(1, "lines"), 
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
    axis.ticks.x = element_line(color = line_color),
    axis.ticks.length.x = unit(5, "pt"),
  )

ggsave(
  "2021/2021-01-12/plot.png",
  height = 6,
  width = 9
)
