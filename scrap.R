library(ojsr)
library(tidyverse)
library(googlesheets4)

journal_url <- "https://publicaciones.sociales.uba.ar/index.php/psicologiasocial"
issues <- ojsr::get_issues_from_archive(journal_url)
articles <- ojsr::get_articles_from_issue(input_url = issues$output_url, 
                                          verbose = TRUE)
meta <- ojsr::get_html_meta_from_article(articles$output_url, verbose = TRUE)

glimpse(meta)

table(meta$meta_data_name)


meta2 <- meta %>% 
  select(-meta_data_scheme,-meta_data_xmllang) %>%
  # filter(meta_data_name %in% campos) %>%
  filter(!is.na(meta_data_content)) %>% 
  group_by(input_url, meta_data_name) %>%
  summarize(meta_data_content = paste(meta_data_content, collapse = ", "), .groups = 'drop') %>%
  pivot_wider(names_from = meta_data_name, values_from = meta_data_content) 

glimpse(meta2)

gs4_auth()

# gs4_deauth()

sheet_url <- "https://docs.google.com/spreadsheets/d/1Vn0MYaZwBshZLEeZCj67Jo4uWvZawI0emsd-26ru_0U/edit?usp=sharing"
sheet_write(meta2, ss = sheet_url, sheet = "Sheet1")


# leer -----------------

gs4_deauth()
data <- googlesheets4::read_sheet(sheet_url)
glimpse(data)

keywords_long <- data %>%
  separate_rows(keywords, sep = "[,;]") %>%
  mutate(keywords = str_trim(keywords)) %>%
  select(input_url, keywords) %>%
  pivot_longer(cols = -input_url, 
               names_to = "xxx", values_to = "key") %>%
  select(-xxx) %>%
  mutate(key=tolower(key) %>% trimws() ) %>%
  filter(!is.na(key)) %>%
  mutate(key = str_replace_all(key, "[^[:alnum:][:space:]]", "")) %>%
  mutate(key = chartr("áéíóú", "aeiou", key))


keywords_long %>% count(key, sort = TRUE) %>% filter(n>1) %>% mutate (t=paste(key,n, collapse = " ")) %>%
  pull(t) %>% head(n = 1)
  




