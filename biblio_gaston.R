gs4_deauth()
sheet_url <- "https://docs.google.com/spreadsheets/d/1Vn0MYaZwBshZLEeZCj67Jo4uWvZawI0emsd-26ru_0U/edit?usp=sharing"
data <- googlesheets4::read_sheet(sheet_url)


names(data)

articles_df <- data %>%
  select(input_url = `input_url`, 
         YEAR = `DC.Date.created`, 
         author = `DC.Creator.PersonalName`, 
         description = `DC.Description`, 
         title = `citation_title`, 
         keywords = `keywords`,
         institution = `citation_author_institution`)
glimpse(articles_df)



result_df <- articles_df %>%
  select(input_url, author, institution) %>%
  mutate(
    author_list = str_split(author, ","),
    institution_list = str_split(institution, ",")
  ) %>%
  rowwise() %>%
  mutate(
    match = length(author_list) == length(institution_list)
  ) %>%
  ungroup()


# Calcular el porcentaje de FALSE
sum(result_df$match)/nrow(result_df)
    
    
    paired = list(
      if(length(author_list) == length(institution_list)) {
        map2(author_list, institution_list, ~ list(author = str_trim(.x), institution = str_trim(.y)))
      } else {
        map(author_list, ~ list(author = str_trim(.x), institution = NA_character_))
      }
    )
  ) %>%
  unnest(paired) %>%
  unnest_wider(paired) %>%
  distinct()





# Tabla de palabras clave
keywords_df <- df %>%
  select(keywords = `keywords`) %>%
  filter(!is.na(keywords)) %>%
  mutate(keywords = str_split(keywords, ", "))

# Tabla de metodología y escalas
escalas_df <- df %>%
  select(description = `DC.Description`)

# Mostrar las primeras filas de cada tabla
head(articles_df)
head(autores_df)
head(keywords_df)
head(escalas_df)



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





