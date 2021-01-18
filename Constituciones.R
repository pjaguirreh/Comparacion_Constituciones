library(rvest)
library(tidyverse)
library(stringi)

urls <- read.csv("urls.csv", encoding = "utf-8")

extraer_texto <- function(x){
  
  read_html(x) %>% 
    html_nodes("p , span , .float-left") %>% 
    html_text() %>% 
    enframe()
  
}

texto_constituciones <- map(urls$url, extraer_texto) %>% 
  map(filter, value != "") %>% 
  map(select, -name)

datos_consolidado <- tibble()
for (i in seq_along(urls$País)){
  
  p <- urls$País[i]
    
  d <- texto_constituciones[[i]] %>% 
    mutate(pais = p, .before = value)
  
  datos_consolidado <- bind_rows(datos_consolidado, d)
  
}

datos_consolidado <- datos_consolidado %>% 
  group_by(pais) %>% 
  slice(-1) %>% 
  mutate(cap = ifelse(str_detect(value, "CAPÍTULO"), 1, 0),
         sec = ifelse(str_detect(value, "SECCIÓN"), 1, 0),
         tit = ifelse(str_detect(value, "TÍTULO"), 1, 0),
         art = ifelse(str_detect(value, "Artículo"), 1, 0),
         largo = ifelse(str_length(value)>14, 1, 0)
  )

datos_consolidado %>% 
  filter(cap == 0, 
         sec == 0,
         tit == 0, 
         art == 0,
         largo == 1) %>% 
  select(pais, texto = value) %>% 
  write_excel_csv("comparacion_constituciones.csv")
