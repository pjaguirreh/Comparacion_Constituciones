library(rvest)
library(RSelenium)

urls <- read.csv("../Constituciones/urls.csv", encoding = "utf-8")

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
    
  d <- texto_constituciones[[1]] %>% 
    mutate(pais = p, .before = value)
  
  datos_consolidado <- bind_rows(datos_consolidado, d)
}
