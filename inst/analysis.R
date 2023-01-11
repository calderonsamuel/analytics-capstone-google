library(tidyverse)

investigadores <- read_rds("data/investigadores_clean.rds")

investigadores |> 
    filter(str_detect(institucion_principal, "UNIVERSIDAD")) |> 
    count(institucion_principal, sort = T)


