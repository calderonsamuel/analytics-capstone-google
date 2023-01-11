library(tidyverse)
library(readxl)
library(skimr)

investigadores <- read_excel("data/investigadores.xlsx") |> 
    janitor::clean_names()

# shorter names and rm empty row
investigadores <- investigadores |> 
    rename(
        institucion_principal = institucion_laboral_principal_fuente_ct_ivitae,
        institucion_actual = institucion_laboral_actual_fuente_ct_ivitae,
        condicion = renacyt_2021,
        fecha_constancia = x7,
        nivel = x8,
        calificacion_previa = ver_calificacion_previa_reglamneto_2018,
        area_ocde = area_ocde_fuente_cti_vitae,
        url = url_cti_vitae
    ) |> 
    filter(row_number() != 1)


# this shows potential need of cleaning in the following fields:
# reglamento: only 2 uniques. convert to factor
# condicion: to factor
# fecha_constancia: to date
# nivel: to factor
# calificacion_previa: needs more inspecting, shouldn't have that many uniques
# area_ocde: needs more inspecting, shouldn't have that many uniques
# genero: to factor

investigadores |> 
    skim()

# Después de inspecionar calificacion_previa, se concluye que no vale la pena 
# seguir trabajando con esa columna

investigadores <- investigadores |> select(-calificacion_previa)

# Al inspeccionar area_ocde se ve que son múltiples valores por investigadores, 
# se trabaja en tabla aparte conservando codigo de investigador para poder hacer join

ocde <- investigadores |> 
    select(codigo_renacyt, area_ocde) |> 
    separate_rows(area_ocde, sep = "\\|") |> 
    filter(area_ocde != "")

investigadores <- investigadores |> select(-area_ocde)

# type convertion with minimal cleaning
# TODO: specify in app `reglamento` is linked to normative changes
# TODO: specify in app `fecha_constacia` only has values when `reglamento` == 2021
investigadores <- investigadores |> 
    mutate(
        reglamento = if_else(str_detect(reglamento, "2018"), 2018, 2021) |> as.factor(),
        fecha_constancia = lubridate::dmy(fecha_constancia),
        condicion = case_when(
            condicion == "VIGENCIA VENCIDA" ~ "Vig. Vencida",
            condicion == "-" ~ "Sin info",
            TRUE ~ condicion
        ) |> factor(c("Sin info", "Activo", "No Activo", "Vig. Vencida")),
        nivel = if_else(nivel == "-", "Sin info", nivel) |> 
            factor(c("Sin info", "I", "II", "III", "IV", "V", 
                     "VI", "VII", "VIII", "IX", "X", "Investigador Distinguido")),
        genero = as.factor(genero)
    ) 

# add 'Sin institución' case for missing data

investigadores <- investigadores |> 
    mutate(
        institucion_principal = replace_na(institucion_principal, "SIN INFO DE INSTITUCIÓN"),
        institucion_actual = replace_na(institucion_actual, "SIN INFO DE INSTITUCIÓN")
    ) |> 
    mutate(
        institucion_principal_fct = institucion_principal |> 
            fct_lump_n(200, other_level = "OTROS") |> 
            fct_infreq() |> 
            fct_relabel(~str_trunc(.x, 45))
    )

investigadores |> 
    write_rds("data/investigadores_clean.rds")

ocde |> 
    write_rds("data/ocde.rds")
