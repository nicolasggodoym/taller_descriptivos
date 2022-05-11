rm(list = ls())
# Análisis univariado -----------------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               sjmisc, 
               sjPlot,
               ggrepel, #Para evitar solapamientos en grafico
               kableExtra) #Para tablas


# Cargar datos ------------------------------------------------------------

data = readRDS("output/data/enut.rds")

# Explorar ----------------------------------------------------------------

names(data)
# id = identificador
# exp = Ponderador
# varstrat = Pseudo-estrato de varianza
# varunit = Pseudo-conglomerado de varianza
# sexo
# quintil (factor)
# serv_nr_ds = servicios de trabajo no remunerado en día de semana (horas)
# sat_econ = satisfacción con situación económica
# sat_trab = satisfacción con trabajo
# sat_can_tl = satisfacción cantidad de tiempo libre
# sat_cal_tl = satisfacción calidad de tiempo libre
# traslado = al trabajo (horas)

# Análisis ----------------------------------------------------------------


# Tablas de frecuencias con frq() -----------------------------------------

frq(data$sexo)

# Personalicemos la tabla

frq(data$sexo,
    title = "Distribución de la variable sexo",
    show.na = F,
    out = "viewer",
    encoding = "UTF-8",
    sort.frq = "asc") 

# Exportamos

frq(data$sexo,
    title = "Distribucion de la variable sexo",
    show.na = F,
    encoding = "latin9",
    out = "viewer",
    sort.frq = "asc",
    file = "output/fig/frq_sexo.doc") 

# Con kableExtra

frq(data$sexo,
    show.na = F,
    encoding = "UTF-8",
    sort.frq = "asc") %>% 
  as.data.frame() %>% #Transformamos en dataframe para trabajar con dplyr 
  select(Sexo = val, 
         "F. absoluta" = frq,
         "F. relativa" = raw.prc,
         "F. rel. acumulada" = cum.prc) %>% 
  mutate_at(vars("F. relativa", "F. rel. acumulada"),
            ~(paste0(., "%"))) %>% #Incorporamos porcentaje %
  kable(caption = "Distribucion de la variable sexo",
      position = "center") %>% 
  kable_classic(full_width = F, html_font = "Cambria") 


# Tabla de MTC con descr() ------------------------------------------------

descr(data$traslado)

# Personalicemos la tabla

descr(data$traslado,
      show = c("n", "mean", "sd", "md", "range"))

# Presentemos dos variables

data %>% 
  select(traslado, serv_nr_ds) %>% 
  descr(show = c("label", "n", "mean", "sd", "md", "range"))
  
# Exportamos

data %>% 
  select(traslado, serv_nr_ds) %>% 
  descr(show = c("label", "n", "mean", "sd", "md", "range"),
        out = "viewer",
        file = "output/fig/traslado_nr.doc")

# Con kableExtra

data %>% 
  select(traslado, serv_nr_ds) %>% 
  descr(show = c("label", "n", "mean", "sd", "md", "range")) %>% 
  as.data.frame() %>% 
  select(Variable = var,
         Etiqueta = label,
         Media = mean,
         "D. estandar" = sd,
         Mediana = md, 
         Rango = range) %>% 
  mutate_at(vars(3, 4), ~(round(., digits = 3))) %>% 
  kable(caption = "MTC para horas de traslado y de trabajo no remunerado",
        position = "center") %>% 
  kable_classic(full_width = F, html_font = "Cambria") 

# Exportar manualmente desde el Viewer
# Export > Save as Image...
# Definir Directory la carpeta output/fig
# Dar un nombre al archivo (por ej., kable) 

# Likert ------------------------------------------------------------------

plot_likert(data %>% select(starts_with("sat_")),
                            title = "Niveles de satisfacción con uso del tiempo",
                            catcount = 5,
                            reverse.scale = T) +
  geom_text_repel() # Para no solapar etiquetas



