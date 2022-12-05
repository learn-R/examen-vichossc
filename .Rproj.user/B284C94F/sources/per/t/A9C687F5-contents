# Carga de paquete y datos ------------------------------------------------

pacman::p_load(tidyverse,
               haven,
               survey,
               srvyr,
               tidyr,
               survey,
               sjPlot,
               sjmisc)

data <- read_stata("input/ELSOC_W05_v1.0_Stata14.dta")

# Selección de variables y objeto encuesta --------------------------------

data.selecto <- select(data, fact_exp02,muestra, estrato,m0_edad, t10, c05_03, f05_07, f01_05 )

names(data.selecto) = c("fact_exp02", "muestra", "estrato", "edad", "inseguridad", "confianza_car", "just_violencia", "conflicto_inm")

objeto_encuesta <- data.selecto %>%
  as_survey_design(id = muestra,
                   strata = estrato,
                   weights = fact_exp02, nest= TRUE)

#t10:	Percepcion de seguridad del barrio
#f01_05 Percepcion de conflictos: Entre chilenos e inmigrantes peruanos
#f05_07:	Justificacion de violencia: Estudiantes tiren piedras a carabineros
#c05_03:	Grado de confianza: Carabineros
#m0_edad:	Edad del entrevistado


# Correlacion Inseguridad y Conflicto inmigrantes -------------------------------------------------

table(data.selecto$inseguridad, exclude=F)
table(data.selecto$conflicto_inm, exclude=F)

data.selecto$inseguridad <- na_if(data.selecto$inseguridad, -666)
data.selecto$inseguridad <- na_if(data.selecto$inseguridad, -999)

data.selecto$conflicto_inm <- na_if(data.selecto$conflicto_inm, -999)
data.selecto$conflicto_inm <- na_if(data.selecto$conflicto_inm, -888)

plot_frq(data.selecto$conflicto_inm)
plot_frq(data.selecto$inseguridad)

data.selecto <- mutate(data.selecto, inseguridad = case_when(inseguridad %in% c(1,2 )~"Inseguro",
                                                 inseguridad %in% c(4, 5)~"Seguro",
                                                 inseguridad %in% c(3)~"Ni seguro ni inseguro"))

data.selecto <- mutate(data.selecto, conflicto_inm = case_when(
  conflicto_inm>=2 & conflicto_inm<=3~"Conflictos menores", 
  conflicto_inm>=4 & conflicto_inm<=5~"Conflcitos mayores", 
  conflicto_inm==1~"Ningun conflicto"))


objeto_encuesta %>%
  group_by(conflicto_inm, inseguridad) %>%
  summarise(prop = survey_prop(vartype = "ci", level = .99, na.rm = TRUE)) %>%
  mutate(per = prop*100) %>%
  ungroup()


plot_xtab(data.selecto$conflicto_inm,data.selecto$inseguridad,  margin = "row", 
          bar.pos = "stack",
          title = "Correlacion inseguridad y conflicto",
          show.summary = TRUE, coord.flip = TRUE,
          show.n = F)

save_plot("output/Graficos/Correlacion inseguridad y conflicto.png", fig = last_plot())


# Correlacion inseguridad y justificacion de la violencia -----------------

table(data.selecto$just_violencia, exclude=F)

data.selecto$just_violencia <- na_if(data.selecto$just_violencia, -999)
data.selecto$just_violencia <- na_if(data.selecto$just_violencia, -888)

plot_frq(data.selecto$just_violencia)


data.selecto <- mutate(data.selecto, just_violencia = case_when(
 just_violencia>=2 & just_violencia<=3~"En ocasiones se justifica", 
  just_violencia>=4 & just_violencia<=5~"En general se justifica", 
 just_violencia==1~"Nunca se justifica"))

objeto_encuesta %>%
  group_by(just_violencia, inseguridad) %>%
  summarise(prop = survey_prop(vartype = "ci", level = .99, na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

plot_xtab(data.selecto$just_violencia,data.selecto$inseguridad,  margin = "row", 
          bar.pos = "stack",
          title = "Correlacion inseguridad y justificacion de la violencia",
          show.summary = TRUE, coord.flip = TRUE,
          show.n = F)

save_plot("output/Graficos/Correlacion inseguridad y justificacion de la violencia.png", fig = last_plot())

# Correlacion inseguridad y Confianza en Carabineros ----------------------

table(data.selecto$confianza_car, exclude=F)

data.selecto$confianza_car <- na_if(data.selecto$confianza_car, -999)
data.selecto$confianza_car <- na_if(data.selecto$confianza_car, -888)
data.selecto$confianza_car <- na_if(data.selecto$confianza_car, -666)

plot_frq(data.selecto$confianza_car)

data.selecto <- mutate(data.selecto, confianza_car = case_when(
  confianza_car==1~"Nada",
  confianza_car>=2 & confianza_car<=3~"Algo", 
  confianza_car>=4 & confianza_car<=5~"Bastante"))

objeto_encuesta %>%
  group_by(confianza_car, inseguridad) %>%
  summarise(prop = survey_prop(vartype = "ci", level = .99, na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

plot_xtab(data.selecto$confianza_car,data.selecto$inseguridad,  margin = "row", 
          bar.pos = "stack",
          title = "Correlacion inseguridad y confianza",
          show.summary = TRUE, coord.flip = TRUE,
          show.n = F)

save_plot("output/Graficos/Correlacion inseguridad y confianza.png", fig = last_plot())

sjt.xtab(data.selecto$confianza_car,data.selecto$inseguridad,
         show.col.prc = TRUE,
         show.summary = FALSE,
         encoding= "UTF-8",
         tittle= "Correlacion inseguridad y edad")

# Correlacion inseguridad y Edad ------------------------------------------

table(data.selecto$edad, exclude=F)

plot_frq(data.selecto$edad)

data.selecto <- mutate(data.selecto, edad = case_when(
  edad>=18 & edad<=26~"Adulto joven", 
  edad>=27 & edad<=59~"Adulto", 
  edad>=60 & edad<=92~"Adulto mayor"))


objeto_encuesta %>%
  group_by(edad, inseguridad) %>%
  summarise(prop = survey_prop(vartype = "ci", level = .99, na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

plot_xtab(data.selecto$edad, data.selecto$inseguridad, margin = "row", 
          bar.pos = "stack",
          title = "Correlacion inseguridad y edad",
          show.summary = TRUE, coord.flip = TRUE,
          show.n = F)

save_plot("output/Graficos/Correlacion inseguridad y edad.png", fig = last_plot())


sjt.xtab(data.selecto$edad,data.selecto$inseguridad,
         show.col.prc = TRUE,
         show.summary = FALSE,
         encoding= "UTF-8",
         tittle= "Correlacion inseguridad y edad")


# Guardar datos -----------------------------------------------------------

save(objeto_encuesta, data.selecto, file = "output/datos-proc.Rdata")
