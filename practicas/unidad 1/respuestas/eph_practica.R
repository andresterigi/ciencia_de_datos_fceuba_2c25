#carga de librerías
library(tidyverse)
library(wooldridge)
#carga de datos
setwd(r'(C:\Users\andre\OneDrive\Documentos\cienciadatos\tidyverse\eph)')
instub <- 'instub'     # Carpeta de datos de entrada
outstub <- 'outstub'   # Carpeta de resultados

# Crear carpetas si no existen
if (!dir.exists(instub)) {
  dir.create(instub, recursive = TRUE)
}

if (!dir.exists(outstub)) {
  dir.create(outstub, recursive = TRUE)
}
#definir nombre del archivo
eph_txt<- 'usu_individual_T324.txt'
# Archivos de texto delimitado
datos_txt <- read_delim(file.path(instub, eph_txt), delim = ";")

#análisis por arriba
glimpse(datos_txt)
head(datos_txt, 10)
summary(datos_txt)
#ejercicio 1. Seleccion y filtrado basico
#Selecciona las variables CODUSU, NRO_HOGAR, COMPONENTE, CH04 (sexo), CH06
#(edad), NIVEL_ED (nivel educativo), CH15 (lugar de nacimiento), ESTADO (condición de actividad), P21
#(ingreso de la ocupación principal) y PONDERA (factor de expansión). Luego filtra para considerar solo
#personas de 25 años o más.
datos_filtrado<-datos_txt%>%filter(CH06>=25)%>%select(CODUSU,NRO_HOGAR,COMPONENTE,CH04,CH06,NIVEL_ED,CH15,ESTADO,P21,PONDERA)

head(datos_filtrado,10)

#Ejercicio 2: Creación de variables categóricas
#Consigna: A partir del conjunto de datos obtenido en el ejercicio anterior, crea las siguientes variables
#nuevas utilizando mutate() y case_when():
class(datos_filtrado$CH04)
datos_nuevas<-datos_filtrado%>%mutate(sexo_desc=case_when(CH04==1~"Varon",CH04==2~"Mujer")
                                      ,nivel_educativo=case_when(
                                        
                                        NIVEL_ED==1~ "Primaria Incompleta(incluye educación especial)",
                                        NIVEL_ED==2 ~ "Primaria Completa",
                                        NIVEL_ED==3 ~ "Secundaria Incompleta",
                                        NIVEL_ED==4 ~ "Secundaria Completa",
                                        NIVEL_ED==5 ~ "Superior Universitaria Incompleta",
                                        NIVEL_ED==6~ "Superior Universitaria Completa",
                                        NIVEL_ED== 7 ~ "Sin instrucción",
                                        NIVEL_ED== 9~ "Ns./ Nr.",
                                        TRUE ~ "Otro"
                                      ),lugar_nacimiento=case_when(
                                        CH15==1~"En esta localidad",
                                        CH15==2~"En otra localidad",
                                        CH15==3~"En otra provincia",
                                        CH15==4~"En un país limítrofe",
                                        CH15==5~"En otro país",
                                        CH15==6~"N/S.N/R."
                                      ),condicion_actividad=case_when(
                                        ESTADO==0~"N/S.N/R.",
                                        ESTADO==1~"Ocupado",
                                        ESTADO==2~"Desocupado",
                                        ESTADO==3~"Inactivo",
                                        ESTADO==4~"Menor a 10 años"
                                      ))

#Ejercicio 3: Filtrado múltiple
#Consigna: Filtra los datos para analizar solo a las personas ocupadas (ESTADO == 1) 
#que tienen entre
#30 y 60 años, y que hayan nacido en otra provincia o en el extranjero.
datos_ocupados<-datos_nuevas%>%filter(ESTADO==1,CH06>=30,CH06<60,CH15==3|CH15==4|CH15==5)

#Ejercicio 4: Ordenamiento de datos
#Consigna: Ordena los datos de manera descendente por nivel educativo (NIVEL_ED) y luego de manera
#ascendente por edad (CH06). Muestra los primeros 15 registros del resultado.

datos_ordenados<-datos_nuevas%>%arrange(desc(NIVEL_ED),CH06)%>%head(15)

#Ejercicio 5: Estadísticas por nivel educativo
#Consigna: Agrupa los datos por nivel educativo y calcula:
#  • La cantidad de personas (ponderada)
#• La edad promedio (ponderada)
#• El ingreso promedio de la ocupación principal (ponderado)
#• El porcentaje de ocupados 
#Ordena los resultados por ingreso promedio de manera descendente.

datos_agrupados<-datos_nuevas%>% group_by(nivel_educativo)%>%summarise(
  n_personas=n(),
  edad_promedio=round(weighted.mean(CH06, PONDERA, na.rm = TRUE),2),
  ingreso_promedio=round(weighted.mean(P21, PONDERA, na.rm = TRUE),2),
  porc_ocupados = 100 * sum(PONDERA[ESTADO == 1], na.rm = TRUE) / sum(PONDERA, na.rm = TRUE)
)%>% arrange(desc(ingreso_promedio))
print(datos_agrupados)

#Ejercicio 6: Estadísticas por lugar de nacimiento
#Consigna: Agrupa los datos por lugar de nacimiento y calcula:
#  • La cantidad de personas (ponderada)
#• El porcentaje de personas con educación superior completa
#• El ingreso promedio de la ocupación principal (ponderado)
#• La edad promedio (ponderada)
#Ordena los resultados por porcentaje de educación superior de manera descendente.

datos_agrupados_lugar<-datos_nuevas%>% group_by(lugar_nacimiento)%>%
  summarise(n_personas=sum(PONDERA, na.rm = TRUE),
            porc_superior_completa=100*sum(PONDERA[NIVEL_ED==6], na.rm = TRUE)/sum(PONDERA, na.rm= TRUE),
            INGRESO_PROMEDIO=round(weighted.mean(P21,PONDERA,na.rm=TRUE),2),
            edad_promedio=round(weighted.mean(CH06,PONDERA,na.rm=TRUE))
            )%>%arrange(desc(porc_superior_completa))
print(datos_agrupados_lugar)

#Ejercicio 7: Medidas de dispersión por nivel educativo
#Consigna: Para las personas ocupadas, calcula por nivel educativo:
#  • El desvío estándar del ingreso
#• El coeficiente de variación (desvío estándar / media * 100)
#• El ingreso mínimo y máximo
#• El rango de ingresos (máximo - mínimo)
# Ordena los resultados por coeficiente de variación.
ejercicio_7<-datos_nuevas%>%filter(ESTADO==1)%>%
  filter(P21 >= 0) %>%
  group_by(nivel_educativo)%>%
  summarise(
    desvio_estandar=sd(P21, na.rm = TRUE),
    coef_var=desvio_estandar/mean(P21,na.rm=TRUE)*100,
    ingreso_min=min(P21,na.rm=TRUE),
    ingreso_max=max(P21,na.rm=TRUE),
    rango_ingresos=ingreso_max-ingreso_min)%>%
  arrange(coef_var)
  print(ejercicio_7)
  
#Ejercicio 8: Análisis por sexo y nivel educativo
#  Consigna: Agrupa los datos por sexo y nivel educativo, y calcula:
#    • La cantidad de personas (ponderada)
#  • El ingreso promedio (ponderado)
#  • La tasa de ocupación (porcentaje de ocupados)
#  Luego calcula la brecha de ingresos entre varones y mujeres para cada nivel educativo.  
  datos_agrupados <- datos_nuevas %>%
    filter(P21 >= 0) %>% 
    group_by(sexo_desc, nivel_educativo) %>%
    summarise(
      n_personas = sum(PONDERA, na.rm = TRUE),
      ingreso_promedio = weighted.mean(P21, PONDERA, na.rm = TRUE),
      tasa_ocupacion = 100 * sum(PONDERA[ESTADO == 1], na.rm = TRUE) / sum(PONDERA, na.rm = TRUE),
      .groups = "drop" 
    )
  
  print(datos_agrupados)
  
 # Ejercicio 9: Cuartiles de ingreso por nivel educativo
#  Consigna: Para las personas ocupadas, calcula por nivel educativo:
#    • El primer cuartil de ingresos (P25)
#  • La mediana de ingresos (P50)
#  • El tercer cuartil de ingresos (P75)
#  • El rango intercuartílico (IQR = P75 - P25)
#  Ordena los resultados por mediana de ingresos.
  
  percentiles_importantes<-datos_nuevas%>%filter(ESTADO==1)%>%
                                                   filter(P21 >= 0) %>%
    group_by(nivel_educativo)%>%summarise(
      primer_cuartil=round(quantile(P21,0.25),2),
      mediana_ingresos=median(P21,na.rm=TRUE),
      tercer_cuartil=round(quantile(P21,0.7,),2),
      rango_intercuantilico=tercer_cuartil-primer_cuartil)%>%
    arrange(mediana_ingresos)
  print(percentiles_importantes)
  
#  Ejercicio 10: Estadísticas por lugar de nacimiento y sexo
 # Consigna: Agrupa los datos por lugar de nacimiento y sexo, y calcula:
#    • La cantidad de personas (ponderada)
#  • El porcentaje de personas con nivel secundario completo o superior
#  • La edad promedio (ponderada)
#  • La tasa de actividad (porcentaje de personas económicamente activas)
#  Finalmente, elimina la agrupación y ordena los resultados por lugar de nacimiento y luego por tasa de
#  actividad de manera descendente.
  
  ejercicio_10 <- datos_nuevas %>%
    mutate(
      educados = NIVEL_ED > 2 & NIVEL_ED < 7,
      pea = ESTADO == 1 | ESTADO == 2
    ) %>%
    group_by(lugar_nacimiento, sexo_desc) %>%
    summarise(
      cantidad_personas = sum(PONDERA, na.rm = TRUE),
      porcentaje_educados = round(100 * mean(educados, na.rm = TRUE), 1),
      edad_promedio = round(weighted.mean(CH06, PONDERA, na.rm = TRUE), 2),
      tasa_actividad = round(100 * mean(pea, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(lugar_nacimiento, desc(tasa_actividad))  
  print(ejercicio_10)
  
 # Desafío final (Opcional)
#  Consigna: Combina las funciones de Tidyverse para crear un análisis que responda 
  #a la siguiente pregunta:
#    ¿Cómo varían los ingresos y la educación según el lugar de nacimiento y qué 
  #grupos tienen mayor desigualdad  interna?
  
#    Para ello deberás:
#    1. Filtrar solo personas ocupadas entre 25 y 65 años
#  2. Agrupar por lugar de nacimiento y calcular estadísticas de ingresos (promedio, mediana, desvío, CV)
#  3. Calcular la proporción de personas con diferentes niveles educativos por lugar de nacimiento
#  4. Ordenar los resultados según algún criterio relevante
  
desafio_final<-datos_nuevas %>% filter(ESTADO==1)%>%
  filter(CH06 >= 26|CH06<65)%>%group_by(lugar_nacimiento)%>%summarise(
    ingreso_promedio = round(weighted.mean(P21, PONDERA, na.rm = TRUE),2),
    mediana_ingreso=median(P21,na.rm=TRUE),
    desvio_salarial=round(sd(P21,na.rm=TRUE),2),
    coef_variacion = round(desvio_salarial / ingreso_promedio, 2),
    .groups = "drop"
    
  )
print(desafio_final)

analisis_educacion <- datos_nuevas %>%
  group_by(lugar_nacimiento, nivel_educativo) %>%
  summarise(
    cantidad_personas = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    proporcion = round(cantidad_personas / sum(cantidad_personas),2)
  ) %>%
  ungroup() %>%
  arrange(lugar_nacimiento, desc(proporcion))
print(analisis_educacion)