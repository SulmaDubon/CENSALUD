#---------------------------
# Diccionario respuestas
#---------------------------
diccionario_respuestas <- list(
  # Variables comunes con Sí = 1, No = 0
  comunes_si_no = c("Sí" = 1, "No" = 0),
  
  # Variables específicas con valores únicos
  MetDiag = c("Prueba" = 1, "sintomatología" = 2),
  BrotVec = c("Sí" = 1, "No" = 0, "Desconoce" = 2),
  MedVec = c("Desconoce" = 1, "consulta" = 2, "Ninguna" = 3),
  Mosq_Peri = c("Todo el año" = 1, "Época de lluvias" = 2),
  MsqFrcStd = c("Seguido" = 1, "Rara vez" = 2, "Nunca" = 3),
  Fum_frec = c("1-2 veces" = 1, "3-4 veces" = 2, "Más de 4 veces" = 3),
  Ult_Vis_A = c("1er trimestre" = 1, "2do trimestre" = 2, "3er trimestre" = 3),
  Fum_MotNo = c("No se encontraba en casa" = 1, "Alérgicos en la familia" = 2),
  Frec_Limp_Dep = c("1 vez" = 1, "2 veces" = 2, "3 veces" = 3, "Más de 4 veces" = 4),
  Abat_tiemp = c("Menos de 1 mes" = 1, "Alrededor de 2 meses" = 2, "Más de 3 meses" = 3),
  ParedTipo = c("Adobe" = 1, "Bloque" = 2, "Ladrillo" = 3, "Nahareque" = 4, "Madera" = 5, 
                "Láminas" = 6, "Otro" = 7),
  RepelloTipo = c("Cemento" = 1, "Lodo/tierra" = 2, "Parcialmente repellado" = 3, "Sin repello" = 4),
  TechoTipo = c("Palma" = 1, "Paja" = 2, "Lámina" = 3, "Duralita" = 4, "Zinc" = 5, 
                "Madera" = 6, "Tejado" = 7, "Otro" = 8),
  SueloTipo = c("Tierra" = 1, "Cemento" = 2, "Ladrillo" = 3, "Otro" = 4),
  EstrucAdic = c("Gallinero" = 1, "Porqueriza" = 2, "Cocina de leña/horno" = 3, 
                 "Conejera" = 4, "Trapiche" = 5, "Otro" = 6, "Ninguno" = 7),
  AguaTipo = c("ANDA" = 1, "De pozo" = 2, "Río" = 3, "Nacimiento" = 4, 
               "Pipa de agua" = 5, "Otro" = 6),
  FrecRecAg = c("1 vez" = 1, "2 veces" = 2, "3 o más veces" = 3),
  CubTipo = c("Ninguno" = 1, "Tapaderas" = 2, "Pedazos de plástico" = 3, 
              "Láminas" = 4, "Otro" = 5)
)


variables_si_no <- c(
  "CncTrans", "CncSint", "CncCont", "FamDiag", "DiagDengue", "DiagZika", "DiagChik",
  "FamHosp", "HospDengue", "HospZika", "HospChik", "Caso_6m", "AtenMed", "PruebLab",
  "AutoMed", "MedNat", "Mosq_uso", "Mosq_Insec", "MFN_5", "MFN5_7", "MFA18_40", 
  "MFA_40", "Malla_Uso", "Repel_Uso", "Charla", "Fum_Com", "Fum_Hogar", "Fum_Cree", 
  "Inf_prev", "Inf_Foll", "Inf_char", "Inf_tv_rad", "Inf_redsoc", "inf_otro", 
  "Resp_Soc", "Resp_Alc", "Resp_MINSAL", "Gote", "AlmBarril", "AlmCub", "AlmPila",
  "AlmOtro", "AlmNing", "ZancViv", "LarvViv", "RecBrl", "RecCub", "RecPila", "RecMct",
  "RecLlnts", "RecOtros", "RecNing", "H_Aeg", "M_Aeg", "H_albo", "M_albo", "Abat_Uso"
  
  
)

# Añadir al diccionario principal
for (var in variables_si_no) {
  diccionario_respuestas[[var]] <- diccionario_respuestas$comunes_si_no
}


#-----------------------------------
# Diccionario titulo grafico
#----------------------------------
dic_titulo_graf <- list(
  "CncTrans" = "Conocimiento sobre la transmisión del dengue",
  "CncSint" = "Conocimiento sobre síntomas del dengue",
  "CncCont" = "Conocimiento sobre control de poblaciones",
  "FamDiag" = "Diagnóstico previo de dengue, zika o chik en la familia",
  "DiagDengue" = "Diagnóstico de dengue",
  "DiagZika" = "Diagnóstico de zika",
  "DiagChik" = "Diagnóstico de chikungunya",
  "FamHosp" = "Hospitalización por dengue, zika o chik en la familia",
  "HospDengue" = "Hospitalización por dengue",
  "HospZika" = "Hospitalización por zika",
  "HospChik" = "Hospitalización por chikungunya",
  "MetDiag" = "Método de diagnóstico del dengue",
  "Caso_6m" = "Caso de dengue en los últimos 6 meses",
  "AtenMed" = "Atención médica por sospecha de dengue",
  "PruebLab" = "Prueba de laboratorio por sospecha de dengue",
  "AutoMed" = "Automedicación por sospecha de dengue",
  "MedNat" = "Uso de medicina natural por sospecha de dengue",
  "BrotVec" = "Brotes de dengue en el vecindario en los últimos 6 meses",
  "MedVec" = "Medidas tomadas en el vecindario ante brotes de dengue",
  "Mosq_uso" = "Uso de mosquiteros sobre las camas",
  "Mosq_Insec" = "Uso de mosquiteros impregnados con insecticida",
  "Mosq_Peri" = "Período de uso de mosquiteros",
  "MosqAguj" = "Mosquiteros poseen agujeros",
  "MsqFrcStd" = "Frecuencia revisa el estado del mosquitero",
  "MosqTds" = "Uso de mosquiteros todos los grupos de edad",
  "MFN_5" = "Uso de mosquiteros menores de 5 años",
  "MFN5_7" = "Uso de mosquiteros entre 5 y 7 años",
  "MFA18_40" = "Uso de mosquiteros entre 18 y 40 años",
  "MFA_40" = "Uso de mosquiteros mayores de 40 años",
  "Malla_Uso" = "Usa mallas en puertas y ventanas",
  "Repel_Uso" = "Utiliza de repelente",
  "Charla" = "recibido charla informativa sobre cómo prevenir el dengue",
  "Fum_Com" = "Campañas de fumigación en su comunidad en los últimos 6 meses",
  "Fum_frec" = "Frecuencia de las campañas de fumigación en los últimos 6 meses",
  "Ult_Vis_A" = "Última visita para fumigar su casa",
  "Fum_Hogar" = "Permitió que su hogar sea fumigado",
  "Fum_MotNo" = "Motivo por el que no permitió que se fumigara",
  "Fum_Cree" = "Cree que las fumigaciones son efectivas en reducir la cantidad de zancudos",
  "Frec_Limp_Dep" = "Frecuencia de limpieza de los depósitos donde se almacena agua",
  "Abat_Uso" = "Uso de abate en los depósitos donde almacena agua",
  "Abat_tiemp" = "Hace cuánto fueron colocadas las bolsas de abate",
  "Inf_prev" = "Recibir más información sobre cómo prevenir el dengue",
  "Inf_Foll" = "Recibir información en folleto",
  "Inf_char" = "Recibir información en charlas comunitarias",
  "Inf_tv_rad" = "Recibir información en programas de TV/radio",
  "Inf_redsoc" = "Recibir información por redes sociales",
  "inf_otro" = "Recibir información por otros medios",
  "Resp_Soc" = "Responsabilidad del control del dengue en la sociedad en general",
  "Resp_Alc" = "Responsabilidad del control del dengue en las Alcaldías",
  "Resp_MINSAL" = "Responsabilidad del control del dengue en el Ministerio de Salud",
  "N <5" = "Niños menores de 5 años",
  "N5-17" = "Niños entre 5 y 17 años",
  "A18_40" = "Adultos entre 18 y 40 años",
  "A >40" = "Adultos mayores de 40 años",
  "Embar" = "Mujeres embarazadas",
  "ParedTipo" = "Tipo de paredes de la vivienda",
  "RepelloTipo" = "Tipo de repello de la vivienda",
  "TechoTipo" = "Tipo de techo de la vivienda",
  "Gote" = "Presencia de goteras en el techo",
  "SueloTipo" = "Tipo de piso de la vivienda",
  "EstrucAdic" = "Estructuras adicionales en la vivienda",
  "AguaTipo" = "Tipo de agua utilizada en casa",
  "FrecRecAg" = "Frecuencia de recolección de agua",
  "AlmBarril" = "Almacena en barriles",
  "AlmCub" = "Almacena en cubetas",
  "AlmPila" = "Almacena en pilas",
  "AlmOtro" = "Almacena en otro",
  "AlmNing" = "No almacena agua",
  "CubTipo" = "Tipo de cubierta en los recipientes de almacenamiento de agua",
  "ZancViv" = "Presencia de zancudos adultos en la vivienda",
  "LarvViv" = "Presencia de larvas en la vivienda",
  "RecBrl" = "Recolecta larvas en barril",
  "RecCub" = "Recolecta larvas en cubetas",
  "RecPila" = "Recolecta larvas en pilas",
  "RecMct" = "Recolecta larvas en macetas",
  "RecLlnts" = "Recolecta larvas en llantas",
  "RecOtros" = "Recolecta larvas en otros recipientes",
  "RecNing" = "No recolecta larvas",
  "H_Aeg" = "Presencia de hembra Aegypti en la vivienda",
  "M_Aeg" = "Presencia de macho Aegypti en la vivienda",
  "H_albo" = "Presencia de hembra Albopictus",
  "M_albo" = "Presencia de macho Albopictus"
)




