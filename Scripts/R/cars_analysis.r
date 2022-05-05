
## mydata <- read.csv(file='protesis.csv',sep=',',header=T)
## summary(mydata)
protesis <- read.csv(file='protesis.csv',sep=';',header=T)

summary(protesis)

asegurador_calidad <- table(protesis$asegurador,protesis$calidad_servicio)
asegurador_calidad <- table(protesis$asegurador,protesis$calidad_servicio)
asegurador_completa <- table(protesis$asegurador,protesis$rehabilitacion_completa)

amputacion_horas <- table(protesis$amputacion,protesis$horas_uso)

actividad_completa <- table(protesis$actividad_fisica,protesis$rehabilitacion_completa)
causa_completa <- table(protesis$causa_amputacion,protesis$rehabilitacion_completa)

mosaicplot(actividad_completa)

atencion_asegurador_completa <- table(protesis$mala_atencion_asegurador,protesis$rehabilitacion_completa)

edad_completa <- table(protesis$edad,protesis$rehabilitacion_completa)

edad_inorportunidad <- table(protesis$edad,protesis$rehabilitacion_completa)

summary(protesis$edad)


summary(protesis$edad)

hist(protesis$edad)


## analisis final

protesis <- read.csv(file='protesisedad.csv',sep=';',header=T)

summary(protesis)

edad_tipo_amp <- table(protesis$edad,protesis$amputacion)
edad_tipo_amp 

edad_completitud <- table(protesis$edad,protesis$rehabilitacion_completa)
edad_completitud 

edad_calidadservicio <- table(protesis$edad,protesis$calidad_servicio)
edad_calidadservicio 

edad_asegurador <- table(protesis$edad,protesis$aseguradora)
edad_asegurador 

edad_geografia <- table(protesis$edad,protesis$lejos)
edad_geografia 

edad_mala_ips <- table(protesis$edad,protesis$mala_atencion_ips)
edad_mala_ips 

edad_mala_asegurado <- table(protesis$edad,protesis$mala_atencion_asegurador)
edad_mala_asegurado 

edad_comunicacion_prestador <- table(protesis$edad,protesis$falla_comunicacion_prestador)
edad_comunicacion_prestador 

edad_bar_economicas <- table(protesis$edad,protesis$barreras_economicas)
edad_bar_economicas 

edad_horas_uso <- table(protesis$edad,protesis$horas_uso)
edad_horas_uso 

edad_insu_equipos <- table(protesis$edad,protesis$insuficiente_atencion)
edad_insu_equipos 

 

aseguradora_calidadservicio <- table(protesis$aseguradora,protesis$calidad_servicio)
aseguradora_calidadservicio 

aseguradora_com_inoportuno <- table(protesis$aseguradora,protesis$comienzo_inoportuno_terapia)
aseguradora_com_inoportuno 

aseguradora_completitud <- table(protesis$aseguradora,protesis$rehabilitacion_completa)
aseguradora_completitud 

aseguradora_falla_com <- table(protesis$aseguradora,protesis$falla_comunicacion_prestador)
aseguradora_falla_com 

aseguradora_insu_equipos <- table(protesis$aseguradora,protesis$insuficiente_atencion)
aseguradora_insu_equipos 

## 2021-06-03

act.fisica_horas.uso <- table(protesis$actividad_fisica,protesis$horas_uso)
act.fisica_horas.uso

act.fisica_alt.amputacion <- table(protesis$actividad_fisica,protesis$amputacion)
act.fisica_alt.amputacion 

asegurado_tramites <- table(protesis$aseguradora,protesis$muchos_tramites_asegurador)
asegurado_tramites 

edad_tramites <- table(protesis$edad,protesis$muchos_tramites_asegurador)
edad_tramites 

calidad.servicio_falla.equipos <- table(protesis$calidad_servicio,protesis$insuficiente_atencion)
calidad.servicio_falla.equipos 

act.fisica_horas.uso_rehab.completa <- table(protesis$actividad_fisica,protesis$horas_uso,protesis$rehabilitacion_completa)
ftable(act.fisica_horas.uso_rehab.completa)
ftable(addmargins(prop.table(act.fisica_horas.uso_rehab.completa, c(1, 2)), 3))*100
ftable(addmargins(prop.table(act.fisica_horas.uso_rehab.completa, margin=2), 3))*100

edad_horas.uso_rehab.completa <- table(protesis$edad,protesis$horas_uso,protesis$rehabilitacion_completa)
ftable(edad_horas.uso_rehab.completa)
ftable(addmargins(prop.table(edad_horas.uso_rehab.completa, c(1, 2)), 3))*100
ftable(addmargins(prop.table(edad_horas.uso_rehab.completa, margin=2), 3))*100

amputacion_horas.uso_rehab.completa <- table(protesis$amputacion,protesis$horas_uso,protesis$rehabilitacion_completa)
ftable(addmargins(prop.table(amputacion_horas.uso_rehab.completa, margin=2), 3))*100
ftable(addmargins(prop.table(amputacion_horas.uso_rehab.completa, margin=1), 3))*100
ftable(amputacion_horas.uso_rehab.completa)

amputacion_act.fisica_rehab.completa <- table(protesis$amputacion,protesis$actividad_fisica,protesis$rehabilitacion_completa)
ftable(amputacion_act.fisica_rehab.completa)
ftable(addmargins(prop.table(amputacion_act.fisica_rehab.completa, margin=1), 3))*100

## 2021-06-05

amputacion_act.fisica_rehab.completa <- table(protesis$amputacion,protesis$actividad_fisica,protesis$rehabilitacion_completa)
ftable(amputacion_act.fisica_rehab.completa)
ftable(addmargins(prop.table(amputacion_act.fisica_rehab.completa, margin=1), 3))*100

aseguradora_calidad.atencion_rehab.completa <- table(protesis$aseguradora,protesis$calidad_servicio,protesis$rehabilitacion_completa)
ftable(aseguradora_calidad.atencion_rehab.completa)

aseguradora_calidad.atencion_falla.comunicacion <- table(protesis$aseguradora,protesis$calidad_servicio,protesis$falla_comunicacion_prestador)
ftable(aseguradora_calidad.atencion_falla.comunicacion)

aseguradora_calidad.atencion_mala.atencion.asegurador <- table(protesis$aseguradora,protesis$calidad_servicio,protesis$mala_atencion_asegurador)
ftable(aseguradora_calidad.atencion_mala.atencion.asegurador)

aseguradora_calidad.atencion_mala.atencion.ips <- table(protesis$aseguradora,protesis$calidad_servicio,protesis$mala_atencion_ips)
ftable(aseguradora_calidad.atencion_mala.atencion.ips)

edad_barrera.geog_barrera.econ <- table(protesis$edad,protesis$lejos,protesis$barreras_economicas)
ftable(edad_barrera.geog_barrera.econ)
 
edad_uso.protesis_op.atencion <- table(protesis$edad,protesis$horas_uso,protesis$comienzo_inoportuno_terapia)
ftable(edad_uso.protesis_op.atencion)

edad_uso.protesis_amputacion <- table(protesis$edad,protesis$horas_uso,protesis$amputacion)
ftable(edad_uso.protesis_amputacion)

act.fisica_uso.protesis_amputacion <- table(protesis$actividad_fisica,protesis$horas_uso,protesis$amputacion)
ftable(act.fisica_uso.protesis_amputacion)
