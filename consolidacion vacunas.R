rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)
library(data.table)

setwd("D:/rfolder/covid/")


n01<-read.table("nomivac01.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n01)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n01)
table(n01$nombre_dosis_generica)


n01<-n01%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n01$nvacunas)

n01<-n01%>%
mutate(ds="DS01")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n02<-read.table("nomivac02.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n02)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n02)
table(n02$nombre_dosis_generica)

n02<-n02%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n02$nvacunas)

n02<-n02%>%
mutate(ds="DS02")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n03<-read.table("nomivac03.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n03)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n03)
table(n03$nombre_dosis_generica)

n03<-n03%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n03$nvacunas)

n03<-n03%>%
mutate(ds="DS03")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n04<-read.table("nomivac04.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n04)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n04)
table(n04$nombre_dosis_generica)

n04<-n04%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n04$nvacunas)

n04<-n04%>%
mutate(ds="DS04")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n05<-read.table("nomivac05.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n05)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n05)
table(n05$nombre_dosis_generica)

n05<-n05%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n05$nvacunas)

n05<-n05%>%
mutate(ds="DS05")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n06<-read.table("nomivac06.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n06)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n06)
table(n06$nombre_dosis_generica)

n06<-n06%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n06$nvacunas)

n06<-n06%>%
mutate(ds="DS06")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n07<-read.table("nomivac07.csv",
skip = 1 , 
header = FALSE, 
sep =',')


names(n07)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n07)
table(n07$nombre_dosis_generica)

n07<-n07%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n07$nvacunas)

n07<-n07%>%
mutate(ds="DS07")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n08<-read.table("nomivac08.txt",
skip=1,
header = FALSE, 
sep =',')

names(n08)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n08)
table(n08$nombre_dosis_generica)

n08<-n08%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())%>%
mutate(nvacunas=as.double(nvacunas))
sum(n08$nvacunas)

n08<-n08%>%
mutate(ds="DS08")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n09<-read.table("nomivac09.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n09)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n09)
table(n09$nombre_dosis_generica)

n09<-n09%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n09$nvacunas)

n09<-n09%>%
mutate(ds="DS09")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n10<-read.table("nomivac10.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n10)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n10)
table(n10$nombre_dosis_generica)

n10<-n10%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n10$nvacunas)

n10<-n10%>%
mutate(ds="DS10")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n11<-read.table("nomivac11.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n11)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n11)
table(n11$nombre_dosis_generica)

n11<-n11%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n11$nvacunas)

n11<-n11%>%
mutate(ds="DS11")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n12<-read.table("nomivac12.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n12)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n12)
table(n12$nombre_dosis_generica)

n12<-n12%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n12$nvacunas)

n12<-n12%>%
mutate(ds="DS12")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n13<-read.table("nomivac13.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n13)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n13)
table(n13$nombre_dosis_generica)

n13<-n13%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n13$nvacunas)

n13<-n13%>%
mutate(ds="DS13")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n14<-read.table("nomivac14.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n14)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n14)
table(n14$nombre_dosis_generica)

n14<-n14%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n14$nvacunas)

n14<-n14%>%
mutate(ds="DS14")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n15<-read.table("nomivac15.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n15)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n15)
table(n15$nombre_dosis_generica)

n15<-n15%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n15$nvacunas)

n15<-n15%>%
mutate(ds="DS15")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n16<-read.table("nomivac16.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n16)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n16)
table(n16$nombre_dosis_generica)

n16<-n16%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n16$nvacunas)

n16<-n16%>%
mutate(ds="DS16")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n17<-read.table("nomivac17.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n17)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n17)
table(n17$nombre_dosis_generica)

n17<-n17%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n17$nvacunas)

n17<-n17%>%
mutate(ds="DS17")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n18<-read.table("nomivac18.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n18)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n18)
table(n18$nombre_dosis_generica)

n18<-n18%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n18$nvacunas)

n18<-n18%>%
mutate(ds="DS18")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n19<-read.table("nomivac19.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n19)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n19)
table(n19$nombre_dosis_generica)

n19<-n19%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n19$nvacunas)

n19<-n19%>%
mutate(ds="DS19")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n20<-read.table("nomivac20.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n20)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n20)
table(n20$nombre_dosis_generica)

n20<-n20%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n20$nvacunas)

n20<-n20%>%
mutate(ds="DS20")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n21<-read.table("nomivac21.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n21)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n21)
table(n21$nombre_dosis_generica)

n21<-n21%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n21$nvacunas)

n21<-n21%>%
mutate(ds="DS21")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n22<-read.table("nomivac22.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n22)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n22)
table(n22$nombre_dosis_generica)

n22<-n22%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n22$nvacunas)

n22<-n22%>%
mutate(ds="DS22")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n23<-read.table("nomivac23.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n23)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n23)
table(n23$nombre_dosis_generica)

n23<-n23%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n23$nvacunas)

n23<-n23%>%
mutate(ds="DS23")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n24<-read.table("nomivac24.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n24)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n24)
table(n24$nombre_dosis_generica)

n24<-n24%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n24$nvacunas)

n24<-n24%>%
mutate(ds="DS24")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n25<-read.table("nomivac25.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n25)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n25)
table(n25$nombre_dosis_generica)

n25<-n25%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n25$nvacunas)

n25<-n25%>%
mutate(ds="DS25")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n26<-read.table("nomivac26.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n26)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n26)
table(n26$nombre_dosis_generica)

n26<-n26%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n26$nvacunas)

n26<-n26%>%
mutate(ds="DS26")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n27<-read.table("nomivac27.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n27)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n27)
table(n27$nombre_dosis_generica)

n27<-n27%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n27$nvacunas)

n27<-n27%>%
mutate(ds="DS27")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n28<-read.table("nomivac28.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n28)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n28)
table(n28$nombre_dosis_generica)

n28<-n28%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n28$nvacunas)

n28<-n28%>%
mutate(ds="DS28")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))




n29<-read.table("nomivac29.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n29)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n29)
table(n29$nombre_dosis_generica)

n29<-n29%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n29$nvacunas)

n29<-n29%>%
mutate(ds="DS29")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n30<-read.table("nomivac30.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n30)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n30)
table(n30$nombre_dosis_generica)

n30<-n30%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n30$nvacunas)

n30<-n30%>%
mutate(ds="DS30")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n31<-read.table("nomivac31.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n31)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n31)
table(n31$nombre_dosis_generica)

n31<-n31%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n31$nvacunas)

n31<-n31%>%
mutate(ds="DS31")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n32<-read.table("nomivac32.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n32)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n32)
table(n32$nombre_dosis_generica)

n32<-n32%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n32$nvacunas)

n32<-n32%>%
mutate(ds="DS32")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n33<-read.table("nomivac33.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n33)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n33)
table(n33$nombre_dosis_generica)

n33<-n33%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n33$nvacunas)

n33<-n33%>%
mutate(ds="DS33")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))


n34<-read.table("nomivac34.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n34)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n34)
table(n34$nombre_dosis_generica)

n34<-n34%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n34$nvacunas)

n34<-n34%>%
mutate(ds="DS34")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n35<-read.table("nomivac35.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n35)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n35)
table(n35$nombre_dosis_generica)

n35<-n35%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n35$nvacunas)

n35<-n35%>%
mutate(ds="DS35")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n36<-read.table("nomivac36.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n36)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n36)
table(n36$nombre_dosis_generica)

n36<-n36%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n36$nvacunas)

n36<-n36%>%
mutate(ds="DS36")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))




n37<-read.table("nomivac37.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n37)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n37)
table(n37$nombre_dosis_generica)

n37<-n37%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n37$nvacunas)

n37<-n37%>%
mutate(ds="DS37")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



n38<-read.table("nomivac38.csv",
skip = 1 , 
header = FALSE, 
sep =',')

names(n38)<-c("sexo",
"grupo_etario",
"jurisdiccion_residencia",
"jurisdiccion_residencia_id",
"depto_residencia",
"depto_residencia_id",
"jurisdiccion_aplicacion",
"jurisdiccion_aplicacion_id",
"depto_aplicacilson",
"depto_aplicacion_id",
"fecha_aplicacion",
"vacuna",
"cod_dosis_generica",
"nombre_dosis_generica",
"condicion_aplicacion",
"orden_dosis",
"lote_vacuna")

nrow(n38)
table(n38$nombre_dosis_generica)

n38<-n38%>%
filter(nombre_dosis_generica=="1ra" | nombre_dosis_generica=="Unica")%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(nvacunas=n())
sum(n38$nvacunas)

n38<-n38%>%
mutate(ds="DS38")%>%
mutate(idobs=paste(ds,jurisdiccion_aplicacion_id,fecha_aplicacion, sep=""))%>%
mutate(idobs=str_replace_all(idobs, "[^[:alnum:]]", ""))



vacunaspordia<-merge(n01, n02, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n03, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n04, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n05, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n06, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n07, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n08, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)


vacunaspordia<-merge(vacunaspordia, n09, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n10, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n11, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n12, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n13, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n14, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n15, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n16, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n17, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n18, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n19, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n20, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n21, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n22, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n23, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n24, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n25, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n26, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n27, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n28, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n29, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n30, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n31, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n32, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n33, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n34, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n35, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n36, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n37, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)

vacunaspordia<-merge(vacunaspordia, n38, by="idobs", all=T)
vacunaspordia<-vacunaspordia%>%
mutate(jurisdiccion_aplicacion_id=case_when(jurisdiccion_aplicacion_id.x=!is.na(jurisdiccion_aplicacion_id.x)~jurisdiccion_aplicacion_id.x,
jurisdiccion_aplicacion_id.y=!is.na(jurisdiccion_aplicacion_id.y)~jurisdiccion_aplicacion_id.y))%>%
mutate(fecha_aplicacion=case_when(fecha_aplicacion.x=!is.na(fecha_aplicacion.x)~fecha_aplicacion.x,
fecha_aplicacion.y=!is.na(fecha_aplicacion.y)~fecha_aplicacion.y))%>%
mutate(nvacunas=case_when(nvacunas.x=!is.na(nvacunas.x)~nvacunas.x,
nvacunas.y=!is.na(nvacunas.y)~nvacunas.y))%>%
select(idobs, fecha_aplicacion, jurisdiccion_aplicacion_id, nvacunas)



vacunaspordia2<-vacunaspordia%>%
group_by(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
summarise(aplicadas=sum(nvacunas))%>%
ungroup()

vacunaspordia2<-vacunaspordia2%>%
group_by(jurisdiccion_aplicacion_id)%>%
arrange(jurisdiccion_aplicacion_id, fecha_aplicacion)%>%
mutate(csumaplicadas=cumsum(aplicadas))%>%
ungroup()%>%
arrange(fecha_aplicacion, jurisdiccion_aplicacion_id)%>%
mutate(labeldistrito=case_when(jurisdiccion_aplicacion_id==6~"Buenos Aires",
jurisdiccion_aplicacion_id==2~"CABA",
jurisdiccion_aplicacion_id==10~"Catamarca",
jurisdiccion_aplicacion_id==22~"Chaco",
jurisdiccion_aplicacion_id==26~"Chubut",
jurisdiccion_aplicacion_id==18~"Corrientes",
jurisdiccion_aplicacion_id==14~"Cordoba",
jurisdiccion_aplicacion_id==30~"Entre Rios",
jurisdiccion_aplicacion_id==34~"Formosa",
jurisdiccion_aplicacion_id==38~"Jujuy",
jurisdiccion_aplicacion_id==42~"La Pampa",
jurisdiccion_aplicacion_id==46~"La Rioja",
jurisdiccion_aplicacion_id==50~"Mendoza",
jurisdiccion_aplicacion_id==54~"Misiones",
jurisdiccion_aplicacion_id==58~"Neuquen",
jurisdiccion_aplicacion_id==62~"Rio Negro",
jurisdiccion_aplicacion_id==66~"Salta",
jurisdiccion_aplicacion_id==70~"San Juan",
jurisdiccion_aplicacion_id==74~"San Luis",
jurisdiccion_aplicacion_id==78~"Santa Cruz",
jurisdiccion_aplicacion_id==82~"Santa Fe",
jurisdiccion_aplicacion_id==86~"S del Estero",
jurisdiccion_aplicacion_id==94~"T del Fuego",
jurisdiccion_aplicacion_id==90~"Tucuman"))

write_csv(vacunaspordia2, "vacunaspordia.csv")

