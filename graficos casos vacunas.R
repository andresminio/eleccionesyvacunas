#CARGAR BIBLIOTECAS
library(tidyverse)
library(wesanderson)


#IMPORAR DATA EN CSV 
datacasos<-read_csv("casos.csv", 
na=c(""," ", "NA", "."),
locale=locale(encoding="latin1"))

datavacunas<-read_csv("vacunas.csv", 
na=c(""," ", "NA", "."),
locale=locale(encoding="latin1"))

datavacunas$fecha<- as.Date(datavacunas$fecha, "%Y/%m/%d") 



#vacunas 
(vacunas<-datavacunas%>%
ggplot()+
geom_smooth(aes(as.Date(fecha),vacunadostot100), se=F, color="#b6ba4c")+
facet_wrap(~distrito,ncol=6)+
theme_minimal()+
labs(y="Vacunados cada 100 habitantes", x="",
caption="Fuente: CNE con datos del Ministerio de Salud (2022)")+
ggtitle("Esquemas de vacunación iniciados cada 100 habitantes",
subtitle="(Septiembre-diciembre 2021, por distrito)")+
theme(plot.caption=element_text(hjust=0),
plot.title=element_text(face="bold", hjust=0.5, size=14),
plot.subtitle=element_text(hjust=0.5, size=14))
)

#casos previos a las elecciones
(casos<-datacasos%>%
ggplot()+
geom_smooth(aes(diasparaeleccion, casos100mil, color=eleccion), se=F)+
facet_wrap(~distrito, ncol=6)+
theme_minimal()+
ylim(0,25)+
labs(x="Días para la elección",
y="Casos cada 100 mil habitantes",
caption="Fuente: CNE con datos del Ministerio de Salud (2022)",
color="")+
ggtitle("Casos detectados de COVID previos y posteriores a la elección",
subtitle="(por distrito)")+
scale_color_manual(values=c("#e43307", "#3088a3"))+
theme(legend.position="bottom",
plot.caption=element_text(hjust=0),
plot.title=element_text(face="bold", hjust=0.5, size=14),
plot.subtitle=element_text(hjust=0.5, size=14),
axis.line.y=element_blank())
)

ggsave("casos.tiff", casos, dpi=800)
ggsave("vacunados.tiff", vacunas, dpi=800)


