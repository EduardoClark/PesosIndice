require(plyr)
require(pls)
require(reshape2)

DB <- read.csv("data/BaseNormalizada2.csv", stringsAsFactors=FALSE)
DB$Indicador <- NULL
DB$X <- NULL
DB$Sentenciados.a.más.de.1.año.de.prisión <- NULL

DB$AnclaCompuesta <- ((.5)*(DB$Inversión)) +
                       ((.5)*(DB$Grado.de.escolaridad.efectiva + 
                               DB$Absorción.de.la.demanda.estudiantil + 
                               DB$Licenciados.ocupados + 
                               DB$Migrantes..nal.y.ext..con.educación.superior +
                               DB$Investigadores ) / 5)

#Principal components regression using ncomp=N(VAR)*c
PC <- pcr( AnclaCompuesta ~Ejecución.de.contratos+Robo.de.vehículos+Competencia.en.servicios.notariales+Tasa.de.homicidios+Monto.reportado.en.robo.de.mercancías+Tierra.ejidal+Percepción.de.inseguridad+Sobreexplotación.de.acuíferos+Consumo.de.agua+Capacidad.de.tratamiento.de.agua.en.operación+Índice.de.gestión.de.calidad.del.aire+Disposición.adecuada.de.residuos.sólidos+Aprovechamiento.del.biogás.en.rellenos.sanitarios+Empresas.certificadas.como..limpia.+Desastres.naturales+Economía.intensiva.en.energía+Emergencias.ambientales+Espacios.culturales+Empresa.socialmente.responsable+Visitas.a.museos.INAH+Universidades.de.calidad+Rendimiento.académico+Escuelas.de.calidad+Mortalidad.por.diabetes+Mortalidad.infantil+Médicos+Camas.de.hospital+Acceso.a.instituciones.de.salud+Crecimiento.en.acceso.a.servicios.de.salud+Crecimiento.en.población.altamente.calificada+Viviendas.con.drenaje..sólo.conexión.a.red.pública.+Viviendas.con.piso.de.tierra+Viviendas.deshabitadas+Personas.por.debajo.de.la.línea.de.bienestar+Desigualdad.en.las.ciudades+Mujeres.en.la.fuerza.laboral+Ingreso.promedio.de.la.mujer+Duración.de.período.para.ediles.y.delegados+Participación.ciudadana+Competencia.electoral+Organizaciones.de.la.sociedad.civil+Periodistas.muertos.o.desaparecidos+Índice.de.información.presupuestal+Ingresos.propios+Apertura.de.un.negocio+Registro.de.una.propiedad+Muertes.por.infecciones.intestinales+Empleados.en.el.sector.formal+Crecimiento.en.los.empleados.en.el.sector.formal+Crecimiento.de.la.mancha.urbana+Densidad.poblacional+Salario.promedio.mensual.para.trabajadores.de.tiempo.completo+Jornadas.laborales.muy.largas....48.horas.semanales.+Crecimiento.en.jornadas.laborales.muy.largas+Productividad.media.del.trabajo+Población.ocupada.sin.ingresos+Crecimiento.de.la.población.ocupada.sin.ingresos+Demandantes.de.conflicto.laboral+Crédito.a.las.empresas+Tamaño.del.mercado.hipotecario+Cartera.hipotecaria.vencida+Sectores.que.han.presentado.alto.crecimiento..2008.2012.+Crecimiento.del.PIB.estatal+Crecimiento.del.salario.promedio+Diversificación.económica+Desempleo+Viviendas.con.líneas.telefónicas.móviles+Viviendas.con.computadora+Uso.deTwitter.como.proxy.de.uso.de.tecnologías.de.información+Muertes.por.accidentes.relacionados.con.transporte+Sistema.de.transporte.masivo+Red.carretera.avanzada+Aerolíneas+Flujo.de.pasajeros.aéreos+Líneas.de.autobús+Uso.de.servicios.financieros+Acceso.a.servicios.financieros+Consumo.de.diesel.en.transporte.de.bienes+Inversión.extranjera.directa..neta.+Flujo.de.pasajeros.del.o.hacia.el.extranjero+Oferta.hotelera.4.y.5.estrellas+Ocupacion.hotelera+Sitios.UNESCO+Ciudad.fronteriza.o.portuaria+Empresas+Grandes.empresas.según.CNN.Expansión+Empresas.certificadas.con.ISO.9000.y.14000+Centros.de.investigación+Posgrados.de.calidad + Patentes, data=DB)

#Extract Full coeficient list
Coefs <- PC$coefficients
CoefLoadings <- c()
for(i in 1:72){
  CoefLoadings <- c(CoefLoadings,as.numeric(Coefs[,,i]))
}
remove(i, Coefs)
Variables <- colnames(DB)[5:94]
Coefs <- data.frame(CoefLoadings=CoefLoadings, Variables=Variables)
SubIndice <- t(read.csv("data/subindices2.csv", stringsAsFactors=FALSE, header=FALSE))
#SubIndice <- SubIndice[-1]
SubIndice <- SubIndice[1:90]
Coefs$SubIndice <- SubIndice
remove(SubIndice, Variables, CoefLoadings)
Coefs <- arrange(Coefs, Coefs$Variables)
Coefs$Componente <- 1:72
SubIndices <- unique(Coefs[,2:3])
Coefs <- dcast(Coefs, formula= Componente ~ Variables , value.var="CoefLoadings", )


#Weigth by variable
Weights <- data.frame(colSums(Coefs))
Variables <- row.names(Weights)
Weights$Variables <- Variables
remove(Variables)
colnames(Weights) <- c("Peso", "Variable")
Weights <- Weights[-1,]
Weights$AbPeso <- abs(Weights$Peso)
Weights$Norm <- (Weights$AbPeso - min(Weights$AbPeso)) / (max(Weights$AbPeso - min(Weights$AbPeso))) * 100
Weights <- merge(Weights, SubIndices, by.x="Variable", by.y=1 )

PesosSubIndices <- data.frame(tapply(Weights$AbPeso, Weights$SubIndice, sum))
PesosSubIndices$Peso <- PesosSubIndices[[1]] / (sum(PesosSubIndices[[1]]))
remove(Coefs, PC,Weights, SubIndices )
PesosSubIndices$SubIndice <- row.names(PesosSubIndices)
PesosSubIndices <- PesosSubIndices[,3:2]
PesosSubIndices <- arrange(PesosSubIndices, desc(PesosSubIndices$Peso))
write.csv(PesosSubIndices, "data-out/PesosSubIndices2.csv", row.names=FALSE)

