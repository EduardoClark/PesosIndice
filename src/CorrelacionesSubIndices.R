require(plyr)

SuI <- read.csv("data/ResultadosSubindices.csv", stringsAsFactors=FALSE)
SuI$Año <- c(rep(2012,78),rep(2011,78),rep(2010,78),rep(2009,78),rep(2008,78) )

DB$AnclaCompuesta <- (.5 * DB$Inversión) + (.25*((DB$Grado.de.escolaridad.efectiva + DB$Absorción.de.la.demanda.estudiantil) /2)) + 
  (.25* (DB$Licenciados.ocupados + DB$Investigadores + DB$Migrantes..nal.y.ext..con.educación.superior) / 3)

DB$InversionF <- DB$Inversión
DB$TalentoRetencion <- (DB$Grado.de.escolaridad.efectiva + DB$Absorción.de.la.demanda.estudiantil) / 2 
DB$TalentoAtraccion <- (DB$Licenciados.ocupados + DB$Migrantes..nal.y.ext..con.educación.superior + DB$Investigadores) * .33
Anclas <- DB[,c(1:2,102:105)]
SuI <- merge(SuI, Anclas, by=c("Ciudad", "Año"))
remove(Anclas, DB, PesosSubIndices)

Correlaciones <- data.frame(SI=colnames(SuI)[5:15])
Sumas <- c()
for(j in 16:19){
  Cor <- c()
  for(i in 5:15){
    Cor <- c(Cor,cor(SuI[[i]], SuI[[j]], method="pearson") )
}
Cor <- data.frame(SI=colnames(SuI)[5:15], Cor=Cor)
Name <- colnames(SuI)[[j]]
colnames(Cor) <- c("SI", Name)
Correlaciones <- merge(Correlaciones, Cor, by=1)
Sumas <- c(Sumas, sum(Cor[[2]]))
}
remove(i, j, Name, Cor, SuI)
write.csv(Correlaciones, "data-out/CorrelacionesSubIndices.csv", row.names=FALSE)
