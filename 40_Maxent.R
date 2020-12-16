require(raster)
require(dismo)
require(rJava)  
require(jsonlite)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_241')

source('10_ImportingRasters.R')

## Envelop by MaxEnt ------------------------
maximas_70 = maximas[maximas$altura_cop>70,]                                     # filtra pontos com altura superior a 70 metros

ocorrenciaHeight70 = data.frame(x1=maximas_70$coords.x1,                         # data frame com latitude e longitude
                                x2=maximas_70$coords.x2)

fold = dismo::kfold(ocorrenciaHeight70, k=5)                                            # add an index that makes five random groups of observations
dadosTeste = ocorrenciaHeight70[fold == 1, ]                                     # hold out one fifth as test data
dadosTreino = ocorrenciaHeight70[fold != 1, ]                                    # the other four fifths are training data

me.height70 = dismo::maxent(layers2maxent, dadosTreino)                               # note we just using the training data
# save(me.height70,
    # file = '../amazon maximum height extras/maxentHeight70Cor80v06092020.Rdata')
load('../amazon maximum height extras/maxentHeight70Cor80v14042020.Rdata')

var_contrib = function(m, df = TRUE, ...) {                                      # extract importance for each variable from maxent plot
  stopifnot(inherits(m,  "MaxEnt"))
  res <- m@results[grep("contribution", rownames(m@results)), ]
  names(res) <- gsub(".contribution", "", names(res))
  if (df)
    res <- data.frame(var = names(res), contrib = unname(res))
  res
}

var_contrib(me.height70)                                                         # obter valor de importância variaveis

png('./plot/meMarginalPlotsCor80v06092020.png', units = 'cm', width = 20, height = 30, res = 300)
response(me.height70)                                                            # marginal plots maxent
dev.off()

rm(dadosTeste, dadosTreino, fold, ocorrenciaHeight70, maximas_70)
