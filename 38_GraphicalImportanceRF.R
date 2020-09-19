library(randomForest)
library(randomForestExplainer)

load("C:\\Users\\gorge\\Documents\\GIS DataBase\\amazon maximum height extras\\randomForestCor80v14042020.Rdata")
# load("C:\\Users\\gorge\\Documents\\GIS DataBase\\amazon maximum height extras\\randomForestIntactForest.Rdata")

importance(rf.heightAll$finalModel)

# Análise gráfica de objetos RandomForests: https://cran.r-project.org/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html
plot_min_depth_distribution(rf.heightAll$finalModel)

importance_rfAll = measure_importance(rf.heightAll$finalModel)
png('../amazon maximum height/gcb review/SF Importance.png', height = 7, 
    width = 15, units = 'cm', res = 300)
plot_multi_way_importance(importance_rfAll, 
                          x_measure = "mse_increase", 
                          y_measure = "node_purity_increase")
dev.off()
# 
# importance_rfIntact <- measure_importance(rf.intactForest$finalModel)
# plot_multi_way_importance(importance_rfIntact, 
#                           x_measure = "mse_increase", 
#                           y_measure = "node_purity_increase")
