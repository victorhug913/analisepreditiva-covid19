setwd("C:/github/predicao-covid")
df <- read.csv(file="MICRODADOS.csv", header=TRUE, sep=";", dec=",", stringsAsFactors=T)
require(stringr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(magrittr)
require(themis)
require(ROCR)

#Apenas confirmados
df <- db[df$Classificacao == "Confirmados", ]

# Tratando os dados
df$DataNotificacao <- NULL
df$DataCadastro <- NULL
df$DataDiagnostico <- NULL
df$DataColeta_RT_PCR <- NULL
df$DataColetaTesteRapido <- NULL
df$DataColetaSorologia <- NULL
df$DataColetaSorologiaIGG <- NULL
df$DataEncerramento <- NULL
df$DataColetaSorologiaIGG <- NULL
df$DataObito <- NULL
df$Bairro <- NULL
df$StatusNotificacao <- NULL
df$Municipio <- NULL
df$CriterioConfirmacao <- NULL
df$Classificacao <- NULL
df$TipoTesteRapido <- NULL
df$FaixaEtaria <- NULL

# Separar idade
df <- separate(df, "IdadeNaDataNotificacao", into="IdadeAnos", sep=" ", remove=FALSE)
df$IdadeAnos <- as.numeric(df$IdadeAnos)
df <- arrange(df, desc(IdadeAnos))
df$IdadeNaDataNotificacao <- NULL

df$Evolucao <- gsub("Óbito pelo COVID-19", "Óbito", df$Evolucao)
df$Evolucao <- gsub("Óbito por outras causas", "Óbito", df$Evolucao)
df <- filter(df, df$Evolucao != "-")
df <- filter(df, df$Evolucao != "Ignorado")
df$Evolucao <- as.factor(df$Evolucao)
summary(df)

# Treino e teste
linhas <- sample(1:length(df$Evolucao),length(df$Evolucao)*0.70)
treino <- df[linhas,]
teste <- df[-linhas,]
summary(treino)
treino_down <- recipe(Evolucao~., data=df) %>% 
  themis::step_downsample(Evolucao) %>% 
  prep() %>% 
  juice()
summary(treino_down)

summary(teste)

# Algorítmo
require(randomForest)
rf <- randomForest(x = treino_down[,-29],
             y = treino_down$Evolucao ,
             xtest = teste[,-1],
             ytest = teste$Evolucao,
             ntree = 1000,
             ntry = 10,
             Replace = T,
             nodesize = -10,
             maxnode = 15,
             keep.forest = T,
)

OOB.votes <- predict (rf,treino_down[,-29],type="prob");
OOB.pred <- OOB.votes[,2];

pred.obj <- prediction (OOB.pred,treino_down[29]);

RP.perf <- performance(pred.obj, "rec","prec");
plot (RP.perf);

ROC.perf <- performance(pred.obj, "tpr","fpr");
plot (ROC.perf);

medirauc <- performance(pred.obj, measure = "auc");
auc_ROCR <- medirauc@y.values[[1]]
auc_ROCR

plot  (RP.perf@alpha.values[[1]],RP.perf@x.values[[1]]);
lines (RP.perf@alpha.values[[1]],RP.perf@y.values[[1]]);
lines (ROC.perf@alpha.values[[1]],ROC.perf@x.values[[1]]);
