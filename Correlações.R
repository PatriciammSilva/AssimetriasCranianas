## Correlações 

## Packages 
library(readxl)
library(forcats)
library(corrplot)
library(corrgram)


## Ler Dataset 
df = read_excel("Desktop/Assimetrias Cranianas/Dataset.xlsx")
str(df)


## Alterações Variáveis
varfac = c("Clínica", "SexoCuidador", "Escolaridade", "GestAnteriores", 
           "GestGemelar", "ComplicaçõesGest", "TipoParto", "ComplicaçõesParto",
           "SexoBebé", "InternNeonatologia", "LuxaçãoCongénita", "Babywearing",
           "TummyTime", "OutraTerapia", "Encaminhamento", "OutrasPatologias", 
           "LateralPlagiocefalia", "TratPretendido", "TerminoTrat", 
           "ClasAssimetriaInicial", "ClasAssimetriaFinal", 
           "ContribuiçãoDesenvolMotor", "ContribuiçãoMelhoriaSono", 
           "RespNecessidades", "Ajuda", "LidarProblemas", "Voltaria", 
           "Recomendação", "SatisfaçãoGeralTrat", "QualificaçãoTrat")
df[varfac] = lapply(df[varfac], as.factor)  
str(df)


## Acrescentar Níveis
ClasAssimetriaInicial = fct_expand(ClasAssimetriaInicial,"Sem Assimetria")
ClasAssimetriaInicial = factor(ClasAssimetriaInicial, 
              levels = c("Grave", "Moderada", "Leve", "Sem Assimetria"))
ClasAssimetriaFinal = fct_expand(ClasAssimetriaFinal, "Grave")
ClasAssimetriaFinal = factor(ClasAssimetriaFinal, 
              levels = c("Grave", "Moderada", "Leve", "Sem Assimetria"))
SatisfaçãoGeralTrat = fct_expand(SatisfaçãoGeralTrat, "Muito Insatisfeito", 
              after=0)


## Guardar Variáveis
attach(df)


## Variáveis Numéricas
dfnum = df[, c("IdadeCuidador", "QuantasGestAnteriores", "DuraçãoGestaMeses",
               "IdadeBebéMeses", "PesoNasKg", "AlturaNasCm", 
               "PerimCefálicoNasCm", "TempoInternadoDias", "ÍndiceAPGAR", 
               "NúmTrat", "DiasTrat", "DifDiagonaisIniciaisMm", 
               "DifDiagonaisFinaisMm", "PontuaçãoCSQ8")]


## Correlações
cor(dfnum[,c(1:13)])
   # Gráfico 1 
pairs(dfnum[,c(1:13)], main = "Matriz de Correlações")
   # Gráfico 2
corrplot(cor(dfnum[,c(1:13)]), method = "shade", type = "lower", 
         tl.col = "black")
    # Gráfico 3
corrgram(dfnum[,c(1:13)], lower.panel = panel.cor, upper.panel = panel.density,
         main= "Matriz de Correlações")





