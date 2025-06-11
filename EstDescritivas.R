## Estatísticas Descritivas

## Packages
library(readxl)
library(forcats)


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


## Estatísticas Descritivas
summary(df)
summary(dfnum)


## Variância e desvio padrão
var(dfnum) 
var(IdadeCuidador)              ;   sd(IdadeCuidador)         
var(QuantasGestAnteriores)      ;   sd(QuantasGestAnteriores) 
var(DuraçãoGestaMeses)          ;   sd(DuraçãoGestaMeses)     
var(IdadeBebéMeses)             ;   sd(IdadeBebéMeses)  
var(PesoNasKg)                  ;   sd(PesoNasKg)  
var(AlturaNasCm)                ;   sd(AlturaNasCm)
var(PerimCefálicoNasCm)         ;   sd(PerimCefálicoNasCm)
var(TempoInternadoDias)         ;   sd(TempoInternadoDias)  
var(ÍndiceAPGAR)                ;   sd(ÍndiceAPGAR) 
var(NúmTrat)                    ;   sd(NúmTrat)               
var(DiasTrat)                   ;   sd(DiasTrat)       
var(DifDiagonaisIniciaisMm)     ;   sd(DifDiagonaisIniciaisMm)  
var(DifDiagonaisFinaisMm)       ;   sd(DifDiagonaisFinaisMm)
var(PontuaçãoCSQ8)              ;   sd(PontuaçãoCSQ8)

