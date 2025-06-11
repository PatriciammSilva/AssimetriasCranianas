## Normalidade

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


## Teste
   # testar se os dados seguem a normalidade
   # H0 : seguem distribuição normal vs H1 : não seguem distribuição normal
   # p-valor :
   # < 0.05	→ Rejeita se H0     → H1
   # ≥ 0.05	→ Não se rejeita H0 → H0
shapiro.test(IdadeCuidador) 
shapiro.test(QuantasGestAnteriores)
shapiro.test(DuraçãoGestaMeses)
shapiro.test(IdadeBebéMeses)
shapiro.test(PesoNasKg) 
shapiro.test(AlturaNasCm) 
shapiro.test(PerimCefálicoNasCm)
shapiro.test(TempoInternadoDias)
shapiro.test(ÍndiceAPGAR)
shapiro.test(NúmTrat)
shapiro.test(DiasTrat)
shapiro.test(DifDiagonaisIniciaisMm)
shapiro.test(DifDiagonaisFinaisMm)
shapiro.test(PontuaçãoCSQ8)


## Gráficos - seguem normalidade
   # Idade Cuidador
hist(IdadeCuidador, main = "Idade do Cuidador", probability = TRUE, 
     col= "#BAE1FF", ylim = c(0, 0.13), xlab = "Idade em Anos", 
     ylab = "Probabilidade")
x1 = seq(min(IdadeCuidador), max(IdadeCuidador), length = 50) 
y1 = dnorm(x1, mean(IdadeCuidador), sd(IdadeCuidador)) 
lines(x1, y1, col = "black", lwd = 2)
   # Peso
hist(PesoNasKg, main = "Peso no Nascimento", probability = TRUE,
     col= "#BAE1FF", ylim = c(0, 0.9), xlab = "Peso em Quilos", 
     ylab = "Probabilidade")
x2 = seq(min(PesoNasKg), max(PesoNasKg), length = 50)  
y2 = dnorm(x2, mean(PesoNasKg), sd(PesoNasKg))  
lines(x2, y2, col = "black", lwd = 2) 
   # Altura
hist(AlturaNasCm, main = "Altura no Nascimento", 
     probability = TRUE, col= "#BAE1FF", ylim = c(0, 0.2),
     xlab = "Altura em Centímetros", ylab = "Probabilidade")
x3 = seq(min(AlturaNasCm), max(AlturaNasCm), length = 50)
y3 = dnorm(x3, mean(AlturaNasCm), sd(AlturaNasCm))  
lines(x3, y3, col = "black", lwd = 2)