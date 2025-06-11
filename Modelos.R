## Modelos

## Packages 
library(readxl)
library(forcats)
library(nnet)

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


# Modelo 1 : Idade Bebé ~ Peso Nascimento + Altura Nascimento
mod1 = lm(IdadeBebéMeses ~ PesoNasKg + AlturaNasCm, data = df)
summary(mod1)

# Modelo 1a : Idade Bebé ~ Peso Nascimento
mod1a = lm(IdadeBebéMeses ~ PesoNasKg, data = df)
summary(mod1a)

# Modelo 2 : Complicações Parto ~ Idade Cuidados + Duração Gestação
mod2 = glm(ComplicaçõesParto ~ IdadeCuidador + DuraçãoGestaMeses, 
           data = df, family = "binomial")
summary(mod2)

# Modelo 2a : Complicações_Parto ~ Duração_Gestação_Meses
mod2a = glm(ComplicaçõesParto ~ DuraçãoGestaMeses, data = df,
            family = "binomial")
summary(mod2a)

# Modelo 3 : Diferença Diagonais Finais ~ Diferença Diagonais Iniciais + 
#           Dias Tratamento + Número Tratamentos
mod3 = lm(DifDiagonaisFinaisMm ~ DifDiagonaisIniciaisMm + DiasTrat + NúmTrat, 
          data = df)
summary(mod3)

# Modelo 3a : Diferença Diagonais Finais ~ Diferença Diagonais Iniciais +
#            Dias Tratamento
mod3a = lm(DifDiagonaisFinaisMm ~ DifDiagonaisIniciaisMm + DiasTrat, data = df)
summary(mod3a)

# Modelo 4 : Dias Internamento ~ Diferença Diagonais Finais + 
#           Dias Tratamento
mod4 = lm(TempoInternadoDias ~ DifDiagonaisFinaisMm + DiasTrat, data = df)
summary(mod4)


# Modelo 4a : Dias Internamento ~ Dias Tratamento
mod4a = lm(TempoInternadoDias ~ DiasTrat, data = df)
summary(mod4a)

# Modelo 5 : Diferença Diagonais Finais ~ Tummy Time + Babywearing
mod5 = lm(DifDiagonaisFinaisMm ~ TummyTime + Babywearing, data = df)
summary(mod5)

# Modelo 5a : Diferença Diagonais Finais ~ Tummy Time 
mod5a = lm(DifDiagonaisFinaisMm ~ TummyTime, data = df)
summary(mod5a)

# Modelo 6 : Diferença Diagonais Finais ~ Idade Bebé + Sexo Bebé + 
#         Outras Terapias
mod6 = lm(DifDiagonaisFinaisMm ~ IdadeBebéMeses + SexoBebé + OutraTerapia, 
          data = df)
summary(mod6)

# Modelo 6a : Diferença Diagonais Finais ~ Idade Bebé + Outras Terapias 
mod6a = lm(DifDiagonaisFinaisMm ~ IdadeBebéMeses + OutraTerapia, data = df)
summary(mod6a)

# Modelo 6b : Diferença Diagonais Finais ~ Idade Bebé 
mod6b = lm(DifDiagonaisFinaisMm ~ IdadeBebéMeses, data = df)
summary(mod6b)

# Modelo 7 : Diferença Diagonais Iniciais ~ Gestação Gemelar + 
#           Perimetro Cefálico
mod7 = lm(DifDiagonaisIniciaisMm ~ GestGemelar + PerimCefálicoNasCm, data = df)
summary(mod7)

# Modelo 8 : Dias Tratamento ~ Outras Patologias + Outro Tratamento + 
#          Classificação Inicial
mod8 = lm(DiasTrat ~ OutrasPatologias + OutraTerapia + ClasAssimetriaInicial, 
          data=df)
summary(mod8)

# Modelo 8a : Dias Tratamento ~ Outras Patologias +  Classificação Inicial
mod8a = lm(DiasTrat ~ OutrasPatologias + ClasAssimetriaInicial, data=df)
summary(mod8a)

# Modelo 8b : Dias Tratamento ~   Classificação Inicial
mod8b = lm(DiasTrat ~ ClasAssimetriaInicial, data=df)
summary(mod8b)

# Modelo 9: Número Tratamentos ~ Outras Patologias + Outro Tratamento + 
#          Classificação Inicial
mod9 = lm(NúmTrat ~ OutrasPatologias + OutraTerapia + ClasAssimetriaInicial, 
          data = df)
summary(mod9)

# Modelo 9a: Número Tratamentos ~ Outro Tratamento + Classificação Inicial
mod9a = lm(NúmTrat ~ OutraTerapia + ClasAssimetriaInicial, data = df)
summary(mod9a)

# Modelo 9b : Número Tratamentos ~ Classificação Inicial
mod9b = lm(NúmTrat ~ ClasAssimetriaInicial, data = df)
summary(mod9b)

# Modelo 10 : Classificação Final ~ Difernça Final
mod10 = multinom(ClasAssimetriaFinal ~ DifDiagonaisFinaisMm + DiasTrat + 
                   NúmTrat, data = df)
summary(mod10)

# Modelo 11 : Satisfação ~ Classificação Final + Diferença Final
mod11 = multinom(SatisfaçãoGeralTrat ~ ClasAssimetriaFinal + 
                   DifDiagonaisFinaisMm)
summary(mod11)

# Modelo 12 : Recomendação ~ Satisfação + Classificação Final + 
#          Diferença Final
mod12 = multinom(Recomendação ~ SatisfaçãoGeralTrat + ClasAssimetriaFinal + 
                   DifDiagonaisFinaisMm)
summary(mod12)

# Modelo 13 : Classificação Final ~ TummyTime + Babywearing 
mod13 = multinom(ClasAssimetriaFinal ~ TummyTime + Babywearing)
summary(mod13)

# Modelo 14
mod14 = multinom(ClasAssimetriaFinal ~ TummyTime + Recomendação + 
                   SatisfaçãoGeralTrat)
summary(mod14)

# Modelo 15
mod15 = multinom(SatisfaçãoGeralTrat ~ QualificaçãoTrat+ Voltaria + 
                   RespNecessidades)
summary(mod15)

