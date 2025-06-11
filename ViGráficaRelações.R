## Visualização Gráfica - Individual

## Packages 
library(readxl)
library(forcats)
library(GGally)


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


   # Correlação
ggpairs(df[, c("PesoNasKg", "AlturaNasCm")])
ggpairs(df[, c("DifDiagonaisIniciaisMm", "DifDiagonaisFinaisMm")])
ggpairs(df[, c("DifDiagonaisIniciaisMm", "DifDiagonaisFinaisMm", 
               "IdadeBebéMeses")])
ggpairs(df[, c("DifDiagonaisIniciaisMm", "DifDiagonaisFinaisMm", "NúmTrat")])
ggpairs(df[, c("DifDiagonaisIniciaisMm", "DifDiagonaisFinaisMm", 
               "PontuaçãoCSQ8")])
ggpairs(df[, c("DifDiagonaisIniciaisMm", "DifDiagonaisFinaisMm", "DiasTrat")])
ggpairs(df[, c("ClasAssimetriaInicial", "ClasAssimetriaFinal")])
ggpairs(df[, c("ClasAssimetriaInicial", "ClasAssimetriaFinal", 
               "QualificaçãoTrat")])


   # Relação 1
boxplot(df[,c(31,33)], main = "Diferenças Diagonais", horizontal = TRUE, 
        col= c("#FFD1DC","#D5FFBA"))
par(mfrow = c(1, 2))
hist(DifDiagonaisIniciaisMm, main = "Diferenças Diagonais no Início", 
     horizontal = TRUE, col= "#FFD1DC")
hist(DifDiagonaisFinaisMm, main = "Diferenças Diagonais no Final", 
     horizontal = TRUE, col= "#D5FFBA")
boxplot(df[,c(31,29,33)], main = "Diferenças Diagonais", horizontal = TRUE, 
        col= c("#FFD1DC","#D5FFBA"))


   # Relação  2
tipo = factor(c(rep("Inicial", 42), rep("Final", 42)), 
              levels = c("Inicial", "Final"))
resultado = c(ClasAssimetriaInicial, ClasAssimetriaFinal)
tabela = table(tipo, resultado)
barplot(tabela, beside = TRUE, col = c("#FFD1DC", "#D5FFBA"),
        legend.text = TRUE, args.legend = list(title = "Tipo", x = "topright"),
        xlab = "Classificação da Assimetria", ylab = "Número de bebés",
        main = "Distribuição das Classificações por Grau")
