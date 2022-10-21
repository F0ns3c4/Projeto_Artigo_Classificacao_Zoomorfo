##____________________________________________________________________________##
# Etapa 1 - Instalação (se necessário) e carregamento dos pacotes usados no script.
#(Installing and loading packages used in Database and Cluster Analysis):
if(!require(readODS)) install.packages("readODS")
if(!require(dplyr)) install.packages("dplyr")
if(!require(factoextra)) install.packages("factoextra")

library(readODS) #Carregar planilha no formato .ODS (OpenOffice)
library(factoextra) #Produzir gráficos (Dendograma e K-média)
library(dplyr) #Manipular Base de Dados


##____________________________________________________________________________##
#Etapa 2 - Importação e Preparação da Base de Dados (Loading DataBase):
#Base da dados (dados_reconhecimento) disponível no GitHub do projeto.
setwd("C:\\doc") #definir diretório de trabalho

Base_dados_zoomorfo=read_ods("database.ods")#importar base de dados.

#Segregar colunas para criar fatores e, posteriormente, vetores numéricos:
ID = c(Base_dados_zoomorfo$ID)
cabeça = c(Base_dados_zoomorfo$Cabeça)
Pescoço = c(Base_dados_zoomorfo$Pescoço)
Tronco = c(Base_dados_zoomorfo$Tronco)
Membros = c(Base_dados_zoomorfo$Membros)
Patas = c(Base_dados_zoomorfo$Patas)
Cauda = c(Base_dados_zoomorfo$Cauda)

#Criar fatores das colunas:
factor_cabeca = factor(cabeça)
factor_pescoco = factor(Pescoço)
factor_tronco = factor(Tronco)
factor_membros = factor(Membros)
factor_patas = factor(Patas)
factor_cauda = factor(Cauda)

#Transformar as colunas em vetores numéricos:
cabeca_numeric = as.numeric(factor_cabeca)
pescoco_numeric = as.numeric(factor_pescoco)
tronco_numeric = as.numeric(factor_tronco)
membro_numeric = as.numeric(factor_membros)
patas_numeric = as.numeric(factor_patas)
cauda_numeric = as.numeric(factor_cauda)

#Dataframe da Base da Dados (numérico):
Dataframe_zoomorfo = data.frame(ID,
                                cabeca_numeric,
                                pescoco_numeric,
                                tronco_numeric,
                                membro_numeric,
                                patas_numeric,
                                cauda_numeric,
                                row.names = 1)

#Segregar Base de Dados em Categorias (Bípedes e Quadrúdepes):

DFBipede<-filter(Dataframe_zoomorfo,membro_numeric=="1"|membro_numeric=="3")
#Formar Dataframe somente associado a Bípede
DFQuadrupede <- filter(Dataframe_zoomorfo,membro_numeric == "4")
#Formar Dataframe somente associado a Quadrúpede


##____________________________________________________________________________##
#Etapa 3 - Análise de Agrupamento (Método Ward.D2):

#Bípedes____________:
#Formação dos Agrupamentos Bípedes
Cluster_Bipede = (hclust(dist(DFBipede), method = "ward.D2")) 

#Plotagem do Resultado Hierárquico Bípede
fviz_dend(Cluster_Bipede, cex = 0.9, lwd = 1, k = 5, 
          rect = T, 
          rect_border = "gray",
          rect_fill = T,
          horiz = F,
          ylab = "Similaridade",
          xlab = "Unidade Pictórica",
          main = "Agrupamento Hierárquico (Dendograma Bípedes)")


#Quadrúpedes____________:
#Formação dos Agrupamentos Quadrúpedes
Cluster_quadruped = (hclust(dist(DFQuadrupede), method = "ward.D2"))

#Plotagem do Resultado Hierárquico Quadrúpedes
fviz_dend(Cluster_quadruped, cex = 1, lwd = 1, k = 8,
          rect = T, 
          rect_border = "gray",
          rect_fill = T,
          horiz = F,
          ylab = "Similaridade",
          xlab = "Unidade Pictórica",
          main = "Agrupamento Hierárquico (Dendograma Quadrúpedes)")

###___________________________________________________________________________##
#Etapa 4 - Análise K-médias dos Agrupamentos:

#Bídedes____________:
#Definição de Escala Bídepes
Dados_escala_bipede = scale(na.omit(DFBipede))

#Gerar dados Kmeans:
Resultado_kmeans_bipede = kmeans(Dados_escala_bipede, center=5)

#Plotagem Kmeans:
fviz_cluster(Resultado_kmeans_bipede, data = Dados_escala_bipede,
             ellipse.type = "euclid",
             repel = T,
             labelsize = 12,
             xlab = FALSE,
             ylab = FALSE,
             main = "Agrupamentos K-média dos Bípedes")

#Quadrúpedes____________:
#Definição de Escala Quadrúpedes
Dados_escala_quadrupede = scale(na.omit(DFQuadrupede))

#Gerar dados Kmeans:

Resultado_kmeans_quadrupede = kmeans(Dados_escala_quadrupede[,-4], center=8)

#Plotagem Kmeans:
fviz_cluster(Resultado_kmeans_quadrupede, data = Dados_escala_quadrupede[,-4],
             ellipse.type = "euclid",
             repel = T,
             labelsize = 11,
             xlab = FALSE,
             ylab = FALSE,
             main = "Agrupamentos K-média dos Quadrúpedes")

##____________________________________________________________________________##
