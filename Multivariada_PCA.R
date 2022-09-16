
## PCA - Principal Components analysis

#-   Método de obtenção de variáveis importantes;
#-   Reduz dimensionalidade dos dados, a partir da redução da quantidade de variáveis do banco de dados sem perder de informações;
#-   É uma análise não-supervisionada que serve como análise exploratória preditiva;
#-   PCA: transformação/combinação linear das dimensões/variáveis originais não correlacionadas, criando componentes principais (PC), agrupadas segundo a variância;
#-   Utilizada para capturar padrões fortes;
#-   PCA somente analisa dados continuos
#-   Eixos - PC - agrupamento das variáveis (caracteristicas gerais);
#-   *O primeiro componente principal* explica a variação máxima do conjunto de dados;
#-   Melhor ajuste baseada no maior valor - junção das informações importantes num componente por meio do calculo de variância (indica qual componente explica melhor a variação dos dados - PC1) 
#- *O segundo componente* tenta explicar a variação restante do conjunto de dados e não está relacionado com o primeiro componente;
#-   As variáveis devem ser numéricas e os dados devem ser padronizados(em escala); > normalizar os dados para redução de dimensionalidade 

#Z = (valor bruto- média)/desvio padrão
  
#-   Padronização automatica do R a partir da função `scale`; 
#-   Youtube : StatQuestPCA 
#-   Dados não lineares podem ser problematicos (?) 
#-   Não é um teste -  tem base na distância Euclidiana 

  
  ### Como se calcula?
  
 # - Cálculo de transposição de matrizes a partir da tranformação utilizando **autovalores** e **autovetores**.

#Tutorial em português 
#[Carvalho Ribeiro:] (https://www.youtube.com/watch?v=BmeBIWW4fsI)


names(iris)
library(tidyverse)
library(factoextra) # ou (FactoMiner) para análise PCA e plot dos gráficos
library(vegan)

## Primeiro selecionamos as colunas que iremos analisar

#Matriz de correlação

pca_t1 = prcomp(iris[,c(1:4)], center = T, scale. = T) #centralizar proximo ao eixo -  padronização dos dados
pca_t1
summary(pca_t1) # aqui já dá o desvio padrão e a proporção de variância
variance_pca = (pca_t1$sdev)^2 #variancia

#O quanto que cada valor contribuiu/é importante para a variação.

#Observe que no PC1 cada variavel contribui de maneira similar, menos a largura da sépala, mas no PC2 apenas o largura contribui para a variação. 

#Standard deviation = Quanto cada eixo explica a correlação

#Proporcion of variance = importancia de cada eixo explicando a variação

# Extrair os autovalores - proporção de variância dos valores de componentes principais
eig.val <- get_eigenvalue(pca_t1)
eig.val

#Cálculos de matriz somando a correlação dos dados em relação a cada dimensão

#Quais eixos são significativos (explicam a variação) e que devem ser utilizados:
  
#-   PC1 = eixo x
#-   PC2 = eixo y

#Cada dimensão tem sua matriz isolada

## Importância que cada variavel tem para a explicação dos dados

### Variaveis
res.var <- get_pca_var(pca_t1)
res.var$coord          # Coordenadas
res.var$contrib        # Contr para os eixos PCs - como cada variavel contribui em porcentagem (no nosso caso o comprimento da pétala)
res.var$cos2           # qualidade representacao 

#Para  cada eixo os maiores valores indicam a variável mais representativa

# Results for individuals
res.ind <- get_pca_ind(pca_t1)
res.ind$coord          # Coordenadas
res.ind$contrib        # Contr para os eixos PCs
res.ind$cos2           # qualidade representacao 

#podemos ver melhor as explicações por plot de correlação
library(corrplot) #pacote para comparar modelos
corrplot(res.var$cos2, is.corr=FALSE) #checa a qualidade de representação
##ou mesmo de barra, # inves do cosseno pode usar a contribuição, ou outro aspecto, depende do interesse
fviz_contrib(pca_t1, choice = c("var"), axes= 1) #podemos ver melhor as explicações por plot de correlação

#contribuições abaixo e acima da linha, mais (acima) ou menos significante

#Gráfico montrando a proporção da variância de cada variável - vamos ver a distribui��o das dimens�es em termos de explica��o da variancia
fviz_eig(pca_t1, addlabels = T)

#Quando os valores estão mais regulares quer dizer que os valores não "funcionaram" para o plot da pca (?)

#Extrair os resultados das variaveis do PCA - agora vamos iniciar primeiro plotando as medidas dos individuos 
fviz_pca_ind(pca_t1)
fviz_pca_ind(pca_t1)+
  labs(title = "PCA", x = "PC1 (73%)", y = "PC2 (23%)")+ #dar nomes aos bois
  xlim(-4,4) + ylim(-4,4)                   #mudar escala manual

#adicionando contribui��o dos individuos
fviz_pca_ind(pca_t1, geom = "point", col.ind = "cos2")+ #"cos2", contrib
  labs(title = "PCA", x = "PC1(73%)", y = "PC2(23%)")+          #dar nomes aos bois
  xlim(-4,4) + ylim(-4,4)  +                          #mudar escala grafico manual
  scale_color_gradient2(low="gold", mid="red",        #escalas importancia manual
                        high="blue", midpoint=0.6)

#Quando os valores estão mais regulares quer dizer que os valores não "funcionaram" para o plot da pca (?)

#Extrair os resultados das variaveis do PCA - agora vamos iniciar primeiro plotando as medidas dos individuos 
fviz_pca_ind(pca_t1)
fviz_pca_ind(pca_t1)+
  labs(title = "PCA", x = "PC1 (73%)", y = "PC2 (23%)")+ #dar nomes aos bois
  xlim(-4,4) + ylim(-4,4)                   #mudar escala manual

#adicionando contribui��o dos individuos
fviz_pca_ind(pca_t1, geom = "point", col.ind = "cos2")+ #"cos2", contrib
  labs(title = "PCA", x = "PC1(73%)", y = "PC2(23%)")+          #dar nomes aos bois
  xlim(-4,4) + ylim(-4,4)  +                          #mudar escala grafico manual
  scale_color_gradient2(low="gold", mid="red",        #escalas importancia manual
                        high="blue", midpoint=0.6)

#Os valores que mais contribuem são os extremos

#adicionando as elipses
fviz_pca_ind(pca_t1, geom = "point", habillage = iris$Species, 
             addEllipses=TRUE, ellipse.level=0.95)+ 
  labs(title = "PCA", x = "PC1(73%)", y = "PC2(23%)")+          #dar nomes aos bois
  xlim(-4,4) + ylim(-4,4) + theme_minimal()          #mudar escala grafico manual

#selecionando alguns de acordo com criterios ou individual
fviz_pca_ind(pca_t1, geom = "point", habillage = iris$Species, 
             addEllipses=TRUE, ellipse.level=0.95, 
             select.ind = list(cos2 = 0.95))+       #criterios de sele��o
  labs(title = "PCA", x = "PC1(73%)", y = "PC2(23%)")+          #dar nomes aos bois
  xlim(-4,4) + ylim(-4,4) + theme_minimal()          #mudar escala grafico manual

#Selecionar quais são os individuos que se deja plotar

fviz_pca_ind(pca_t1, geom = "point", habillage = iris$Species, #ind(linhas)
             addEllipses=TRUE, ellipse.level=0.95, 
             select.ind = list(name = c("1", "15", "74")))+ #selecionar individual
  labs(title = "PCA", x = "PC1(73%)", y = "PC2(23%)")+          #dar nomes aos bois
  xlim(-4,4) + ylim(-4,4) + theme_minimal()          #mudar escala grafico manual


#Extrair os resultados das variaveis do PCA - agora vamos de graficos para as vari�veis
fviz_pca_var(pca_t1, col.var = "cornflowerblue") #setas que mostram onde cada variavel está mais distribuida (circulo 3) - gráfico de PCA

fviz_pca_var(pca_t1, col.var="cos2")+
  scale_color_gradient2(low="gold", mid="red",
                        high="blue", midpoint=0.955) 

fviz_pca_var(pca_t1, select.var = list(contrib = 4))
fviz_pca_var(pca_t1, select.var = list(names = c("Sepal.Width"))) #plotar só a variável que você quer


#sepal width que está separado apresenta uma explicação melhor
#repel = não sobrepor informaões

#agora vamos fazer um grafico juntando ambos acima

pca_t = fviz_pca_biplot(pca_t1, 
                        # Individuals
                        geom.ind = "point",
                        pointshape = 21,
                        fill.ind = iris$Species,
                        col.ind = "black",
                        pointsize = 3,
                        addEllipses = T,
                        ellipse.level=0.95,
                        # Variables
                        col.var = "contrib",
                        legend.title = list(fill = "Espécies de Plantas", 
                                            color = "Contr(metrics)"), 
                        ggtheme = theme_classic(base_size=15),
                        label = "all", 
                        palette = "Set1", 
                        title = "", 
                        repel = T)+
  scale_color_gradient2(low = "gold",mid = "red",
                        high="blue", midpoint = 25)+
  labs(x = "PC1(73%)", y = "PC2(23%)")

pca_t

#Quais dimensões serão utilizadas para fazer uma análise de variância?
  
#usar as dimensoes da pca como vari�vel explicativa
# usar significancia do autovalor das dimensoes
#install.packages("BiodiversityR")
library(BiodiversityR)

#eixo 2 não é diferente do nulo - aconselhavel não usar

#devtools::install_github("arleyc/PCAtest", force= TRUE)
library(PCAtest)
result = PCAtest(iris[,1:4], nperm = 999, nboot = 999, alpha = 0.05,
                 varcorr=T, counter=F, indload = T, plot=T)

#vamos usar a dimensão 1 (PC1) em uma anova
pca_t1
m1 = aov(pca_t1$x[,1]~iris$Species)
summary(m1)

#outro pacote para plotar
library(ggfortify)
library(ggthemes)
library(EnvStats)

t = autoplot(pca_t1, data=iris, colour = "Species", label = F, scale = T,
             label.size = 2, loadings = T, loadings.label = T, #size = "IVG", 
             frame.type = "norm",# shape = "trat", 
             loadings.colour = 'black', 
             loadings.label.colour = "black", loadings.label.size = 4)
t=t+theme_classic()+labs(tags = "A")+
  scale_color_brewer(palette = "Dark2", aesthetics = "colour")
t

library(RColorBrewer)
mycolor = brewer.pal(3, "Dark2")

a = autoplot(pca_t1, data=iris, colour = "Species", label = F, scale = T,
             label.size = 2, loadings = T, loadings.label = T, #size = "plantula", 
             frame.type = "norm", loadings.colour = 'black',#shape = "trat", 
             loadings.label.colour = "black", loadings.label.size = 4)
a=a+theme_classic()+
  scale_color_brewer(palette = "Set1", aesthetics = "colour")
a

# O PCA é direcionado para conjunto de dados de mais de 3 dimensões;
# O conjunto de dados deve ser númerico ou transformado para tal.
# Número de eixo é igual ao numero de variaveis, porém são utilizados os dois primeiros eixos que representam melhor as explicações das variáveis
