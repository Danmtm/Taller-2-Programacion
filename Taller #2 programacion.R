

library(gapminder)
library(ggplot2)
library(dplyr)
library(openxlsx)
d = dim(gapminder)
x=(0.1*d[1])
indice1=sample(1:d[1],x)
indice2=sample(1:d[1],x)
indice3=sample(1:d[1],x)
lis_1=list(indice1, indice2, indice3)
gapminder$lifeExp[indice1]=NA
gapminder$gdpPercap[indice3]=NA
gapminder$pop[indice2]=NA
write.xlsx(gapminder, "C:\\Users\\LENOVO\\Dropbox\\Mi PC (LAPTOP-TIS1BHOO)\\Documents\\prueba_1xlsx.xlsx")
dt<- read.xlsx("C:\\Users\\LENOVO\\Dropbox\\Mi PC (LAPTOP-TIS1BHOO)\\Documents\\prueba_1xlsx.xlsx")
dt
p1<-mean(df$lifeExp,na.rm=TRUE)
df$lifeExp[indice1]=p1
p2<-mean(df$pop,na.rm=TRUE)
df$pop[indice2]=p2
p3<-mean(df$gdpPercap,na.rm=TRUE)
df$gdpPercap[indice3]=p3
ggplot(df, aes(lifeExp, pop, col=continent))+geom_point()
ggplot(df, aes(log(gdpPercap), pop, col=continent))+geom_point()
boxplot(df$gdpPercap~df$continent)

df1<-df %>% select(continent, year, gdpPercap) %>% filter(year <= 2007) %>% 
  filter(year >= 1990)

boxplot(df1$gdpPercap~df1$continent)

########################################

est<-function(){
  print("Bienvenido a algunas relaciones estadisticas entre experimentos normales")
  cat('\n')
  cat('\n')
  print("Dado que se necesitan dos experimentos con valores de una distribución normal, ingrese:")
  j<-as.integer(readline("Por favor, ingrese la longitud de los datos para ambos experimentos: \n"))
  x<-as.integer(readline("Por favor, ingrese la media para el experimento_a: \n"))
  x1<-as.integer(readline("Por favor, ingrese la media para el experimento_b \n"))
  d<-as.integer(readline("Por favor, ingrese la desviación estandar para el experimento_a: \n"))
  d1<-as.integer(readline("Por favor, ingrese la desviación estandar para el experimento_b: \n"))
  vec1=rnorm(j,x,d)
  vec2=rnorm(j,x1,d1)
  write.csv(vec1, "Experimento_a.csv")
  write.csv(vec2, "Experimento_b.csv")
  vec1a<-read.csv("Experimento_a.csv")$x
  vec2a<-read.csv("Experimento_b.csv")$x
  layout.show(2)
  hist(vec1a, main = "Histograma del experimento A", xlab = "Experimento_a")
  hist(vec2a, main = "Histograma del experimento B", xlab = "Experimento_b")
  i=0
  while (i!=4){
    print("1. ¿Son las medias estadisticamente significativas?")
    print("2. Correlación de Pearson y Spearman para los experimentos.")
    print("3. Diagrama de dispersión con linea de tendencia.")
    print("4. Salir")
    i<-as.integer(readline("Ingrese una opción:"))
    if (i==1){
      h<-t.test(vec1a,vec2a)
      h1<-h$p.value
      cat(sprintf("El p-valor de los experimentos es: \n %s", h1))
      if(h1>0.05){
        cat('\n')
        print("Las medias no presentan diferencias estadisticamente significativas")
        cat('\n')
      }else{
        cat('\n')
        print("Las medias presentan diferencias estadisticamente significativas")
        cat('\n')
      }
    }else if(i==2){
      cat('\n')
      print("Correlación de Pearson es:")
      cat('\n')
      print(cor(x=vec1a,y=vec2a))
      cat('\n')
      cat('\n')
      print("Correlación de Spearman es:")
      cat('\n')
      print(cor(x=vec1a,y=vec2a, method = "spearman"))
      cat('\n')
    }else if(i==3){
      vecT<- data.frame(vec1a, vec2a)
      print(ggplot(vecT, aes(x=vec1a,y=vec2a))+geom_point()+
              geom_smooth(method="lm", colour="Red")+
              labs(x="Experimento_a", y="Experimento_b", 
                   title="Diagrama de dispersión con liena de tendencia" ))
    }else if (i==4){
      cat('\n')
      print("Muchas gracias, adios.")
      cat('\n')
    }
  }
}
gapminder %>% filter(gdpPercap<5000) %>% 
 ggplot(aes(x=gdpPercap, y=lifeExp,col=continent, size=pop))+ 
geom_point(alpha=0.3)+ geom_smooth()
gapminder %>% filter(gdpPercap<5000) %>% 
 ggplot(aes(x=gdpPercap, y=lifeExp,col=continent, size=pop))+ 
geom_point(alpha=0.3)+ geom_smooth(method=lm)
gapminder %>% filter(gdpPercap<5000) %>% 
 ggplot(aes(x=gdpPercap, y=lifeExp,col=continent, size=pop))+ 
geom_point(alpha=0.3)+ geom_smooth(method=lm)+facet_wrap(~continent)
cc=1
for (i in df[][4:6]){
  p1<-mean(i,na.rm=TRUE)
  indice=lis_1[cc]
  print(indice)
  cc=cc+1
  x=i[indice[[1]]]=p1
  print(x)
  print(i)
}
p1<-mean(df$pop,na.rm=TRUE)
df$pop[indice2]=p1
df$pop
ggplot(df, aes(lifeExp, log(gdpPercap)))+geom_point()

layout(matrix(c(1:2),nrow=1, byrow=FALSE))
###############################################33
# Generamos datos
dn <- rnorm(500)
par(mfrow = c(1, 2))

# Creamos un histograma
hist(dn, freq = FALSE, main = "Histograma y densidad",
     ylab = "Densidad")
# Calculamos la densidad
ds <- density(dn)
# Añadimos la línea de densidad
lines(ds, lwd = 2, col = "red")
# Curva de densidad sin histograma
plot(ds, lwd = 2, col = "red",
     main = "Densidad")
##########################################

#densidad de la distribucion poisson
du<- rpois(500,200)
du
hist(du)
ds<-density(du)
lines(ds,lwd = 2, col= "red")
plot(ds, lwd = 2, col = "red")
#################
#densidad de la distribucion exponencial
dm<-rexp(500, rate = 1)
dm
hist(dm)
dn<-density(dm)
lines(dm, lwd = 2, col = "red")
plot(dn, lwd = 2, col = "red")
#########################
#distribucion de la uniforme
jp<-runif(500)
jp
hist(jp)
jpr<-density(jp)
jpr
lines(jpr, lwd = 2, col = "red")
plot(jpr, lwd = 2, col= "red")
#####################
#densidad de la distribucion binomial
bin<-rbinom(500, 500, 0.5)
bin
hist(bin)
des<-density(bin)
des
lines(des, lwd = 2, col = "red")
plot(des, lwd = 2,  col = "red")
