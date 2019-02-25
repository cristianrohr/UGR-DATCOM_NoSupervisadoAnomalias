# M?ster -> Detecci?n de anomal?as
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# UNIVARIATE STATISTICAL OUTLIERS -> IQR 
###########################################################################


# Siga las instrucciones indicadas en el fichero InstruccionesGuionPracticas.txt

# Cuando necesite lanzar una ventana gr?fica, ejecute X11()

# Vamos a trabajar con los siguientes objetos:

# mydata.numeric: frame de datos
# indice.columna: ?ndice de la columna de datos de mydata.numeric con la que se quiera trabajar
# nombre.mydata:  Nombre del frame para que aparezca en los plots

# En este script usaremos:

mydata.numeric  = mtcars[,-c(8:11)]  # mtcars[1:7]
indice.columna  = 1
nombre.mydata   = "mtcars"

# ------------------------------------------------------------------------

# Ahora creamos los siguientes objetos:

# mydata.numeric.scaled -> Debe contener los valores normalizados demydata.numeric.
#Para ello, usad la funci?n scale
# columna -> Contendr? la columna de datos correspondiente a indice.columna. 
#Basta realizar una selecci?n con corchetes de mydata.numeric
# nombre.columna -> Debe contener el nombre de la columna. Para ello, 
# aplicamos la funci?n names sobre mydata.numeric
# columna.scaled -> Debe contener los valores normalizados de la anterior



mydata.numeric.scaled = scale(mydata.numeric)
columna         = mydata.numeric[, indice.columna]
nombre.columna  = names(mydata.numeric)[indice.columna]
columna.scaled  = mydata.numeric.scaled[, indice.columna]





###########################################################################
###########################################################################
# Parte primera. C?mputo de los outliers IQR
###########################################################################
###########################################################################



###########################################################################
# Calcular los outliers seg?n la regla IQR. Directamente sin funciones propias
###########################################################################

# Transparencia 80


# ------------------------------------------------------------------------------------

# Calculamos las siguientes variables:

# cuartil.primero -> primer cuartil, 
# cuartil.tercero -> tercer cuartil
# iqr             -> distancia IQR

# Para ello, usamos las siguientes funciones:
# quantile(columna, x) para obtener los cuartiles
#    x=0.25 para el primer cuartil, 0.5 para la mediana y 0.75 para el tercero
# IQR para obtener la distancia intercuartil 
#    (o bien reste directamente el cuartil tercero y el primero)

# Calculamos las siguientes variables -los extremos que delimitan los outliers-

# extremo.superior.outlier.normal  = cuartil tercero + 1.5 IQR
# extremo.inferior.outlier.normal  = cuartil primero - 1.5 IQR
# extremo.superior.outlier.extremo = cuartil tercero + 3 IQR
# extremo.inferior.outlier.extremo = cuartil primero - 3 IQR

# Construimos sendos vectores: 

# vector.es.outlier.normal 
# vector.es.outlier.extremo

# Son vectores de valores l?gicos TRUE/FALSE que nos dicen 
# si cada registro es o no un outlier con respecto a la columna fijada
# Para ello, basta comparar con el operador > o el operador < la columna con alguno de los valores extremos anteriores

# El resultado debe ser el siguiente:
# [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [18] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE



# COMPLETAR
quantiles <- quantile(columna.scaled, c(0.25, 0.5, 0.75))
cuartil.primero <- quantiles[1]
cuartil.tercero <- quantiles[3]
iqr <- cuartil.tercero - cuartil.primero

extremo.superior.outlier.normal  = cuartil.tercero + 1.5*iqr
extremo.inferior.outlier.normal  = cuartil.primero - 1.5*iqr
extremo.superior.outlier.extremo = cuartil.tercero + 3*iqr
extremo.inferior.outlier.extremo = cuartil.primero - 3*iqr

vector.es.outlier.normal <- ifelse(columna.scaled < extremo.inferior.outlier.normal, TRUE,
                                   ifelse(columna.scaled > extremo.superior.outlier.normal, TRUE, FALSE))
# El toyota Corolla es un outlier
vector.es.outlier.extremo <- ifelse(columna.scaled < extremo.inferior.outlier.extremo, TRUE,
                                    ifelse(columna.scaled > extremo.superior.outlier.extremo, TRUE, FALSE))
# No hay outliers extremos

###########################################################################
# ?ndices y valores de los outliers
###########################################################################

# Construimos las siguientes variables:

# claves.outliers.normales     -> Vector con las claves (identificador num?rico de fila) de los valores que 
# son outliers. Para obtenerlo, usad which sobre vector.es.outlier.normal

# data.frame.outliers.normales -> data frame obtenido con la selecci?n del data frame original de las filas e
# que son outliers. Puede usarse o bien vector.es.outlier.normal o bien claves.outliers.normales
#                                 Este dataframe contiene los datos de todas las columnas de aquellas filas que son outliers.                                  
# nombres.outliers.normales    -> vector con los nombres de fila de los outliers. Para obtenerlo, 
# usad row.names sobre el data frame anterior
# valores.outliers.normales    -> vector con los datos de los outliers. Se muestra s?lo 
# el valor de la columna que se fij? al inicio del script 
# Idem con los extremos

# Aplicando la selecci?n dada por vector.es.outlier.normal:

#    [1] 20
#                    mpg cyl disp hp drat    wt qsec
     #Toyota Corolla 33.9   4 71.1 65 4.22 1.835 19.9
#    [1] "Toyota Corolla"
#    [1] 33.9

# Aplicando la selecci?n dada por vector.es.outlier.extremo:
# Ninguno



# COMPLETAR
claves.outliers.normales <- which(vector.es.outlier.normal)
data.frame.outliers.normales <- mydata.numeric[vector.es.outlier.normal, ]
row.names(data.frame.outliers.normales)
valores.outliers.normales <- mydata.numeric[vector.es.outlier.normal, nombre.columna]

claves.outliers.extremos <- which(vector.es.outlier.extremo)
data.frame.outliers.extremos <- mydata.numeric[vector.es.outlier.extremo, ]
row.names(data.frame.outliers.extremos)
valores.outliers.extremos <- mydata.numeric[vector.es.outlier.extremo, nombre.columna]


###########################################################################
# Desviaci?n de los outliers con respecto a la media de la columna
###########################################################################

# Construimos la variable:

# valores.normalizados.outliers.normales -> Contiene los valores normalizados de los outliers. 
# Usad columna.scaled y (o bien vector.es.outlier.normal o bien claves.outliers.normales)

# Toyota Corolla 
# 2.291272 



# COMPLETAR
valores.normalizados.outliers.normales <- columna.scaled[vector.es.outlier.normal]



###########################################################################
# Plot
###########################################################################

# Mostramos en un plot los valores de los registros (los outliers se muestran en color rojo)
# Para ello, llamamos a la siguiente funci?n:
# MiPlot_Univariate_Outliers (columna de datos, indices -claves num?ricas- de outliers , nombre de columna)
# Lo hacemos con los outliers normales y con los extremos



# COMPLETAR
MiPlot_Univariate_Outliers(columna, claves.outliers.normales, nombre.columna)
MiPlot_Univariate_Outliers(columna.scaled, claves.outliers.normales, nombre.columna)

# Cambia la escala, pero no la distancia entre los datos



###########################################################################
# BoxPlot
###########################################################################



# Vemos el diagrama de caja 

# Para ello, podr?amos usar la funci?n boxplot, pero ?sta no muestra el outlier en la columna mpg :-(
# Por lo tanto, vamos a usar otra funci?n. Esta es la funci?n geom_boxplot definida en el paquete ggplot
# En vez de usarla directamente, llamamos a la siguiente funci?n:
# MiBoxPlot_IQR_Univariate_Outliers = function (datos, indice.de.columna, coef = 1.5)
# Esta funci?n est? definida en el fichero de funciones A3 que ha de cargar previamente.
# Esta funci?n llama internamente a geom_boxplot

# Una vez que la hemos llamado con mydata.numeric y con indice.columna, la volvemos
# a llamar pero con los datos normalizados.
# Lo hacemos para resaltar que el Boxplot es el mismo ya que la normalizaci?n
# no afecta a la posici?n relativa de los datos 



# COMPLETAR
MiBoxPlot_IQR_Univariate_Outliers(datos = mydata.numeric, indice.de.columna = indice.columna, coef = 1.5)
MiBoxPlot_IQR_Univariate_Outliers(mydata.numeric.scaled, indice.columna, coef = 1.5)


  ###########################################################################
# C?mputo de los outliers IQR con funciones propias
###########################################################################


# En este apartado hacemos lo mismo que antes, pero llamando a funciones que est?n dentro de !Outliers_A3_Funciones.R :

# vector_es_outlier_IQR = function (datos, indice.de.columna, coef = 1.5)  
# datos es un data frame y coef es el factor multiplicativo en el criterio de outlier,
# es decir, 1.5 por defecto para los outliers normales y un valor mayor para outliers extremos (3 usualmente)
# -> devuelve un vector TRUE/FALSE indicando si cada dato es o no un outlier


# vector_claves_outliers_IQR = function(datos, indice, coef = 1.5)
# Funci?n similar a la anterior salvo que devuelve los ?ndices de los outliers

# COMPLETAR
vector_es_outlier_IQR(mydata.numeric, indice.columna, coef = 1.5)  
vector_claves_outliers_IQR(mydata.numeric, indice.columna, coef = 1.5)


vector_es_outlier_IQR(mydata.numeric.scaled, indice.columna, coef = 1.5)  
vector_claves_outliers_IQR(mydata.numeric.scaled, indice.columna, coef = 1.5)



###########################################################################
# BoxPlot
###########################################################################


# Mostramos los boxplots en un mismo gr?fico.
# Tenemos que usar los datos normalizados, para que as? sean comparables


# Llamamos a la funci?n  MiBoxPlot_Juntos
# MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir)  
# Pasamos mydata.numeric como par?metro a datos.
# Si no pasamos nada como segundo par?metro, se incluir?n todos los datos en el c?mputo.
# Esta funci?n normaliza los datos y muestra, de forma conjunta, los diagramas de cajas
# As?, podemos apreciar qu? rango de valores toma cada outlier en las distintas columnas.

# Para etiquetar los outliers en el gr?fico
# llamamos a la funci?n MiBoxPlot_juntos_con_etiquetas 


# COMPLETAR  
MiBoxPlot_juntos(mydata.numeric)  
MiBoxPlot_juntos_con_etiquetas(mydata.numeric)




###########################################################################
###########################################################################
# AMPLIACI?N
###########################################################################
###########################################################################



###########################################################################
# TODO LO QUE HAY A PARTIR DE AHORA ES DE AMPLIACI?N
# RESU?LVALO S?LO CUANDO TERMINE EL RESTO DE FICHEROS
# POR LO TANTO, PASE AHORA A RESOLVER EL SIGUIENTE FICHERO

# ESTA PARTE DE AMPLIACI?N ES OBLIGATORIO RESOLVERLA EN EL CASO DE QUE HAGA 
# EL TRABAJO DEL CURSO SOBRE LA PARTE DE ANOMAL?AS
###########################################################################



###########################################################################
# Trabajamos con varias columnas simult?neamente
###########################################################################


# Los outliers siguen siendo univariate, es decir, con respecto a una ?nica columna
# Pero vamos a aplicar el proceso anterior de forma autom?tica a todas las columnas

# Vamos a obtener los ?ndices de aquellos registros que tienen un outlier en alguna de las columnas
# As? pues, vamos a construir la siguiente variable:

# indices.de.outliers.en.alguna.columna -> 
# Contiene los ?ndices de aquellos registros que tengan un valor an?malo en cualquiera de las columnas

# Para ello, hay que aplicar la funci?n vector_claves_outliers_IQR sobre cada una de las columnas
# Para hacer esto autom?ticamente con todas las columnas, usamos sapply:
#   El primer argumento de sapply ser? el rango de las columnas que vamos a barrer, es decir, 1:ncol(mydata.numeric)
#   El segundo argumento de sapply ser? la funci?n a aplicar sobre el primer argumento, es decir, vector_claves_outliers_IQR 
#   Consulte la ayuda para obtener m?s informaci?n sobre sapply
# Como esta funci?n devuelve una lista, al final, tenemos una lista de listas.
# Para "desempaquetar" el resultado, usamos la funci?n unlist
# El resultado lo guardamos en indices.de.outliers.en.alguna.columna
# [1] 20 31 15 16 17  9


# El anterior resultado nos quiere decir que las filas con claves 20, 31, 15, 16, 17 y 9 
# tienen un outlier en alguna de sus columnas



# COMPLETAR  
indices.de.outliers.en.alguna.columna <- unlist(sapply(1:ncol(mydata.numeric), 
                                                       vector_claves_outliers_IQR, 
                                                       datos = mydata.numeric, 
                                                       coef = 1.5 )
                                                )


# Ahora queremos mostrar los valores normalizados de los registros que tienen un valor an?malo en cualquier columna 
# Pero mostramos los valores de todas las columnas 
# (no s?lo la columna con respecto a la cual cada registro era un valor an?malo)

#                       mpg       cyl       disp         hp       drat          wt        qsec
# Toyota Corolla       2.2912716 -1.224858 -1.2879099 -1.1914248  1.1660039 -1.41268280  1.14790999
# Maserati Bora       -0.8446439  1.014882  0.5670394  2.7465668 -0.1057878  0.36051645 -1.81804880
# Cadillac Fleetwood  -1.6078826  1.014882  1.9467538  0.8504968 -1.2466598  2.07750476  0.07344945
# Lincoln Continental -1.6078826  1.014882  1.8499318  0.9963483 -1.1157401  2.25533570 -0.01608893
# Chrysler Imperial   -0.8944204  1.014882  1.6885616  1.2151256 -0.6855752  2.17459637 -0.23993487
# Merc 230             0.4495434 -1.224858 -0.7255351 -0.7538702  0.6049193 -0.06873063  2.82675459

# Vemos, por ejemplo, que el Toyota se dispara (por arriba) en mpg pero no tanto en el resto de columnas
# El Maserati se dispara en hp (por arriba) y algo menos en qsec (por abajo)




# COMPLETAR  
mydata.numeric.scaled[indices.de.outliers.en.alguna.columna, ]




#----------------------------------------------------------------------
# Hacemos los mismo pero construyendo una funci?n

# Construya una funci?n que devuelva lo calculado anteriormente, es decir, 
# un vector de ?ndices de aquellos registros que tienen un outlier en alguna de las columnas
# La funci?n ha de tener la siguiente cabecera:

# vector_claves_outliers_IQR_en_alguna_columna = function(datos, coef = 1.5)

# coef es 1.5 para los outliers normales y hay que pasarle 3 para los outliers extremos

# Vuelva a calcular el valor de la variable indices.de.outliers.en.alguna.columna llamando a esta funci?n
# para comprobar que obtiene el mismo resultado
# [1] 20 31 15 16 17  9


# COMPLETAR  
vector_claves_outliers_IQR_en_alguna_columna <- function(datos, coef = 1.5) {
  indices <- unlist(sapply(1:ncol(datos), 
    vector_claves_outliers_IQR, 
    datos = datos, 
    coef = 1.5 )
  )
  indices
}

indices.de.outliers.en.alguna.columna <- vector_claves_outliers_IQR_en_alguna_columna(mydata.numeric)



# Una vez construida la funci?n vector_claves_outliers_IQR_en_alguna_columna,
# construimos la siguiente funci?n que devuelve un vector de bool
# indicando si un dato es outlier o no con respecto a cualquier columna


vector_es_outlier_IQR_en_alguna_columna = function(datos, coef = 1.5){
  indices.de.outliers.en.alguna.columna =  vector_claves_outliers_IQR_en_alguna_columna(datos, coef)
  todos = c(1:nrow(datos))
  bools = todos %in% indices.de.outliers.en.alguna.columna
  return (bools)
}

outliers.en.alguna.columna <- vector_es_outlier_IQR_en_alguna_columna(mydata.numeric)
