############Habilitando Librerías
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(mltools)

###########Pregunta 1

datos <- read_table("C:\\Users\\carlo\\OneDrive\\Documentos\\Data Science aplicado a Ciberseguridad\\Practica_3\\epa-http.csv")

# Renonbrando los encabezados de las columnas
colnames(datos) <- c("Source", "Datetime", "Http_method", "Resource", "Http_version", "Return_code", "Bytes_sent")

# Conversiones
datos$Http_method <- as.factor(datos$Http_method)
datos$Http_version <- as.factor(datos$Http_version)
datos$Return_code <- as.factor(datos$Return_code)
datos$Bytes_sent <- as.numeric(datos$Bytes_sent)

datos$Bytes_sent <- ifelse(is.na(datos$Bytes_sent), 0, datos$Bytes_sent)

View(datos)



#########Pregunta 2

tipo_error <- as.data.frame(table(datos$Return_code))

#Renombrando las columnas del dataframe "tipo_error"
names(tipo_error) <- c("Tipo_error", "Cantidad_Usuarios")


tipo_error <-  tipo_error %>%
  mutate(Descripcion = case_when(
    Tipo_error == "200" ~ "OK (peticion satisfactoria)",
    Tipo_error == "304" ~ "No modificado",
    Tipo_error == "302" ~ "Redirección temporal",
    Tipo_error == "400" ~ "Solicitud incorrecta",
    Tipo_error == "403" ~ "Prohibido",
    Tipo_error == "404" ~ "No encontrado",
    Tipo_error == "500" ~ "Error interno del servidor",
    Tipo_error == "501" ~ "No implementado",
    TRUE ~ "Otro tipo de error"
  )
  )


print(tipo_error)



####### Pregunta 3

#Identificando la frecuencia de peticiones HTTP (GET, POST, PUT, DELETE)


freq_peticiones <- as.data.frame(table(datos$Http_method))
names(freq_peticiones) <- c("Tipo de Peticion", "Frecuencia")
print(freq_peticiones)


#Identificando frecuencia de peticiones HTTP filtrando recursos de tipo imagen
tipo_imagen <- datos %>% filter(!grepl("(?i)\\.(gif|jpg|jpeg|png|bmp)$", Resource))

freq_peticiones_2 <- table(tipo_imagen$Http_method)

peticiones <- data.frame(Tipo_Peticion = names(freq_peticiones_2), Frecuencia = as.vector(freq_peticiones_2))

print(peticiones)

### Pregunta 4
# Eligan el grafico que les convenga
table_freq<-table(datos$Return_code)
Return_code_table <- data.frame(Return_code = names(table_freq), 
                                FRECUENCY = as.vector(table_freq))


ggplot(Return_code_table, aes(x = Return_code, y = FRECUENCY)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = FRECUENCY), vjust = -0.5, size = 4) +
  labs(title = "Gráfico Respuesta de Peticiones HTTP", x = "Peticiones",y = "Frecuencia")

ggplot(Return_code_table, aes(x = "", y = FRECUENCY, fill = Return_code)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = FRECUENCY), position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  labs(title = "Gráfico Respuesta de Peticiones HTTP", fill = "Peticion") +
  theme_void()




########Pregunta 5

library(stringr)
FILTER_HTTP <- datos[, c("Http_method", "Return_code", "Http_version")]

FILTER_DATA <- one_hot(as.data.table(FILTER_HTTP), sparsifyNAs = TRUE)


datos$Resource_dimension <- str_length(datos$Resource)

# Agrupamiento de 2 y 5
Agrup2 <- kmeans(FILTER_DATA, centers = 2)

Agrup5 <- kmeans(FILTER_DATA, centers = 5)


#Pregunta 6

library(ggplot2)


# Agregar la columna de clusters al conjunto de datos original
datos$cluster2 <- factor(Agrup2$cluster)
datos$cluster5 <- factor(Agrup5$cluster)

# Crear el gráfico con ggplot2
ggplot(datos, aes(x = Resource_dimension, y = Bytes_sent, color = cluster2)) +
  geom_point() +
  ggtitle("Gráfico con 2 clusters") +
  labs(x = "Resource_dimension", y = "Bytes_sent") +
  scale_color_discrete(name = "Grupos")

ggplot(datos, aes(x = Resource_dimension, y = Bytes_sent, color = cluster5)) +
  geom_point() +
  ggtitle("Gráfico con 5 clusters") +
  labs(x = "Resource_dimension", y = "Bytes_sent") +
  scale_color_discrete(name = "Grupos")
