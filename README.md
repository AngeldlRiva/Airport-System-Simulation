# Simulación de un Sistema de Colas en un Aeropuerto

## Descripción del Proyecto
Este repositorio contiene una simulación de un sistema de colas en un aeropuerto, modelado en R. Se simulan diferentes etapas del proceso de embarque de pasajeros: llegada, check-in, seguridad, control de pasaporte y embarque. La simulación emplea distribuciones de probabilidad para representar tiempos de servicio y llegadas, incluyendo el uso de una normal truncada y una distribución Weibull.

## Requisitos
Para ejecutar el código, necesitas tener instalado R. Puedes instalar R desde:
[R Project](https://www.r-project.org/)

## Estructura del Código
- **Generación de muestras:** Uso del algoritmo Box-Muller para generar valores de distribuciones normales truncadas.
- **Definición de eventos:** Modelado de las diferentes fases del proceso aeroportuario.
- **Lógica de simulación:** Control de eventos en base a un reloj de simulación.
- **Visualización de resultados:** Histogramas y pruebas de bondad de ajuste para evaluar el modelo.

## Ejecución
Para ejecutar la simulación, simplemente abre `Version_final.R` en R y corre el script completo.
```r
source("Version_final.R")
```

## Resultados Esperados
Al finalizar la simulación, se imprimirá en pantalla el estado final de cada cola y cuántos pasajeros han llegado a cada fase del sistema.

Ejemplo de salida:
```
En el instante de parada había en la primera etapa X personas
En el instante de parada había en la segunda etapa Y personas
...
```

## Mejoras Futuras
- Implementar visualización dinámica del sistema de colas.
- Explorar variaciones en los tiempos de servicio para analizar sensibilidad.
- Comparar con modelos de simulación en Python.

## Autor
Este proyecto fue desarrollado como parte de un trabajo de simulación de colas.

## Licencia
Este proyecto es de uso libre bajo la licencia MIT.

