<img src="https://www.artimix.fr/var/artimix/storage/images/artimix/partenaires/ccafs/31850-4-esl-ES/ccafs_medium.jpg" alt="centered image" id="logo" data-height-percentage="100" data-actual-width="140" data-actual-height="55" class="center"><img src="https://ciat.cgiar.org/wp-content/uploads/Alliance_logo.png" alt="right image" id="logo" data-height-percentage="100" data-actual-width="140" data-actual-height="55" class="rigth">


# Manual para el manejo de datos de sensores de movimiento y de temperatura timpánica en bovinos


## Motivación
La influencia de los datos y de la inteligencia artificial en nuestras decisiones, comportamiento, incluso en nuestros sentimientos es una realidad. Todos los días nace un nuevo desarrollo, una nueva aplicación, estamos frente, sin duda, a la cuarta revolución industrial. 

La agricultura y la ganadería no son ajenas a esta nueva realidad. Hace unos años eran muy poco común encontrar artículos científicos que involucrarán inteligencia artificial y agricultura o ganadería, pero ahora su uso ha permeado sus distintas áreas y la cantidad de artículos han crecido considerablemente.

Un tema que ha comenzado a posicionarse en el ámbito ganadero es el impacto del cambio climático en sus sistemas productivos, se ha demostrado su impacto negativo en estos sistemas (Oosting, 2014) [1].  Por lo que monitorear estas variaciones en clima y en el impacto en los bovinos es una necesidad actual. Desde hace unos años, se empezó a utilizar sensores para la medición de distintas variables, como la temperatura corporal, entre los más utilizados, está la temperatura por el recto y por el tímpano, esta última es la más amigable con el bienestar animal. 

A nivel comercial, hay una gran variedad de sensores, la mayoría de estos tienen con un alto costo y adaptados a países europeos o norteamericanos, y en menor medida para los países del trópico, como Colombia. Sin embargo, varios centros de investigación y universidades han empezado a desarrollar estos sensores teniendo en cuenta las condiciones ambientales propias del lugar. Luego del desarrollo de los sensores se empieza con el análisis de los datos generados. Este manual pretende ofrecer una herramienta para el tratamiento de estos datos, se implementa una rutina para convertir en datos en bruto en datos fáciles de leer, prácticos y exactos. 

## Contexto

La temperatura en las zonas tropicales se ha incrementado  anualmente en más de  1°C durante los últimos cincuenta años y espera un incremento de más de 2°C en los próximos cincuenta años (Trewin, 2014) [2].  El incremento de la temperatura ha impacto al sector ganadero en gran medida, esto afecta el estrés calórico diario del animal que afecta negativamente el desempeño de la productividad  y la reproductividad. (Sirohi, 2007)[3].  De acuerdo al IDEAM (Instituto de Hidrología, Meteorología y Estudios Ambientales). La región del Valle Cauca ha experimientado desde el 2015 un incremento en temperatura entre 2 y 4 °C y aumento en las lluvia del 40%. Es importante notar que en los bovinos las variables climáticas tales como, temperatura ambiente, radiación solar, humedad relativa, velocidad del viento y precipitación son importantes para la regulación térmica. (Perrilla, 2021)[5]. 

<img src="https://github.com/j-river1/sensoresbovinos/blob/main/Imagenes/Vaca2.png" alt="centered image" id="logo" data-height-percentage="100" data-actual-width="140" data-actual-height="55" class="center">

Bajo este panorama, se llevó a cabo el proyecto, cuantificación de los impactos del clima en sistemas bovinos lecheros en el trópico bajo a través de minera de datos y modelos, liderado por la Universidad Nacional de Colombia, junto al grupos del grupo a agricultura digital de la Alianza Bioversity y CIAT, bajo el programa de Climate, Change, Agriculture and Food Security CCAFS. 


<img src="https://github.com/j-river1/sensoresbovinos/blob/main/Imagenes/Vaca1.png" alt="centered image" id="logo" data-height-percentage="100" data-actual-width="140" data-actual-height="55" class="center">
                                     
 
El objetivo de este trabajo es cuantificar la relación existente entre las variables climáticas y la temperatura corporal timpánica registrada a través del uso de sensores inalámbricos en vacas de pastoreo. 

El proceso descrito en la figura de arriba, homesotasis, es la capacidad de mantener una condición interna estable compensando los cambios en su entorno mediante el intercambio regulado de materia y energía exterior [4].  En este caso se pretende estudiar este proceso metabólico en la región del Valle del Cauca. 

Y el grupo de agricultura digital junto al equipo de la Universidad Nacional de Colombia trabajó en el flujo de trabajo para el proceso de mineríaa de datos utilizando los datos de los sensores, ciclo de vida de los datos, utilizando la metodologia [CRISDM](https://www.sngular.com/es/data-science-crisp-dm-metodologia/). Este manual hace parte de este procesamiento.


## Metodología

Los sensores utilizados para la captura 

<img src="https://github.com/j-river1/sensoresbovinos/blob/main/Imagenes/sensores1.png" alt="centered image" id="logo" data-height-percentage="100" data-actual-width="140" data-actual-height="55" class="center">


## Referencias 
[1]. Oosting, S. J., Udo, H. M. J., & Viets, T. C. (2014). Development of livestock production in the tropics: Farm and farmers’ perspectives. Animal, 8(8), 1238-1248. https://doi.org/10.1017/S1751731114000548 

[2] Trewin, B. The climates of the tropics and how they are changing. (2014). Bureau of Meteorology. https://www.jcu.edu.au/state-of-the-tropics/publications/2014/2014-essay-pdfs/Essay-1-Trewin.pdf

[3] Sirohi, S, Sufferer and cause: Indian livestock and climate change, (2017)

[4] https://es.wikipedia.org/wiki/Homeostasis

[5] Perilla, (2021). Efecto del clima sobre la respuesta térmica en vacas de diferentes grupos raciales en trópico bajo.
