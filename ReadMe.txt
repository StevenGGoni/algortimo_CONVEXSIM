Read Me

Se corre primero el "generador_restricciones.R", esto produce varias 
restricciones de frontera superior que crean diseños de vértices extremos. 

Para generar las restricciones CONVEXSIM (generar_restricciones_CONVEXSIM.R)
se emplean las funciones customizadas
"funcion_CONVEXSIM.R" y "funcion_obtener_limites.R".

Con los diseños generados, se aplica la función CONVEXSIM, tanto I como S. Esto
se hace mediante "generador_restricciones_CONVEXSIM.R". Estas se almacenan
para ser empleadas luego.

Finalmente, los resultados son generados por el scrit "generador_resultados.R"
estos son almacenados en una lista compuesta de 10 data.frames, el cuál es
utilizado para realizar el análisis. 

Estos data.frames se unifican con el script 'unificar_resultados.R' en un 
solo data.frame, con el cual se realiza el análisis y visualización de datos. 

