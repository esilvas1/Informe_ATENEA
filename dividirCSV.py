"""
Script para dividir archivos CSV grandes en archivos más pequeños
Autor: Edwin Silva Salas
"""

import pandas as pd
import os
from pathlib import Path


def dividir_csv(archivo_entrada, filas_por_archivo=10000, directorio_salida=None):
    """
    Divide un archivo CSV grande en varios archivos más pequeños.
    
    Args:
        archivo_entrada (str): Ruta del archivo CSV a dividir
        filas_por_archivo (int): Número de filas por cada archivo dividido (default: 10000)
        directorio_salida (str): Directorio donde guardar los archivos. Si es None, usa el mismo directorio del archivo de entrada
    
    Returns:
        list: Lista de rutas de los archivos generados
    """
    
    # Verificar que el archivo existe
    if not os.path.exists(archivo_entrada):
        raise FileNotFoundError(f"El archivo {archivo_entrada} no existe")
    
    # Obtener nombre base del archivo
    archivo_path = Path(archivo_entrada)
    nombre_base = archivo_path.stem
    extension = archivo_path.suffix
    
    # Definir directorio de salida
    if directorio_salida is None:
        directorio_salida = archivo_path.parent
    else:
        # Crear directorio si no existe
        os.makedirs(directorio_salida, exist_ok=True)
    
    print(f"Procesando archivo: {archivo_entrada}")
    print(f"Dividiendo en archivos de {filas_por_archivo:,} filas cada uno")
    
    # Lista para almacenar rutas de archivos generados
    archivos_generados = []
    
    # Leer y dividir el CSV en chunks
    chunk_numero = 1
    
    for chunk in pd.read_csv(archivo_entrada, chunksize=filas_por_archivo, encoding='latin1'):
        # Generar nombre del archivo de salida
        archivo_salida = os.path.join(
            directorio_salida,
            f"{nombre_base}_parte_{chunk_numero:03d}{extension}"
        )
        
        # Guardar el chunk
        chunk.to_csv(archivo_salida, index=False, encoding='latin1')
        archivos_generados.append(archivo_salida)
        
        print(f"  ✓ Generado: {os.path.basename(archivo_salida)} ({len(chunk):,} filas)")
        
        chunk_numero += 1
    
    print(f"\n✓ Proceso completado: {chunk_numero - 1} archivos generados")
    print(f"Ubicación: {directorio_salida}")
    
    return archivos_generados


def dividir_multiples_csv(directorio, patron="*.csv", filas_por_archivo=10000, directorio_salida=None):
    """
    Divide múltiples archivos CSV en un directorio.
    
    Args:
        directorio (str): Directorio con archivos CSV
        patron (str): Patrón para buscar archivos (default: "*.csv")
        filas_por_archivo (int): Número de filas por archivo
        directorio_salida (str): Directorio de salida para todos los archivos divididos
    """
    
    archivos_csv = list(Path(directorio).glob(patron))
    
    if not archivos_csv:
        print(f"No se encontraron archivos CSV en {directorio}")
        return
    
    print(f"Encontrados {len(archivos_csv)} archivos CSV para procesar\n")
    
    for archivo in archivos_csv:
        try:
            dividir_csv(str(archivo), filas_por_archivo, directorio_salida)
            print()
        except Exception as e:
            print(f"✗ Error procesando {archivo.name}: {str(e)}\n")


if __name__ == "__main__":
    # Opción 1: Dividir un solo archivo
    # dividir_csv("archivo_grande.csv", filas_por_archivo=5000)
    
    # Opción 2: Dividir con directorio de salida específico
    dividir_csv("DATA/ATENEA/2024/2024_01_AFA_FORMATO_TC1.csv", filas_por_archivo=900000, directorio_salida="./divididos")
    
    # Opción 3: Dividir múltiples archivos de un directorio
    # dividir_multiples_csv("./DATA/CENS/2024", patron="*.CSV", filas_por_archivo=10000, directorio_salida="./divididos")
    
    print("Script de división de CSV")
    print("=" * 50)
    print("\nEjemplos de uso:")
    print("1. dividir_csv('archivo.csv', filas_por_archivo=5000)")
    print("2. dividir_csv('archivo.csv', filas_por_archivo=5000, directorio_salida='./output')")
    print("3. dividir_multiples_csv('./carpeta', filas_por_archivo=10000)")
