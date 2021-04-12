#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <stdlib.h>  
#include <stdio.h>
#include <time.h>

using namespace std;
int const WIDTH_IMG = 32;
int const BLOCK_SIZE = 16;
int const WIDTH_FILTER = 3;

__global__ void convolucionMemCompartida(int* matrixImg, int* matrixFilter, int* matrixOut)
{
	int* subMatriz;
	int* matrixMul;
	int fila = blockIdx.y * BLOCK_SIZE + threadIdx.y;
	int columna = blockIdx.x * BLOCK_SIZE + threadIdx.x;
	subMatriz = (int*)malloc(WIDTH_FILTER * WIDTH_FILTER * sizeof(int));
	matrixMul = (int*)malloc(WIDTH_FILTER * WIDTH_FILTER * sizeof(int));
	int aux = 0;
	int sum = 0;

	__shared__ int matrizImagen[BLOCK_SIZE][BLOCK_SIZE];

	//-----paso de la matrixImg de memoria global a compartida
	matrizImagen[threadIdx.y][threadIdx.x] = matrixImg[fila * WIDTH_IMG + columna];
		
	__syncthreads();
	printf("{%d,%d} \n", fila, columna);

	//----------GENERACION DE SUBMATRIZ A LA QUE APLICAR EL FLITRO EN CADA HILO
	if (fila != 0 && columna != 0 && fila != WIDTH_IMG - 1 && columna != WIDTH_IMG - 1) { 
		//Centro
		printf("IF (f,c): {%d,%d} \n ", fila, columna);
		for (int j = -1; j < 2; j++) {
			for (int k = -1; k < 2; k++) {
				subMatriz[(j + 1) * WIDTH_FILTER + (k + 1)] = matrizImagen[fila+j][columna+k];
			}
		}
		
	}
	else { 
		//Borde
		printf("ELSE (f,c): {%d,%d} \n ", fila, columna);
		for (int j = -1; j < 2; j++) {
			for (int k = -1; k < 2; k++) {
				//recorremos todas las posiciones adyacentes a la casilla, lo cual nos rellena la submatriz de los valores adecuados,
				//salvo si son fuera de la matrizImg, en cuyo caso hay problema
				if (!((fila * WIDTH_IMG + columna + j * WIDTH_IMG + k) < 0 || (fila * WIDTH_IMG + columna + j * WIDTH_IMG + k) > WIDTH_IMG * WIDTH_IMG)) { // Si no se sale por arriba o por abajo dejamos el valor dde matrixImg
					subMatriz[(j + 1) * WIDTH_FILTER + (k + 1)] = matrizImagen[fila + j][columna + k];                                             // Si se sale dejamos el 0 de calloc
				}
				else {
					subMatriz[(j + 1) * WIDTH_FILTER + (k + 1)] = 0;
				}
			}
		}
		if ((fila * WIDTH_IMG + columna) % WIDTH_IMG == WIDTH_IMG - 1) { 
				//Aquí me encargo de las casillas por la derecha de la matrizImg
				subMatriz[3 * j - 1] = 0;
			}
		}
		if ((fila * WIDTH_IMG + columna) % WIDTH_IMG == 0) { 
			//Aquí me encargo de las casillas por la izquierda de la matrizImg
			for (int j = 0; j < 3; j++) {
				subMatriz[3 * j] = 0;
			}
		}
	}

	//--------MULTRIPLICACION SUBMATRIZ*FILTRO
	for (int i = 0; i < WIDTH_FILTER; i++) {
		for (int j = 0; j < WIDTH_FILTER; j++) {
			for (int k = 0; k < WIDTH_FILTER; k++) {
				aux = aux + subMatriz[i * WIDTH_FILTER + k] * matrixFilter[k * WIDTH_FILTER + j];
			}
			matrixMul[i * WIDTH_FILTER + j] = aux;
			aux = 0;
		}
	}
	//------SUMA DE LOS VALORES DE LA MATRIZ RESULTADO DE LA MULTRIPLICACION
	for (int i = 0; i < WIDTH_FILTER; i++) {
		for (int j = 0; j < WIDTH_FILTER; j++) {
			sum += matrixMul[j * WIDTH_FILTER + i];


		}
	}

	if (fila == 0 && columna == 0) {
		printf("La matriz subMatriz: \n");
		for (int i = 0; i < WIDTH_FILTER; i++) {
			for (int j = 0; j < WIDTH_FILTER; j++) {
				printf(" %d ", subMatriz[i * WIDTH_FILTER + j]);
			}
			printf("\n");
		}
		printf("La matriz matrixMul: \n");
		for (int i = 0; i < WIDTH_FILTER; i++) {
			for (int j = 0; j < WIDTH_FILTER; j++) {
				printf(" %d ", matrixMul[i * WIDTH_FILTER + j]);
			}
			printf("\n");
		}
		printf("%d", sum);
	}
	//-----ASIGNACIÓN DE OUPUT
	matrixOut[fila * WIDTH_IMG + columna] = sum;
}

__global__ void trasponer(int* matrixImg, int* matrixFilter)
{
	int i = threadIdx.x;
	int j = threadIdx.y;
	matrixFilter[j * WIDTH_FILTER + i] = matrixImg[i * WIDTH_FILTER + j];
}


int main(){
	size_t tamA = WIDTH_IMG * WIDTH_IMG * sizeof(int);
	size_t tamB = WIDTH_FILTER * WIDTH_FILTER * sizeof(int);
	//Variables del host

	int* matrizImg_h;
	int* matrizFilter_h;
	int* matrizFTras_h;
	int* matrizOut_h;

	matrizImg_h = (int*)malloc(tamA);
	matrizFilter_h = (int*)malloc(tamB);
	matrizFTras_h = (int*)malloc(tamB);
	matrizOut_h = (int*)calloc(WIDTH_IMG * WIDTH_IMG, sizeof(int));

    
	//Variables del device

	int* matrizImg_d;
	int* matrizFilter_d;
	int* matrizFTras_d;
	int* matrizOut_d;

    srand(time(0));
	for (int i = 0; i < WIDTH_IMG; i++) {
		for (int j = 0; j < WIDTH_IMG; j++) {
			matrizImg_h[i * WIDTH_IMG + j] = rand() % 255 + rand() % 255 + rand() % 255; 
		}
	}
	for (int i = 0; i < WIDTH_FILTER; i++) {
		for (int j = 0; j < WIDTH_FILTER; j++) {
			matrizFilter_h[i * WIDTH_FILTER + j] = rand() % 4 - 2;
		}
	}

	//Reservamos espacio en el device para trasponer el filtro
	cudaMalloc((void**)&matrizFilter_d, tamB);
	cudaMalloc((void**)&matrizFTras_d, tamB);

	//Pasamos a la memoria del device las matrices del host
	cudaMemcpy(matrizFilter_d, matrizFilter_h, tamB, cudaMemcpyHostToDevice);
	cudaMemcpy(matrizFTras_d, matrizFTras_h, tamB, cudaMemcpyHostToDevice);
	
	//Llamamos al kernel con las dimensiones adecuadas
	dim3 dimBlockT(WIDTH_FILTER , WIDTH_FILTER);
	trasponer <<<1, dimBlockT >>> ( matrizFilter_d, matrizFTras_d);
	//Guardamos el resultado en matrixOut de la operacion en el host de la trasposicion
	cudaMemcpy(matrizFTras_h, matrizFTras_d, tamB, cudaMemcpyDeviceToHost);

	//----------------------------------------------------------------

	//Comienza la convolucion
	//Reservamos espacio en el device para trabajar con la convolucion
	cudaMalloc((void**)&matrizImg_d, tamA);
	cudaMalloc((void**)&matrizOut_d, tamA);

	//Pasamos a la memoria del device las matrices del host
	cudaMemcpy(matrizImg_d, matrizImg_h, tamA, cudaMemcpyHostToDevice);
	cudaMemcpy(matrizFTras_d, matrizFTras_h, tamB, cudaMemcpyHostToDevice);
	cudaMemcpy(matrizOut_d, matrizOut_h, tamA, cudaMemcpyHostToDevice);
	
	//Llamamos al kernel con las dimensiones adecuadas
	dim3 dimGrid(WIDTH_IMG/BLOCK_SIZE, WIDTH_IMG / BLOCK_SIZE);
	dim3 dimBlockImg(BLOCK_SIZE, BLOCK_SIZE);
	convolucion <<< dimGrid, dimBlockImg >>> (matrizA_d, matrizBT_d, matrizC_d);
	cudaMemcpy(matrizC_h, matrizC_d, tamA, cudaMemcpyDeviceToHost);
	
	
	//Mostramos el resultado
	printf("La matriz Imagen: \n");
	for (int i = 0; i < WIDTH_IMG; i++) {
		for (int j = 0; j < WIDTH_IMG; j++) {
			printf(" %d ", matrizImg_h[i * WIDTH_IMG + j]);
		}
		printf("\n");
	}
	printf("La matriz Filtro: \n");
	for (int i = 0; i < WIDTH_FILTER; i++) {
		for (int j = 0; j < WIDTH_FILTER; j++) {
			printf(" %d ", matrizFilter_h[i * WIDTH_FILTER + j]);
		}
		printf("\n");
	}
	printf("La matriz Filtro Traspuesta: \n");
	for (int i = 0; i < WIDTH_FILTER; i++) {
		for (int j = 0; j < WIDTH_FILTER; j++) {
			printf(" %d ", matrizFTras_h[i * WIDTH_FILTER + j]);
		}
		printf("\n");
	}
	printf("Resultado de la convolucion de AxB*: \n");
	for (int i = 0; i < WIDTH_IMG; i++) {
		for (int j = 0; j < WIDTH_IMG; j++) {
			printf(" %d ", matrizOut_h[i * WIDTH_IMG + j]);
		}
		printf("\n");
	}

	//Liberamos memoria
	free(matrizImg_h);
	free(matrizFilter_h);
	free(matrizFTras_h);
	free(matrizOut_h);
	cudaFree(matrizImg_d);
	cudaFree(matrizFilter_d);
	cudaFree(matrizFTras_d);
	cudaFree(matrizOut_d);
	return 0;
}