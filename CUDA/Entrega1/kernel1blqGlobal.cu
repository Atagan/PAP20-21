﻿#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <stdlib.h>  
#include <stdio.h>

using namespace std;
int const WIDTH_IMG = 32;
int const BLOCK_SIZE = 16;
int const WIDTH_FILTER = 3;

__global__ void convolucion(int* matrixImg, int* matrixFilter, int* matrixOut){
	int* subMatriz;
	int* matrixMul;
	subMatriz = (int*)malloc(WIDTH_FILTER * WIDTH_FILTER * sizeof(int));
	matrixMul = (int*)malloc(WIDTH_FILTER * WIDTH_FILTER * sizeof(int));
	int aux = 0;
	int sum = 0;

	//----------GENERACION DE SUBMATRIZ A LA QUE APLICAR EL FLITRO EN CADA HILO
	if (threadIdx.y != 0 && threadIdx.x != 0 && threadIdx.y != WIDTH_IMG - 1 && threadIdx.x != WIDTH_IMG - 1) {
		//centro
		for (int j = -1; j < 2; j++) {
			for (int k = -1; k < 2; k++) {
				subMatriz[(j + 1) * WIDTH_FILTER + (k + 1)] = matrixImg[(threadIdx.y * WIDTH_IMG + threadIdx.x) + (j * WIDTH_IMG + k)];
			}
		}	
	} else { 
		//Borde
		for (int j = -1; j < 2; j++) {
			for (int k = -1; k < 2; k++) {
				//recorremos todas las posiciones adyacentes a la casilla, lo cual nos rellena la submatriz de los valores adecuados,
				//salvo si son fuera de la matrizImg, en cuyo caso hay problema
				if (!((threadIdx.y * WIDTH_IMG + threadIdx.x + j * WIDTH_IMG + k) < 0 || (threadIdx.y * WIDTH_IMG + threadIdx.x + j * WIDTH_IMG + k) > WIDTH_IMG * WIDTH_IMG)) { // Si no se sale por arriba o por abajo dejamos el valor dde matrixImg
					subMatriz[(j + 1) * WIDTH_FILTER + (k + 1)] = matrixImg[(threadIdx.y * WIDTH_IMG + threadIdx.x) + (j * WIDTH_IMG + k)];                                 // Si se sale dejamos el 0 de calloc
				}
				else {
					//Aquí gestionamos los problemas que dan los bordes por arriba y abajo
					subMatriz[(j + 1) * WIDTH_FILTER + (k + 1)] = 0;
				}
			}
		}
		if ((threadIdx.y * WIDTH_IMG + threadIdx.x) % WIDTH_IMG == WIDTH_IMG - 1) { 
			//Aquí me encargo de las casillas por la derecha de la matrizImg
			for (int j = 1; j < 4; j++) {
				subMatriz[3 * j - 1] = 0;
			}
		}if ((threadIdx.y * WIDTH_IMG + threadIdx.x) % WIDTH_IMG == 0) { 
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
				aux = aux + matrixFilter[i * WIDTH_FILTER + k] * subMatriz[k * WIDTH_FILTER + j];
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

	//-----ASIGNACIÓN DE OUPUT
	matrixOut[threadIdx.y * WIDTH_IMG + threadIdx.x] = sum;
}

__global__ void trasponer(int* A, int* B){
	int x = threadIdx.x;
	int y = threadIdx.y;
	B[y * WIDTH_FILTER + x] = A[x * WIDTH_FILTER + y];
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
	dim3 dimBlockImg(WIDTH_IMG, WIDTH_IMG);
 	convolucion <<<1, dimBlockImg >>> (matrizImg_d, matrizFTras_d, matrizOut_d);
	cudaMemcpy(matrizOut_h, matrizOut_d, tamA, cudaMemcpyDeviceToHost);
	
	
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

