#include <stdio.h>
#include <stdlib.h> 
#include "cuda.h"
#include "cuda_runtime.h"
#include "device_launch_parameters.h"

<<<<<<< HEAD
__global__ void incrementar_vector(float *a, float b, int N) { //a es el array, b el numero a incrementarlo, N es el numero de elementos total del array.
	int idThread = blockIdx.x * blockDim.x + threadIdx.x; //comprobamos que no haya ningun 
=======
__global__ void sumarMatriz(int *a, int *b, int N) { //a es una matriz, b la otra, N es el numero de elementos total del array que representa la matriz.
	int idThread = blockIdx.x * blockDim.x + threadIdx.x; //comprobamos que no haya ningun problema
>>>>>>> a77a26feec424b962fc081a9b067f5c9724a91bb

	if (idThread < N) { //Esta comprobacion existe por si acaso, idx nunca deberia superar a N
		a[idThread] = a[idThread] + b[idThread];
	}
}


int main() {
	cudaError_t error = cudaSuccess;
	//reservamos memoria en el host para el vector
<<<<<<< HEAD
	size_t size = 8 * sizeof(float);
	float* host_vector = (float*) malloc(size);

	for (int i = 0; i < 7; i++) {
		host_vector[i] = i;
	}

	//reservamos la memoria para el dispositivo AKA: GPU
	float* device_vector = nullptr;
=======
	size_t size = 4 * 4 * sizeof(int);
	int* host_matrixA = (int*)malloc(size);
	int* host_matrixB = (int*)malloc(size);


	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++){
			host_matrixA[j + (i - 1) * 4] = i + j;
			host_matrixB[j + (i - 1) * 4] = i;
		}
	}

//reservamos la memoria para el dispositivo AKA: GPU
int* device_matrixA = nullptr;
int* device_matrixB = nullptr;
>>>>>>> a77a26feec424b962fc081a9b067f5c9724a91bb

error = cudaMalloc((void**)&device_matrixA, size);

if (error != cudaSuccess) {
	printf("addWithCuda failed!");
	return 1;
}

error = cudaMalloc((void**)&device_matrixB, size);

if (error != cudaSuccess) {
	printf("addWithCuda failed!");
	return 1;
}

//Copiar memoria de host a device
error = cudaMemcpy(device_matrixA, host_matrixA, size, cudaMemcpyHostToDevice);

if (error != cudaSuccess) {
	printf("addWithCuda failed!");
	return 1;
}

error = cudaMemcpy(device_matrixB, host_matrixB, size, cudaMemcpyHostToDevice);

if (error != cudaSuccess) {
	printf("addWithCuda failed!");
	return 1;
}

<<<<<<< HEAD
	//Copiamos la memoria del device al host
	printf("El vector original: [%d, %d, %d, %d, %d, %d, %d, %d]", host_vector[0], host_vector[1], host_vector[2], host_vector[3], host_vector[4], host_vector[5], host_vector[6], host_vector[7]);
=======
//Lanzar el kernel que haga la operacion
>>>>>>> a77a26feec424b962fc081a9b067f5c9724a91bb

int threadsPerBlock = 256;
int blocksPerGrid = (16 + threadsPerBlock - 1) / threadsPerBlock;
sumarMatriz<<<blocksPerGrid, threadsPerBlock>>>(device_matrixA, device_matrixB, 16);


error = cudaGetLastError();
if (error != cudaSuccess) {
	printf("addWithCuda failed!");
	return 1;
}

printf("MatrizA original: \n");
for (int i = 0; i < 4; i++){
	for (int j = 0; j < 4; j++) {
		printf(" %d", host_matrixA[(j + (i - 1) * 4)]);
	}
	printf("\n");
}

printf("MatrizB original: \n");
for (int i = 0; i < 4; i++) {
	for (int j = 0; j < 4; j++) {
		printf(" %d", host_matrixB[(j + (i - 1) * 4)]);
	}
	printf("\n");
}
//Copiamos la memoria del device al host
error = cudaMemcpy(host_matrixA, device_matrixA, size, cudaMemcpyDeviceToHost);

<<<<<<< HEAD
	printf("El vector tras sumarle 10: [%d, %d, %d, %d, %d, %d, %d, %d]", host_vector[0], host_vector[1], host_vector[2], host_vector[3], host_vector[4], host_vector[5], host_vector[6], host_vector[7]);
=======
if (error != cudaSuccess) {
	printf("addWithCuda failed!");
	return 1;
}
>>>>>>> a77a26feec424b962fc081a9b067f5c9724a91bb

printf("Matriz resultado: \n");
for (int i = 0; i < 4; i++) {
	for (int j = 0; j < 4; j++) {
		printf(" %d", host_matrixA[(j + (i - 1) * 4)]);
	}
	printf("\n");
}

error = cudaFree(device_matrixA);
if (error != cudaSuccess) {
	printf("addWithCuda failed!");
	return 1;
}
error = cudaFree(device_matrixB);
if (error != cudaSuccess) {
	printf("addWithCuda failed!");
	return 1;
}
free(host_matrixA);
free(host_matrixB);

<<<<<<< HEAD
	printf("todo ok");
	return(0);
=======
printf("Ejecucion del programa correcta");
return(0);
>>>>>>> a77a26feec424b962fc081a9b067f5c9724a91bb
}
