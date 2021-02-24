#include <stdio.h>
#include <stdlib.h> 
#include "cuda.h"
#include "cuda_runtime.h"
#include "device_launch_parameters.h"

__global__ void incrementar_vector(float *a, float b, int N) { //a es el array, b el numero a incrementarlo, N es el numero de elementos total del array.
	int idThread = blockId.x * blockDim.x + threadhIdx.x; //comprobamos que no haya ningun 

	if (idThread < N) { //Esta comprobacion existe por si acaso, idx nunca deberia superar a N
		a[idThread] = a[idThread] + b;
	}
}


int main() {
	cudaError_t error = cudaSuccess;

	//reservamos memoria en el host para el vector
	size_t size = 8 * sizeof(float);
	float* host_vector = (float*) malloc(size);

	for (int i = 0; i < 7; i++) {
		host_vector[i] = i;
	}

	//reservamos la memoria para el dispositivo AKA: GPU
	float* device_vector = nullptr;

	error = cudaMalloc((void**)&device_vector, size);

	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}

	//Copiar memoria de host a device
	error = cudaMemcpy(device_vector, host_vector, size, cudaMemcpyHostToDevice);

	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}

	//Lanzar el kernel que haga la operacion

	int threadsPerBlock = 256;
	int blocksPerGrid = (7 + threadsPerBlock - 1) / threadsPerBlock;
	incrementar_vector <<<blocksPerGrid, threadsPerBlock >>> (device_vector, 10, 8);


	error = cudaGetLastError();
	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}

	//Copiamos la memoria del device al host
	printf("El vector original: [%d, %d, %d, %d, %d, %d, %d, %d]", host_vector[0], host_vector[1], host_vector[2], host_vector[3], host_vector[4], host_vector[5], host_vector[6], host_vector[7]);

	error = cudaMemcpy(host_vector, device_vector, size, cudaMemcpyDeviceToHost);

	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}

	printf("El vector tras sumarle 10: [%d, %d, %d, %d, %d, %d, %d, %d]", host_vector[0], host_vector[1], host_vector[2], host_vector[3], host_vector[4], host_vector[5], host_vector[6], host_vector[7]);

	error = cudaFree(device_vector);
	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}
	free(host_vector);

	printf("todo ok");
	return(0);
}
