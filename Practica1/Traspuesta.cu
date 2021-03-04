#include <stdio.h>
#include <stdlib.h> 
#include "cuda.h"
#include "cuda_runtime.h"
#include "device_launch_parameters.h"


__global__ void transponer(int *in, int *out) {
	int x = blockIdx.x * blockDim.x + threadIdx.x;
	int y = blockIdx.y * blockDim.x + threadIdx.y;
	out[y+x*4] = in[x+y*4];
}

int main(void) {
	cudaError_t error = cudaSuccess;
	int h_matrix[4][4] = {{0,0,0,0},
						  {1,1,1,1},
						  {2,2,2,2},
						  {3,3,3,3}
							};



	int* d_matrixIn = nullptr;

	error = cudaMalloc(&d_matrixIn, sizeof(int) * 16);

	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}

	int* d_matrixOut = nullptr;

	error = cudaMalloc(&d_matrixOut, sizeof(int) * 16);

	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}

	error = cudaMemcpy(d_matrixIn, h_matrix, sizeof(int) * 16, cudaMemcpyHostToDevice);

	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}

	printf("Matriz original: \n");
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			printf(" %d", h_matrix[i][j]);
		}
		printf("\n");
	}

	int threadsPerBlock = 256;
	int blocksPerGrid = (16 + threadsPerBlock - 1) / threadsPerBlock;
	transponer <<<blocksPerGrid, threadsPerBlock >>> (d_matrixIn, d_matrixOut);

	error = cudaMemcpy(h_matrix, d_matrixOut, sizeof(int) * 16, cudaMemcpyDeviceToHost);

	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}

	printf("Matriz traspuesta: \n");
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			printf(" %d", h_matrix[i][j]);
		}
		printf("\n");
	}
}
