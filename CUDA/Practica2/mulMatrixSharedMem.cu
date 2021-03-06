#include <cuda_runtime.h>
#include <device_launch_parameters.h>
#include <stdio.h>
#include <stdlib.h>

// Sets the size of the tile to be used
#define TILE_WIDTH 2 

/*
 * Method which tests whether there occur erroes while doing the operations to
 * move data from CPU to GPU and viceversa
 */
cudaError_t multMatSharedMem(int* result, int* a, int* b, int size);

__global__ void multMat(int* result, int* a, int* b, int size)
{
	// Elements which store elements in shared memory
	__shared__ int sA[TILE_WIDTH][TILE_WIDTH];
	__shared__ int sB[TILE_WIDTH][TILE_WIDTH];
	// MAtrices dimensions
	int bx = blockIdx.x;  int by = blockIdx.y;
	int tx = threadIdx.x; int ty = threadIdx.y;
	// Identifier of the corresponding executino at the GPU. It will be obtained
	// from the block's ID and the corresponging thread's ID
	int col = bx * TILE_WIDTH + tx;
	int row = by * TILE_WIDTH + ty;

	float mult = 0.0;
	// Are the column and row computed within the rigth limits of the matrices?
	if (col < size && row < size)
	{
		for (int i = 0; i < size / TILE_WIDTH; ++i)
		{
			// Copy data from global mem. to shared mem.
			sA[ty][tx] = a[row * size + (i * TILE_WIDTH + tx)];
			sB[ty][tx] = b[(i * TILE_WIDTH + ty) * size + col];

			__syncthreads();

			// Multiplication
			for (int j = 0; j < TILE_WIDTH; ++j) {
				mult += sA[ty][j] * sB[j][tx];
			}

			__syncthreads();
		}

		result[row * size + col] = mult;
	}
}

/*
 * Method which simplifies CUDA's error treatment
 */
__host__ void checkErrorCuda(const char *msg){
	cudaError_t err;
	cudaDeviceSynchronize();
	err = cudaGetLastError();

	if (err != cudaSuccess){
		printf("ERROR %d OCURRED: %s (%s)\n", err, cudaGetErrorString(err), msg);
		printf("Press any key to finish execution...");
		fflush(stdin);
		char tecla = getchar();
		exit(-1);
	}
}

int main()
{
	const int size = 4;

	int* h_matrixA = (int*)calloc(size * size, sizeof(int));
	int* h_matrixB = (int*)calloc(size * size, sizeof(int));
	int* h_matrixResult = (int*)calloc(size * size, sizeof(int));

	// Data loaded into the arrays
	for (int i = 0; i < size; i++){
		for (int j = 0; j < size; j++){
			h_matrixA[i * size + j] = 1;
			h_matrixB[i * size + j] = 2;
		}
	}

	printf("Matrixes Initialized\n");

	multMatSharedMem(h_matrixResult, h_matrixA, h_matrixB, size);
	checkErrorCuda("The matrix multiplication failed at multMat!\n");

	// Operation is printed
	for (int i = 0; i < size; i++){
		for (int j = 0; j < 3; j++){
			printf("| ");

			for (int k = 0; k < size; k++){
				switch (j){
					case 0: // Matrix A
						printf("%d", h_matrixA[i * size + k]);
						break;

					case 1: // Matrix B
						printf("%d", h_matrixB[i * size + k]);
						break;

					case 2: // Matrix Result
						printf("%d", h_matrixResult[i * size + k]);
						break;
				}
				if (k != size - 1) printf(", ");
			}

			if (i != (int)(size / 2) && j != 2) {
				printf(" |   ");
			}
			else if (j == 2) {
				printf(" |");
			}
			else {
				switch (j){
					case 0:
						printf(" | * ");
						break;

					case 1:
						printf(" | = ");
						break;
					case 2:
						printf(" |");
				}
			}

		}

		printf("\n");
	}

	cudaDeviceReset();
	checkErrorCuda("cudaDeviceReset failed!\n");

	return 0;
}

/*
 * Method which tests whether there occur erroes while doing the operations to
 * move data from CPU to GPU and viceversa
 */
cudaError_t multMatSharedMem(int* result, int* a, int* b, int size){
	// Variables used to operate in GPU mem
	int* dev_matrixA = 0;
	int* dev_matrixB = 0;
	int* dev_result = 0;

	dim3 dimGrid(size / TILE_WIDTH, size / TILE_WIDTH);
	dim3 dimBlock(TILE_WIDTH, TILE_WIDTH);

	// Select GPU where the threads are to be executed
	cudaSetDevice(0);
	checkErrorCuda("cudaSetDevice failed!\n");

	// Allocate memory for GPU's variables
	cudaMalloc((void**)&dev_result, size * size * sizeof(int));
	checkErrorCuda("cudaMalloc failed at Matrix Result!\n");

	cudaMalloc((void**)&dev_matrixA, size * size * sizeof(int));
	checkErrorCuda("cudaMalloc failed at Matrix A!\n");

	cudaMalloc((void**)&dev_matrixB, size * size * sizeof(int));
	checkErrorCuda("cudaMalloc failed at Matrix B!\n");


	cudaMemcpy(dev_matrixA, a, size * size * sizeof(int), cudaMemcpyHostToDevice);
	checkErrorCuda("cudaMemcpy failed at Matrix A!\n");

	cudaMemcpy(dev_matrixB, b, size * size * sizeof(int), cudaMemcpyHostToDevice);
	checkErrorCuda("cudaMemcpy failed at Matrix B!");

	multMat <<<dimGrid, dimBlock >>> (dev_result, dev_matrixA, dev_matrixB, size);

	checkErrorCuda("multMat launch failed");

	cudaDeviceSynchronize();
	checkErrorCuda("cudaDeviceSynchronize returned error after launching multMat!");

	cudaMemcpy(result, dev_result, size * size * sizeof(int), cudaMemcpyDeviceToHost);
	checkErrorCuda("cudaMemcpy failed after copying the result from GPU to CPU!");

	return cudaGetLastError();
}