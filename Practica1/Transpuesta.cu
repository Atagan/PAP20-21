#include <stdio.h>
#include <cuda_runtime.h>


int main(void) {
	cudaError_t errorAux = cudaSuccess;
	cudaDeviceProp prop;
	int count;
	errorAux = cudaGetDeviceCount(&count);

	if (errorAux != cudaSuccess) {
		printf("Error al leer el numero de tarjetas tarjeta");
		return 1;
	}

	for (int i = 0; i < count; i++) {
		errorAux= cudaGetDeviceProperties(&prop, i);

		if (errorAux != cudaSuccess) {
			printf("Error al leer la tarjeta: %d", i);
			return 1;
		}

		printf("General Information:\n");
		printf("\tName: %s\n", prop.name);
		printf("\tNumber of multiprocessors in the device: %d\n", prop.multiProcessorCount);

		printf("\nMemory information: \n");
		printf("\tTotal Global Memory: %zu\n", prop.totalGlobalMem);
		printf("\tTotal constant memory: %zu", prop.totalConstMem);
		printf("\tMaximun amount of shared memory in a block: %zu\n", prop.sharedMemPerBlock);
		printf("\tNumber of registers avaiable per block: %d\n", prop.regsPerBlock);

		printf("\nThread information: \n");
		printf("\tMAX GRID SIZE (AKA: maximun amount of blocks in each dimension of a grid)\n");
		printf("\t [x -> %d]\n [y -> %d]\n [z -> %d]\n", prop.maxGridSize[0], prop.maxGridSize[1], prop.maxGridSize[2]);
		printf("\tMAX BLOCK SIZE\n");
		printf("\t [x -> %d]\n [y -> %d]\n [z -> %d]\n", prop.maxThreadsDim[0], prop.maxThreadsDim[1], prop.maxThreadsDim[2]);
		printf("\tMAX Threads per block: %d\n", prop.maxThreadsPerBlock);

		printf("\nTexture Information:\n");
		printf("\tMaximun sice of one dimensional textures: %d\n", prop.maxTexture1D);
		printf("\tMaximun resolution of two dimensional textures: [%d x %d]\n", prop.maxTexture2D[0], prop.maxTexture2D[1]);
		printf("\tMaximun resolution of three dimensional textures: [%d x %d x %d]\n", prop.maxTexture3D[0], prop.maxTexture3D[1], prop.maxTexture3D[2]);
	}
}
