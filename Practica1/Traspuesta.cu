#include <stdio.h>
#include <cuda_runtime.h>


__global__ void transponer(int *in, int *out){
	int x = blockIdx.x * blockDim.x + threadIdx.x;
	int y = blockIdx.y * blockDim.x + threadIdx.y;
	int width = gridDim.x * TILE_DIM;
	out[x][y]=in[y][x];
}

int main(void) {
	cudaError_t error = cudaSuccess;
	int h_matrix[4][4];

	for(i=0;i<4;i++){
		for(j=0;j<4;j++){
			h_matrix[i][j]=i;
		}
	}
	
	int* d_matrix = nullptr;
	
	error=cudaMalloc(&d_matrix, sizeof(int) * 16);
	
	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}
	
	error = cudaMemcpy(d_matrix, h_matrix, sizeof(int)*16, cudaMemcpyHostToDevice);
	
	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}
	
	printf("Matriz original: \n");
	for (int i = 0; i < 4; i++){
		for (int j = 0; j < 4; j++) {
			printf(" %d", h_matrix[(j + (i - 1) * 4)]);
		}
		printf("\n");
	}
	
	int threadsPerBlock = 256;
	int blocksPerGrid = (16 + threadsPerBlock - 1) / threadsPerBlock;
	transponer<<<blocksPerGrid, threadsPerBlock>>>(d_matrix, d_matrix);
	
	error = cudaMemcpy(h_matrix, d_matrix, sizeof(int)*16, cudaMemcpyDeviceToHost);
	
	if (error != cudaSuccess) {
		printf("addWithCuda failed!");
		return 1;
	}
	
	printf("Matriz traspuesta: \n");
	for (int i = 0; i < 4; i++){
		for (int j = 0; j < 4; j++) {
			printf(" %d", h_matrix[(j + (i - 1) * 4)]);
		}
		printf("\n");
	}
}
