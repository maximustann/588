#include <cuda.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#define SIZE 1024

float *readFiletoArray(char *filename, int *size)
{
	FILE *fstream = fopen(filename, "r");
	int i = 0;
	char buf[SIZE];
	char *record;
	char *numbers, *number1, *number2;
	float *mat;
	if(fstream == NULL)
	{
		perror("open failed:");
		return NULL;
	}
	mat = (float *)malloc(SIZE * sizeof(float));
	memset(mat, 0, SIZE * sizeof(float));
	while((record = fgets(buf, sizeof(buf), fstream))!=NULL)
	{
		numbers = strtok(record, "\n");
		number1 = strtok(numbers, ",");
		number2 = strtok(NULL, " ");
		mat[i] = atof(number1);
		++i;
		mat[i] = atof(number2);
		++i;
	}
	//for(int iter = 0; iter < i; iter++)
	//{
		//printf("%G\n", mat[iter]);
	//}
	*size = i;
	fclose(fstream);
	return mat;
}

__global__ void step1(float *d_A, size_t pitch, int rows, int cols, float mat2row, float mat2col){
	int r = blockIdx.y * blockDim.y + threadIdx.y;
	int c = blockIdx.x * blockDim.x + threadIdx.x;
	if((r < rows) && (c < cols)){
		float *Row = (float *)((char *)d_A + r*pitch);
		float elem = Row[c];
		if(c == 0){
			d_A[r * 2 + c] = (elem - mat2row) * (elem - mat2row);
		}else{
			d_A[r * 2 + c] = (elem - mat2col) * (elem - mat2col);
		}
	}
}

__global__ void step2(float *d_A, float *result, size_t pitch, int rows, int cols){
	int r = threadIdx.x;
	result[r] = d_A[r*2] + d_A[r*2 + 1];
	//printf("result[%d] = %G\n", r, result[r]);
}

float min(float *arr, size_t length) {
    // returns the minimum value of array
    size_t i;
    float minimum = arr[0];
    for (i = 1; i < length; ++i) {
        if (minimum > arr[i]) {
            minimum = arr[i];
        }
    }
    return minimum;
}

float igd(char *refFront, char *front)
{
	float *mat1;
	float *mat2;
	float *d_mat1,*d_mat2;
	float *Curesult, *result;
	int *mat1size, *mat2size;
	int columnCount = 2, rowCount;
	mat1size = (int *)malloc(sizeof(int));
	*mat1size = 0;
	mat2size = (int *)malloc(sizeof(int));
	*mat2size = 0;
	float igd_value;

	
	mat1 = readFiletoArray(refFront, mat1size);
	mat2 = readFiletoArray(front, mat2size);
	size_t pitch;
	cudaMallocPitch((void **)&d_mat2, &pitch, sizeof(int)*columnCount, *mat2size);
	cudaMalloc((void **)&Curesult, *mat2size * sizeof(float) / 2);
	result = (float *)malloc(*mat2size * sizeof(float) / 2);
	memset(result, 0, *mat2size * sizeof(float) / 2);
	dim3 block(2,*mat2size/2);
	dim3 grid(1, 1);

	for(int i = 0; i < *mat1size;i += 2)
	{
		//copy front to cuda mem
		cudaMemcpy2D(d_mat2, pitch, mat2, sizeof(float)*columnCount, sizeof(float)*columnCount, *mat2size, cudaMemcpyHostToDevice);
		step1<<<grid, block>>>(d_mat2, pitch, *mat2size, columnCount, mat1[i], mat1[i + 1]);
		step2<<<1, *mat2size/2>>>(d_mat2, Curesult, pitch, *mat2size, columnCount);
		cudaMemcpy(result, Curesult, *mat2size * sizeof(float) / 2, cudaMemcpyDeviceToHost);
		cudaError_t error = cudaGetLastError();
		if(error != cudaSuccess){
			printf("%s\n", cudaGetErrorString(error));
		}
		//printf("igd_value = %G\n", sqrt(min(result, *mat2size/2)));
		igd_value += sqrt(min(result, *mat2size/2));
	}
	//printf("igd_value = %G\n", igd_value);
	igd_value = igd_value / (*mat1size /2 );
	cudaDeviceSynchronize();

	cudaFree(d_mat1);
	cudaFree(d_mat2);
	cudaFree(Curesult);
	free(result);
	free(mat1);
	free(mat2);
	free(mat1size);
	free(mat2size);
	return igd_value;
}

char* itoa(int i, char b[]){
    char const digit[] = "0123456789";
    char* p = b;
    if(i<0){
        *p++ = '-';
        i *= -1;
    }
    int shifter = i;
    do{ //Move to where representation ends
        ++p;
        shifter = shifter/10;
    }while(shifter);
    *p = '\0';
    do{ //Move back, inserting digits as u go
        *--p = digit[i%10];
        i = i/10;
    }while(i);
    return b;
}
float mean(float *a, int num)
{
	float value= 0;
	for(int i = 0; i < num; i++)
	{
		value += a[i];
	}
	return(value/num);
}

int create_csv(char *algorithm, char *problem, float *array, int n)
{

	FILE *fp;

	int i;

	char filename[SIZE] = "/home/st-james1/tanboxi/588_project/code/";
	strcat(filename, algorithm);
	strcat(filename, "/result/");
	strcat(filename, problem);
	strcat(filename, "/best/igd.csv");

	fp = fopen(filename, "w+");
	if(fp == NULL)
	{
		perror("open failed: ");
		return -1;
	}

	for(i = 0;i < n; i++){
		fprintf(fp,"%f,%d\n", array[i], i+1);
	}

	printf("finished!\n");
	fclose(fp);
	return 0;
}


int main(int argc, char *argv[])
{

	float igd_value[SIZE];
	char filename[SIZE];
	char base[SIZE] = "/home/st-james1/tanboxi/588_project/code/";
	char filedir[SIZE];
	char refFrontfilename[SIZE];
	int generation = 50;
	int run = 40;
	float runigd[run];
	float genigd[generation];
	char num[4];
	//float igd_value;
	//igd_value = igd("2.csv", "front.csv");
	//printf("igd = %G\n", igd_value);
	memcpy(refFrontfilename, base, strlen(base));
	strcat(refFrontfilename, "dataset/trueFront/");
	strcat(refFrontfilename, argv[2]);
	strcat(refFrontfilename, ".csv");
	for(int j = 1; j <= generation; j++)
	{
		for(int i = 1; i <= run; i++)
		{
			memcpy(filedir, base, strlen(base));
			strcat(filedir, argv[1]); //algorithm name
			strcat(filedir, "/logData/");
			memcpy(filename, filedir, strlen(filedir));
			strcat(filename, argv[2]); //problem N
			strcat(filename, "/");
			strcat(filename, itoa(i, num));
			strcat(filename, "/");
			strcat(filename, itoa(j, num));
			strcat(filename, "/front.csv");
			//printf("%s\n", filename);
			runigd[i-1] = igd(refFrontfilename, filename);
			//printf("runigd = %G\n", runigd[i-1]);
			memset(filedir, 0, strlen(filedir));
			memset(filename, 0, strlen(filename));
		}
		genigd[j-1] = mean(runigd, run);
	}
	//for(int j = 0; j < generation; j++)
	//{
//		printf("igd = %G, j = %d\n", genigd[j], j);
//	}

	create_csv(argv[1], argv[2], genigd, generation);
	return 0;
}
