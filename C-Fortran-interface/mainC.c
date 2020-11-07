#include<stdio.h>

void carea_(double *radius, double *area);

int main(){
	double R, A;
	R = 2;

	carea_(&R, &A);
	printf("Circle of area: %lf has area of: %lf\n", R, A);
	return 0;
}
