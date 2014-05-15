#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/timeb.h>

typedef struct {
	char* stringValue;
} String_Element;
String_Element* stringList=NULL;

String_Element** stringUnique=NULL;

/*
 * C - Categorical
 * N - Nominal
 * P - To-Be-Predicted
 */
char data_layout[] = {'C','C','C','N','N','N','N','P'};
//char data_layout[] = {'N','N','P'};
int* layout_Unique;
int Matrix_Rows,Matrix_Cols;
struct timeb time_start,time_end;
int MaxBuffer = INT_MAX;

//float matrices post dummy coding

//A-part of AX=B
double** Multiply_Elements;
//Matrix post multiplication with its Transpose
double** Post_Multiply_Elements;

// Ax=B Apart
double* Multiply_Elements_CUDA;
//Matrix post multiplication for CUDA
double* Post_Multiplication_CUDA;

//B-part of AX=B
double* Result_Elements;
//Matrix post multiplication with its Transpose
double* Post_Result_Elements;

// B part CUDA
double* Result_Elements_CUDA;
//Matrix post multiplication for CUDA
double* Post_Result_CUDA;

//size of nxn matrix post multiplication
int FinalMatrixSize;

//replacement for the sawp
int replaceForSwap;

// declaring function to find all the coefficients
void findCoeffs();
/*
 * return 0 when zero 1 otherwise
 */
double epsilon = 0.000001;
int checkZero(double num){
	int result =1;
	if(num<0 && (-num)<epsilon ){
		result =0;
	}
	else if(num<epsilon){
		result=0;
	}
	return result;
} 

/**
 *	prints the result matrix 
 */
 void printMatrix(){
	int sq_row,sq_col;
	for(sq_row=0;sq_row<Matrix_Cols;sq_row++){
		for(sq_col=0;sq_col<Matrix_Cols;sq_col++){
			printf("%10.17f ",Post_Multiply_Elements[sq_row][sq_col]);
		}
		printf("\t result %10.17f \n",Post_Result_Elements[sq_row]);
	}
 }

/**
 * Find number of rows in the file
 */
int findFileLength(char* filename)
{
	int Num_Rows=0,iter;
	FILE* file = fopen(filename, "r");
	char buffer[2000];
	for(iter=0; iter<MaxBuffer; iter++) {
		if(fgets(buffer, 200, file)!=NULL) {
			Num_Rows++;
		} else {
			break;
		}
	}
	fclose(file);
	return Num_Rows;
}

/**
 * Load the file-data into a structure
 */
int loadStringData(char* filename, String_Element** strings)
{
	char buffer[2000];
	FILE* file;
	int iter_row,iter_col,len_string;
	int num_rows = findFileLength(filename);
	int num_columns = sizeof(data_layout)/sizeof(data_layout[0]);
	Matrix_Rows = num_rows-1;
	Matrix_Cols =num_columns;
	*strings = (String_Element*)malloc(num_rows*num_columns*sizeof(String_Element));

	file = fopen(filename, "r");
	fgets(buffer, 200, file);

	for(iter_row = 0; iter_row < num_rows; iter_row++) {
		char *token = fgets(buffer, 200, file);
		for(iter_col = 0; iter_col< num_columns; iter_col++) {
			(*strings)[iter_row*num_columns + iter_col].stringValue = "";
			if(token!=NULL) {
				if(iter_col==0) {
					token = strtok(buffer, ",");
				} else {
					token = strtok(NULL, ",");
				}
				len_string = strlen(token);
				(*strings)[iter_row*num_columns + iter_col].stringValue = (char*)malloc(sizeof(char)*(len_string+1));
				if(iter_col == num_columns - 1) {
					token[strlen(token) - 1] = ' ';
				}
				token[strlen(token)] = '\0';
				strcpy((*strings)[iter_row*num_columns + iter_col].stringValue,token);
			} else {
				return 0;
			}
		}

	}
	fclose(file);
	return 1;
}

/**
 * Does the array contain the string
 */
int doesListContain(char* string, int size)
{
	if(string !=NULL) {
		int iter;
		for(iter=0; iter<size; iter++) {
			if(strcmp( (stringList)[iter].stringValue, string)==0) {
				return 1;
			}
		}
	}
	return -1;
}

/**
 * Add string to the array
 */
void addToStringList(char* string, int size)
{
	if(string!=NULL && strlen(string)>0) {
		if(size==0) {
			//malloc
			stringList = (String_Element*)malloc(sizeof(String_Element));
		} else {
			//realloc size+1
			stringList = (String_Element*)realloc(stringList, (size+1)*sizeof(String_Element));
		}

		stringList[size].stringValue = (char*)malloc(strlen(string)+1);
		strcpy( (stringList)[size].stringValue,string);// Solved  !!!
	}
}

/**
 * Find number of unique-elements in a given column
 */
int uniqueCategs( int col_num, String_Element** strings)
{
	int iter_row, num_Unique=0;

	for(iter_row=0; iter_row<Matrix_Rows; iter_row++) {
		if((*strings)[iter_row*Matrix_Cols+col_num].stringValue!=NULL && strlen((*strings)[iter_row*Matrix_Cols+col_num].stringValue)>0 ) {

			if( doesListContain((*strings)[iter_row*Matrix_Cols+col_num].stringValue, num_Unique ) > 0) {
				continue;
			} else {
				addToStringList((*strings)[iter_row*Matrix_Cols+col_num].stringValue, num_Unique);
				++num_Unique;
			}
		}
	}

	return num_Unique;
}

/**
 * Find index of a particular unique-string
 */
int findIndexinUniqueArray(char* string , int row, int size)
{
	int itercol=0;
	if(string!=NULL) {
		for(itercol=0; itercol<=size; itercol++) {
			if(strcmp((stringUnique)[row][itercol].stringValue, string)==0) {
				break;
			}
		}
	}

	return itercol;
}

/**
 * Pre-process and perform dummy-coding
 */
int ModifyWithDummyCoding(char* filename, String_Element** strings)
{
	int iter;
	int temp_iter;
	int	num_categs=0;
	int final_cols = 0;
	int temp_Unique;
	int iter_col,iter_row;
	int index,current_col;
	ftime(&time_start);
	
	// to find-length of each row and populate strings
	loadStringData(filename, strings);

	printf("Reading from files with %d lines \n\n",Matrix_Rows);


//	gettimeofday(&time_start, NULL);
	// find number of categorical values
	for(iter=0; iter<sizeof(data_layout); iter++) {
		if(data_layout[iter]=='C') {
			num_categs++;
		}
	}

	// array to store unique elements
	stringUnique = (String_Element**)malloc(num_categs*sizeof(String_Element*));
	layout_Unique = (int*)malloc(sizeof(data_layout)*sizeof(int));
	num_categs=0;

	//find dummy columns to be added and keep track of unique elements
	for(iter=0; iter<sizeof(data_layout); iter++) {
		temp_Unique=0;
		if(data_layout[iter]=='N' || data_layout[iter]=='P') {
			final_cols++;
		} else if(data_layout[iter]=='C') {
			temp_Unique = uniqueCategs(iter, strings);
			if(temp_Unique>=3) {
				final_cols += temp_Unique-1;
			} else {
				final_cols ++;
			}

			//copy-data and free the stringList...here
			stringUnique[num_categs] = (String_Element*)malloc((temp_Unique)*sizeof(String_Element));
			for(temp_iter=0; temp_iter<temp_Unique; temp_iter++) {
				if((stringList)[temp_iter].stringValue!=NULL || strlen((stringList)[temp_iter].stringValue)>0) {
					(stringUnique)[num_categs][temp_iter].stringValue = (char*)malloc( sizeof( strlen( (stringList)[temp_iter].stringValue)));
					strcpy((stringUnique)[num_categs][temp_iter].stringValue, (stringList)[temp_iter].stringValue);
				}
			}
			num_categs++;
		}
		layout_Unique[iter] = temp_Unique;
		if(stringList!=NULL){
			free(stringList);
			stringList = NULL;
		}
			
	}

//	printf("Initializing data-structures after Dummy-Coding into %d columns \n", final_cols);

	//Resizing the multiplier-double-matrix's rows!!!
	Multiply_Elements = (double**)malloc(Matrix_Rows*sizeof(double*));
	//Resizing the result-double-matrix's rows!!!
	Result_Elements = (double*)malloc(Matrix_Rows*sizeof(double));

	// Resizing the multiplier-matrix for CUDA
	Multiply_Elements_CUDA = (double*)malloc(Matrix_Rows*final_cols*sizeof(double));
	//Resizing result for CUDA
	Result_Elements_CUDA =(double*)malloc(Matrix_Rows*sizeof(double));


	//add columns, initialize it to 0
	for(iter_row=0; iter_row<Matrix_Rows; iter_row++) {
		Multiply_Elements[iter_row]=(double*)malloc(final_cols*sizeof(double));
		Multiply_Elements[iter_row][0]=1;
		Multiply_Elements_CUDA[iter_row*final_cols]=1;
		for(iter_col=1; iter_col<final_cols; iter_col++) {
			Multiply_Elements[iter_row][iter_col]=0;
			Multiply_Elements_CUDA[iter_row*final_cols + iter_col] = 0;
		}
		Result_Elements_CUDA[iter_row]=0;

	}

//	printf("\n Adding Data to Columns after Dummy-Coding \n");
	//add data to Elements
	current_col=0;
	num_categs=0;
	for(iter_col=0; iter_col<sizeof(data_layout); iter_col++) {

		if(data_layout[iter_col]=='N' || data_layout[iter_col]=='P') {
			for(iter_row=0; iter_row<Matrix_Rows; iter_row++) {
				if( (*strings)[iter_row*Matrix_Cols+iter_col].stringValue!=NULL && strlen((*strings)[iter_row*Matrix_Cols+iter_col].stringValue)>0) {
					if(data_layout[iter_col]=='N') {
						Multiply_Elements[iter_row][current_col+1]= atof( (*strings)[iter_row*Matrix_Cols+iter_col].stringValue );
						Multiply_Elements_CUDA[iter_row*final_cols + current_col +1 ] = atof( (*strings)[iter_row*Matrix_Cols+iter_col].stringValue );
					} else if(data_layout[iter_col]=='P') {
						Result_Elements[iter_row]=atof( (*strings)[iter_row*Matrix_Cols+iter_col].stringValue );
						Result_Elements_CUDA[iter_row] = atof( (*strings)[iter_row*Matrix_Cols+iter_col].stringValue );
					}
				}
			}
			current_col++;
		} else if(data_layout[iter_col]=='C') {
			//from current-column to current-column+number-of-unique-values
			for(iter_row=0; iter_row<Matrix_Rows; iter_row++) {
				if( (*strings)[iter_row*Matrix_Cols+iter_col].stringValue!=NULL && strlen((*strings)[iter_row*Matrix_Cols+iter_col].stringValue)>0) {
					index = findIndexinUniqueArray((*strings)[iter_row*Matrix_Cols+iter_col].stringValue, num_categs, layout_Unique[iter_col]);
					if(index>0) {
						Multiply_Elements[iter_row][current_col+index]=1;
						Multiply_Elements_CUDA[iter_row*final_cols + current_col + index] = 1;
					}
				}
			}

			if(layout_Unique[iter_col]>2) {
				current_col+= layout_Unique[iter_col]-1;
			} else {
				current_col++;
			}
			num_categs++;
		}
	}
	Matrix_Cols = final_cols;


	
//	printf("Original Matrix ?? \n");getchar();
//	for(iter_row =0;iter_row<Matrix_Rows;iter_row++){
//		for(iter_col =0;iter_col<Matrix_Cols;iter_col++){
//			printf("%f ",Multiply_Elements[iter_row][iter_col]);
//		}
//		printf("\t result %f \n",Result_Elements[iter_row]);
//	}
//	printf("\n");

//	gettimeofday(&time_end, NULL);
//	printf("\n Finished pre-processing on Mul %f, Res %f in  %lu ms\n", Multiply_Elements[Matrix_Rows-1][final_cols-2],Result_Elements[Matrix_Rows-1] , (time_end.tv_usec - time_start.tv_usec)/1000);
	ftime(&time_end);
	iter_row = (int)(1000.0*(time_end.time - time_start.time)
	                 +(time_end.millitm - time_start.millitm));
	printf("\nFinished pre-processing on Mul %f, Res %f in %d ms \n", Multiply_Elements[Matrix_Rows-1][final_cols-1],Result_Elements[Matrix_Rows-1],iter_row );
	free(stringUnique);
	return 0;
}

/**
 *
 */

void initializeMultiplier(){
	//initializing
	int iter_row;
	Post_Multiply_Elements = (double**)malloc(Matrix_Cols*sizeof(double*));
	Post_Result_Elements = (double*)malloc(Matrix_Cols*sizeof(double));//because its a column matrix
	Post_Multiplication_CUDA = (double*)malloc(Matrix_Cols*Matrix_Cols*sizeof(double));
	Post_Result_CUDA = (double*)malloc(Matrix_Cols*sizeof(double));


	for(iter_row=0;iter_row<Matrix_Cols;iter_row++){
		Post_Multiply_Elements[iter_row] = (double*)malloc((Matrix_Cols)*sizeof(double));//initialized a row
	}


}

/**
 * Multiply A with its Transpose, B with A-transpose
 */
int multiplyWithTransposeNO_CUDA()
{
	
	int iter_row,sq_col,sq_row;
	double sum_post_multi=0;
	double sum_post_result=0;
	// Multiplying A with A(Transpose)
	ftime(&time_start);
	printf("\n");

	for(sq_row=0; sq_row<Matrix_Cols; sq_row++) {
		sum_post_result=0;
		for(sq_col=0; sq_col<Matrix_Cols; sq_col++) {
			sum_post_multi=0;
			for(iter_row=0; iter_row<Matrix_Rows; iter_row++) {
				sum_post_multi += Multiply_Elements[iter_row][sq_row] *Multiply_Elements[iter_row][sq_col];
				if(sq_col==0) {
					sum_post_result += Multiply_Elements[iter_row][sq_row] * Result_Elements[iter_row];
				}
			}
			Post_Multiply_Elements[sq_row][sq_col] = sum_post_multi;
		}
		Post_Result_Elements[sq_row] = sum_post_result;
//		printf("Completed %d of %d \n",(sq_row+1),(Matrix_Cols) );
	}

//	printf("\n\n Post Multiplication ??\n");getchar();
//	for(sq_row=0;sq_row<Matrix_Cols;sq_row++){
//		for(sq_col=0;sq_col<Matrix_Cols;sq_col++){
//			printf("%f ",Post_Multiply_Elements[sq_row][sq_col]);
//		}
//		printf("\t result %f \n",Post_Result_Elements[sq_row]);
//	}getchar();
	ftime(&time_end);
	iter_row = (int)(1000.0*(time_end.time - time_start.time)
	                 +(time_end.millitm - time_start.millitm));
	printf("\nMultiplied matrix with its transpose WITHOUT using CUDA in  %u ms \n ",iter_row);
	FinalMatrixSize = Matrix_Cols;
	free(Multiply_Elements);
	free(Result_Elements);
	// calling gaussian elimination
	GaussianEliminateforCoeffs();
	return 0;
}

/**
 * getting back to Multiply elements/woCUDA
 */
int Assign_2D(){
	int iter_row;
	int iter_col;
	for(iter_row=0;iter_row<Matrix_Cols;iter_row++){
		for(iter_col=0;iter_col<Matrix_Cols;iter_col++){
			Post_Multiply_Elements[iter_row][iter_col] = Post_Multiplication_CUDA[iter_row*Matrix_Cols + iter_col];
		}
		Post_Result_Elements[iter_row]=Post_Result_CUDA[iter_row];
	}
}

/**
 * Mulitiply A with-its transpose, B with A-transpose
 */
int multiplyWithTransposeWith_CUDA(){


	//post multipilication call below
	Assign_2D();
	// call gaussian
	FinalMatrixSize = Matrix_Cols;
	free(Multiply_Elements);
	free(Result_Elements);
	// calling gaussian elimination
	GaussianEliminateforCoeffs();
}

/*
 * Find the replacement
 * 0 -rows
 * 1 -colums
 * -1-No Replacement
 */
int findReplacement(int diag)
{
	int iter_row,iter_col;

	//try on the rows below diag !!!
	for(iter_row=Matrix_Cols-1; iter_row>diag; iter_row--) {
		if(checkZero(Post_Multiply_Elements[iter_row][diag])) {
			replaceForSwap = iter_row;
			return 0;
		}
	}

	for(iter_col=Matrix_Cols-1; iter_col>diag; iter_col--) {
		if(checkZero(Post_Multiply_Elements[diag][iter_col])) {
			replaceForSwap = iter_col;
			return 1;
		} else if(!checkZero(Post_Multiply_Elements[diag][iter_col])) {
			for(iter_row =Matrix_Cols-1; iter_row>diag; iter_row--) {
				if(checkZero(Post_Multiply_Elements[iter_row][iter_col])) {
					replaceForSwap = iter_col;
					return 1;
				}
			}
		}
	}

	return -1;
}

/**
 * swap Rows or Colums of the Matrix
 */
void swapMatrix(int RoworColumn, int swap1, int swap2)
{
	int iter;
	double temp_value;
	if(RoworColumn==0) { // row-swap so iter over columns
		for(iter=0; iter<FinalMatrixSize; iter++) {
			temp_value = Post_Multiply_Elements[swap1][iter];
			Post_Multiply_Elements[swap1][iter] = Post_Multiply_Elements[swap2][iter];
			Post_Multiply_Elements[swap2][iter]=temp_value;
		}
		
			// swapping the right had side
		temp_value = Post_Result_Elements[swap1];
		Post_Result_Elements[swap1] = Post_Result_Elements[swap2];
		Post_Result_Elements[swap2] = temp_value;

	} else if(RoworColumn==1) { // column-swap so iter over rows
		for(iter=0; iter<FinalMatrixSize; iter++) {
			temp_value = Post_Multiply_Elements[iter][swap1];
			Post_Multiply_Elements[iter][swap1] = Post_Multiply_Elements[iter][swap2];
			Post_Multiply_Elements[iter][swap2]=temp_value;
		}
	}


}

/*
 * Adjust matrix for any zero-diagonal-elements and Update final size of matrix;
 */
int AdjustDiagonalElements()
{
	int iter_diag,replacement;
	for(iter_diag=0; iter_diag<FinalMatrixSize; iter_diag++) {
		if(Post_Multiply_Elements[iter_diag][iter_diag]==0) {
			//found zero in iter_diag_row and iter_diag_col
			replacement = findReplacement(iter_diag);
			if(replacement==0) { //rows
				//access replaceForSwap for row-swapping
				swapMatrix(0, iter_diag, replaceForSwap);
			} else if(replacement==1) { //columns
				// access replaceForSwap for colum-swapping
				swapMatrix(1,iter_diag,replaceForSwap);
				//run the findReplacement again to get the row to swap
				findReplacement(iter_diag);
				swapMatrix(0, iter_diag, replaceForSwap);
			} else if(replacement<0) {
				FinalMatrixSize = iter_diag;
				return iter_diag;
			}
		}
	}
	return 0;
}

/**
 *	Ends up making all the rows below that colum into zeros
 * takes operations on all rows except on itself
 */
int clearRowsBelow(int diag){
	
	int iter_row,iter_col,replac;

	double divider=Post_Multiply_Elements[diag][diag];
	if(checkZero(divider)){
		//for-loop chalani
		for(iter_row=0; iter_row<FinalMatrixSize; iter_row++) {
			if(iter_row!=diag) {
				double multiplier=Post_Multiply_Elements[iter_row][diag];
				for(iter_col=0; iter_col<FinalMatrixSize; iter_col++) {
					Post_Multiply_Elements[iter_row][iter_col] -= (Post_Multiply_Elements[diag][iter_col]*multiplier)/(divider);
				}
					Post_Result_Elements[iter_row] -= (Post_Result_Elements[diag]*multiplier)/(divider);
			}
		}
	}else{
		replac = findReplacement(diag);
			
			if(replac==0) { //rows
//				printf("\n zeros at %f diagona at %d with replacement %d ... \n",Post_Multiply_Elements[diag-1][diag-1],diag,replaceForSwap);
//				getchar();printMatrix();

				//access replaceForSwap for row-swapping
				swapMatrix(0, diag, replaceForSwap);
//				printf("\n post swap replac zero with %f ... \n",Post_Multiply_Elements[diag][diag] );
				getchar();printMatrix();
				clearRowsBelow(diag);
			} else if(replac==1) { //columns
//				printf("\n zeros at %f diagona at %d with replacement %d !!! \n",Post_Multiply_Elements[diag-1][diag-1],diag,replaceForSwap);
//				getchar();printMatrix();
				// access replaceForSwap for colum-swapping
				swapMatrix(1,diag,replaceForSwap);
				//run the findReplacement again to get the row to swap
				findReplacement(diag);
				swapMatrix(0, diag, replaceForSwap);
//				printf("\n post swap replac one with %f !!! \n",Post_Multiply_Elements[diag][diag] );
//				getchar();printMatrix();
				clearRowsBelow(diag);
			} else if(replac<0) {
//				printf("\n\n\n\n\n zeros at %f diagona at %d and Cols are %d\n",Post_Multiply_Elements[diag-1][diag-1],diag,Matrix_Cols);getchar();
				FinalMatrixSize = diag;
//				printf(" Replac Zeros Aya THIS IS THE END \n\n\n\n\n");
				return -1;
			}
		
	}	
	return 0;
}

/*
 * After doing some series of row-eliminations
 * we convert it to a identity matrix
 * without CUDA
 */
void ConvertoIdentityMatrixWithoutCUDA()
{
	int iter_diag;

	//by clearing rows below them in the same colum
	for(iter_diag =0; iter_diag<Matrix_Cols; iter_diag++) {
		if(clearRowsBelow(iter_diag)==-1){
			break;
		}
	}

}

/*
 * GaussianEliminate to solve for Post_Multiply_Elements and Post_Result_Elements
 */
int GaussianEliminateforCoeffs()
{
	//int sq_row,sq_col;
	int timer;
	ftime(&time_start);
	printf("\n\n Gaussian !!!");
	AdjustDiagonalElements();

	ConvertoIdentityMatrixWithoutCUDA();
//	ConvertoIdentityMatrixWithCUDA();

//	printf("\n Post Gaussian Elimination??\n");getchar();
//	for(sq_row=0;sq_row<Matrix_Cols;sq_row++){
//		for(sq_col=0;sq_col<Matrix_Cols;sq_col++){
//			printf("%f ",Post_Multiply_Elements[sq_row][sq_col]);
//		}
//		printf("\t result %f \n",Post_Result_Elements[sq_row]);
//	}
	ftime(&time_end);
	timer = (int)(1000.0*(time_end.time - time_start.time)
	                 +(time_end.millitm - time_start.millitm));
	printf("\n Performed GaussianElimination WITHOUT using CUDA in  %u ms \n ",timer);
	
	findCoeffs();
	
	return 0;
}

void findCoeffs()
{
	int iter_diags;
	
	printf("\n\n the final %d coefficients are \n\n",FinalMatrixSize);
	for(iter_diags =0; iter_diags<FinalMatrixSize; iter_diags++) {
		double coeff=0;
		coeff = Post_Result_Elements[iter_diags] / Post_Multiply_Elements[iter_diags][iter_diags];
		printf(" %.18f \n",coeff);
	}
	for(iter_diags=0; iter_diags< (Matrix_Cols-FinalMatrixSize); iter_diags++) {
		printf(" N/A \n");
	}

}

int main()
{
	//file to get data from
	char* CSV_file = "F:/C/test.csv";
	
	//String matrix
	String_Element* strings;
	ModifyWithDummyCoding(CSV_file, &strings);
	initializeMultiplier();

	// CUDA Program for Multiplication and Gaussian Elimination and finding coefficients
	multiplyWithTransposeNO_CUDA();	
//	multiplyWithTransposeWith_CUDA();

	getchar();
	return 0;
}
