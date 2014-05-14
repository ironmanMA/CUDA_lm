#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/timeb.h>

typedef struct {
	char* stringValue;
} String_Element;
String_Element* stringList;

String_Element** stringUnique;

/*
 * C - Categorical
 * N - Nominal
 * P - To-Be-Predicted
 */
//char data_layout[] = {'C','C','C','N','N','N','N','P'};
char data_layout[] = {'N','N','P'};
int* layout_Unique;
int Matrix_Rows,Matrix_Cols;
struct timeb time_start,time_end;
int MaxBuffer = 10000;

//float matrices post dummy coding

//A-part of AX=B
double** Multiply_Elements;
//Matrix post multiplication with its Transpose
double** Post_Multiply_Elements;

//B-part of AX=B
double* Result_Elements;
//Matrix post multiplication with its Transpose
double* Post_Result_Elements;

//size of nxn matrix post multiplication
int FinalMatrixSize;

//replacement for the sawp
int replaceForSwap;



/**
* To print matrices
*/
void print(char* message){
/** to test ***/
		int sq_row,sq_col;
		printf("\n %s \n",message);
		for(sq_row=0;sq_row<Matrix_Cols;sq_row++){
			for(sq_col=0;sq_col<Matrix_Cols;sq_col++){
				printf("%f ",Post_Multiply_Elements[sq_row][sq_col]);
			}printf("\n");
		}printf("\n");printf(" Result Matrix \n");
		for(sq_row=0;sq_row<Matrix_Cols;sq_row++){
			printf("%f ",Post_Result_Elements[sq_row]);
			printf("\n");
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

	int iter_row,iter_col,len_string;
	int num_rows = findFileLength(filename);
	int num_columns = sizeof(data_layout)/sizeof(data_layout[0]);
	Matrix_Rows = num_rows-1;
	Matrix_Cols =num_columns;
	*strings = (String_Element*)malloc(num_rows*num_columns*sizeof(String_Element));

	FILE* file = fopen(filename, "r");
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
	int iter,temp_iter,num_categs=0;
	int final_cols = 0;
	int temp_Unique;
	int iter_col,iter_row;

	// to find-length of each row and populate strings
	loadStringData(filename, strings);

	printf("After fclose with %d lines \n\n",Matrix_Rows);


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
		free(stringList);
	}

	printf("Columns after Dummy-Coding %d \n", final_cols);

	//Resizing the multiplier-double-matrix's rows!!!
	Multiply_Elements = (double**)malloc(Matrix_Rows*sizeof(double*));
	//Resizing the result-double-matrix's rows!!!
	Result_Elements = (double*)malloc(Matrix_Rows*sizeof(double));


	//add columns, initialize it to 0
	for(iter_row=0; iter_row<Matrix_Rows; iter_row++) {
		Multiply_Elements[iter_row]=(double*)malloc(final_cols*sizeof(double));

		Multiply_Elements[iter_row][0]=1;
		for(iter_col=1; iter_col<final_cols; iter_col++) {
			Multiply_Elements[iter_row][iter_col]=0;
		}

		if(iter_row%1000 == 0) {
			printf("Initialzing Column.No %d of %d \n", iter_row,Matrix_Rows);
		}
	}

	printf("\n Adding Data to Columns after Dummy-Coding \n");
	//add data to Elements
	int index,current_col=0;
	num_categs=0;
	for(iter_col=0; iter_col<sizeof(data_layout); iter_col++) {

		if(data_layout[iter_col]=='N' || data_layout[iter_col]=='P') {
			for(iter_row=0; iter_row<Matrix_Rows; iter_row++) {
				if( (*strings)[iter_row*Matrix_Cols+iter_col].stringValue!=NULL && strlen((*strings)[iter_row*Matrix_Cols+iter_col].stringValue)>0) {
					if(data_layout[iter_col]=='N') {
						Multiply_Elements[iter_row][current_col+1]= atof( (*strings)[iter_row*Matrix_Cols+iter_col].stringValue );
					} else if(data_layout[iter_col]=='P') {
						Result_Elements[iter_row]=atof( (*strings)[iter_row*Matrix_Cols+iter_col].stringValue );
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


	
	printf("Original Matrix ?? \n");getchar();
	for(iter_row =0;iter_row<Matrix_Rows;iter_row++){
		for(iter_col =0;iter_col<Matrix_Cols;iter_col++){
			printf("%f ",Multiply_Elements[iter_row][iter_col]);
		}
		printf("\t result %f \n",Result_Elements[iter_row]);
	}
	printf("\n");

//	gettimeofday(&time_end, NULL);
//	printf("\n Finished pre-processing on Mul %f, Res %f in  %lu ms\n", Multiply_Elements[Matrix_Rows-1][final_cols-2],Result_Elements[Matrix_Rows-1] , (time_end.tv_usec - time_start.tv_usec)/1000);
	printf("\n Finished pre-processing on Mul %f, Res %f \n", Multiply_Elements[Matrix_Rows-1][final_cols-1],Result_Elements[Matrix_Rows-1] );
	free(stringUnique);
	return 0;
}

/**
 * Multiply A with its Transpose, B with A-transpose
 */
int multiplyWithTransposeNO_CUDA()
{
	ftime(&time_start);
	int iter_row,sq_col,sq_row;
	printf("\n");
	double sum_post_multi=0;
	double sum_post_result=0;
	// Multiplying A with A(Transpose)
	//initializing
	Post_Multiply_Elements = (double**)malloc(Matrix_Cols*sizeof(double*));
	Post_Result_Elements = (double*)malloc(Matrix_Cols*sizeof(double));//because its a column matrix

	for(sq_row=0; sq_row<Matrix_Cols; sq_row++) {
		Post_Multiply_Elements[sq_row] = (double*)malloc((Matrix_Cols)*sizeof(double));//initialized a row
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
		printf("Completed %d of %d \n",(sq_row+1),(Matrix_Cols) );
	}

	printf("\n\n Post Multiplication ??\n");getchar();
	for(sq_row=0;sq_row<Matrix_Cols;sq_row++){
		for(sq_col=0;sq_col<Matrix_Cols;sq_col++){
			printf("%f ",Post_Multiply_Elements[sq_row][sq_col]);
		}
		printf("\t result %f \n",Post_Result_Elements[sq_row]);
	}

	ftime(&time_end);
	iter_row = (int)(1000.0*(time_end.time - time_start.time)
	                 +(time_end.millitm - time_start.millitm));
	printf("\nMultiplied matrix with its transpose WITHOUT using CUDA in  %u ms \n ",iter_row);
	FinalMatrixSize = Matrix_Cols;
	free(Multiply_Elements);
	free(Result_Elements);

	return 0;
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
	for(iter_row=diag+1; iter_row<FinalMatrixSize; iter_row++) {
		if(Post_Multiply_Elements[iter_row][diag]!=0) {
			replaceForSwap = iter_row;
			return 0;
		}
	}

	for(iter_col=diag+1; iter_col<FinalMatrixSize; iter_col++) {
		if(Post_Multiply_Elements[diag][iter_col]!=0) {
			replaceForSwap = iter_col;
			return 1;
		} else if(Post_Multiply_Elements[diag][iter_col]==0) {
			for(iter_row = diag+1; iter_row<FinalMatrixSize; iter_row++) {
				if(Post_Multiply_Elements[iter_row][iter_col]!=0) {
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

	} else if(RoworColumn==1) { // column-swap so iter over rows
		for(iter=0; iter<FinalMatrixSize; iter++) {
			temp_value = Post_Multiply_Elements[iter][swap1];
			Post_Multiply_Elements[iter][swap1] = Post_Multiply_Elements[iter][swap2];
			Post_Multiply_Elements[iter][swap2]=temp_value;
		}

	}
	// swapping the right had side
	temp_value = Post_Result_Elements[swap1];
	Post_Result_Elements[swap1] = Post_Result_Elements[swap2];
	Post_Result_Elements[swap2] = temp_value;

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
void clearRowsBelow(int diag)
{
	int iter_row,iter_col;

	for(iter_row=0; iter_row<FinalMatrixSize; iter_row++) {
		if(iter_row!=diag) {
			double multiplier=Post_Multiply_Elements[iter_row][diag];
			double divider=Post_Multiply_Elements[diag][diag];
			for(iter_col=0; iter_col<FinalMatrixSize; iter_col++) {
//				Post_Multiply_Elements[iter_row][iter_col] -= (Post_Multiply_Elements[iter_row][iter_col]*Post_Multiply_Elements[diag][diag] - Post_Multiply_Elements[iter_row][diag]*Post_Multiply_Elements[diag][iter_col]);
				Post_Multiply_Elements[iter_row][iter_col] -= (Post_Multiply_Elements[diag][iter_col]*multiplier)/(divider);
			}
//			Post_Result_Elements[iter_row] -= (Post_Result_Elements[iter_row]*Post_Multiply_Elements[diag][diag] - Post_Multiply_Elements[iter_row][diag]*Post_Result_Elements[diag]);
			Post_Result_Elements[iter_row] -= (Post_Result_Elements[diag]*multiplier)/(divider);
		}
	}

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
	for(iter_diag =0; iter_diag<FinalMatrixSize; iter_diag++) {
		clearRowsBelow(iter_diag);
	}

}

/*
 * GaussianEliminate to solve for Post_Multiply_Elements and Post_Result_Elements
 */
int GaussianEliminateforCoeffs()
{
	int sq_row,sq_col;
	printf("\n\n Gaussian !!!");
	AdjustDiagonalElements();

	ConvertoIdentityMatrixWithoutCUDA();
//	ConvertoIdentityMatrixWithCUDA();

	printf("\n Post Gaussian Elimination??\n");getchar();
	for(sq_row=0;sq_row<Matrix_Cols;sq_row++){
		for(sq_col=0;sq_col<Matrix_Cols;sq_col++){
			printf("%f ",Post_Multiply_Elements[sq_row][sq_col]);
		}
		printf("\t result %f \n",Post_Result_Elements[sq_row]);
	}
	
	printf("\n final size to be dealt with is %d",FinalMatrixSize);
	
	return 0;
}

void findCoeffs()
{
	int iter_diags;
	printf("\n\n the coefficients are \n\n");
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
	char* CSV_file = "F:/C/tests2.csv";
	
	//String matrix
	String_Element* strings;
	ModifyWithDummyCoding(CSV_file, &strings);
	//further Actual Algo !!!
	multiplyWithTransposeNO_CUDA();
	
//	multiplyWithTransposeWith_CUDA();
	GaussianEliminateforCoeffs();
	findCoeffs();

	getchar();
	return 0;
}
