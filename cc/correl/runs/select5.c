#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MAXEVE 649110


struct data {
   int i1;
   int i2;
   float dt1;
   float cc1;
   float dt2;
   float cc2;
};

main(int argc,char *argv[]){

/*  cc -o select5 select5.c -lm   */


FILE *cfPtr;
FILE *cfPtr2;
struct data dat;
int *ids = malloc(MAXEVE*sizeof(int));
int i, nev, nev2, ct, ctt, ct3, id1, id2, ind1, ind2, i1, i2;

ct=0;

if (argc == 1){
   printf("Usage: select5 NET.STA.cc\n");
   return 1;
}
else{ 


cfPtr2 = fopen("select.out","w");


if ((cfPtr = fopen(*++argv,"rb")) == NULL)
   printf("File could not be open. %s\n",*argv);
else {
     fread(&nev,sizeof(int),1,cfPtr);
     for(i = 0; i <= nev-1; i++) {
        fread(&ids[i],sizeof(int),1,cfPtr);
     }
     i=0;
     while (!feof(cfPtr)) {
     fread(&dat,sizeof(struct data),1,cfPtr);
     if ((dat.cc1 >= 0.0) && (fabs(dat.dt1-dat.dt2)<=.01)) { 
/*     if ((dat.cc1 >= 0.7)) { */
         ct++;

fprintf(cfPtr2,"%9d %9d %8.4f %4.2f\n", ids[dat.i1-1], ids[dat.i2-1], -dat.dt1,dat.cc1);
         }
     }
     fclose(cfPtr);
     printf("Matching pairs: %d of %d   0.03: %d\n",ct,ctt,ct3);
}
     fclose(cfPtr2);
}

free(ids);

return 0;
}

