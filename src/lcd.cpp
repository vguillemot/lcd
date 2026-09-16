#include <queue>
#include <vector>
using namespace std;


int
is_linked_c(double *amat, int p, int i, int j)
{
    queue<int> q;
    vector<int> un;
    int k;
    for(int l = 0; l < p; l++)
	un.push_back(1);
    q.push(i);
    while(!q.empty()){
	k = q.front();
	q.pop();
	if(k == j){
	    return 1;
	} else {
	    for(int l = 0; l < p; l++)
		if(amat[k*p + l] == 1.0 && amat[k + p*l] == 1.0 && un[l] == 1){
		    q.push(l);
		    un[l] = 0;
		}
	}
    }
    return 0;
}


extern "C" {

#include <R.h>
#include <Rinternals.h>    

SEXP
is_linked(SEXP amat, SEXP iR, SEXP jR)
{
    SEXP dim_attr;
    int p;
    int i;
    int j;
    int res;
    SEXP result;
    int nprotect = 0;

    if (!Rf_isMatrix(amat) ||
        (TYPEOF(amat) != INTSXP && TYPEOF(amat) != REALSXP))
        Rf_error("amat must be a numeric matrix");
    dim_attr = Rf_getAttrib(amat, R_DimSymbol);
    if (LENGTH(dim_attr) != 2 || INTEGER(dim_attr)[0] != INTEGER(dim_attr)[1])
        Rf_error("amat must be a square matrix");
    if (TYPEOF(iR) != INTSXP || XLENGTH(iR) != 1 ||
        TYPEOF(jR) != INTSXP || XLENGTH(jR) != 1)
        Rf_error("i and j must be integer scalars");

    p = INTEGER(dim_attr)[0];
    i = INTEGER(iR)[0];
    j = INTEGER(jR)[0];
    if (i < 1 || i > p || j < 1 || j > p)
        Rf_error("i and j must be valid matrix indices");

    amat = PROTECT(coerceVector(amat, REALSXP));
    nprotect++;
    i--;
    j--;
    res = is_linked_c(REAL(amat), p, i, j);
    PROTECT(result = allocVector(INTSXP,1));
    nprotect++;
    INTEGER(result)[0] = res;
    UNPROTECT(nprotect);
    return result;
}




}// end of extern "C"



    
	    
    
    
