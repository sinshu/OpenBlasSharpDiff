using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CSYSVX uses the diagonal pivoting factorization to compute the
        /// solution to a complex system of linear equations A * X = B,
        /// where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
        /// matrices.
        /// </para>
        /// <para>
        /// Error bounds on the solution and a condition estimate are also
        /// provided.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="fact">
        /// [in] FACT is CHARACTER*1.
        /// Specifies whether or not the factored form of A has been
        /// supplied on entry.
        /// = &#39;F&#39;:  On entry, AF and IPIV contain the factored form
        /// of A.  A, AF and IPIV will not be modified.
        /// = &#39;N&#39;:  The matrix A will be copied to AF and factored.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of linear equations, i.e., the order of the
        /// matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrices B and X.  NRHS &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX array, dimension (LDA,N).
        /// The symmetric matrix A.  If UPLO = &#39;U&#39;, the leading N-by-N
        /// upper triangular part of A contains the upper triangular part
        /// of the matrix A, and the strictly lower triangular part of A
        /// is not referenced.  If UPLO = &#39;L&#39;, the leading N-by-N lower
        /// triangular part of A contains the lower triangular part of
        /// the matrix A, and the strictly upper triangular part of A is
        /// not referenced.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="af">
        /// [in,out] AF is COMPLEX array, dimension (LDAF,N).
        /// If FACT = &#39;F&#39;, then AF is an input argument and on entry
        /// contains the block diagonal matrix D and the multipliers used
        /// to obtain the factor U or L from the factorization
        /// A = U*D*U**T or A = L*D*L**T as computed by CSYTRF.
        /// 
        /// If FACT = &#39;N&#39;, then AF is an output argument and on exit
        /// returns the block diagonal matrix D and the multipliers used
        /// to obtain the factor U or L from the factorization
        /// A = U*D*U**T or A = L*D*L**T.
        /// </param>
        /// <param name="ldaf">
        /// [in] LDAF is INTEGER.
        /// The leading dimension of the array AF.  LDAF &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [in,out] IPIV is INTEGER array, dimension (N).
        /// If FACT = &#39;F&#39;, then IPIV is an input argument and on entry
        /// contains details of the interchanges and the block structure
        /// of D, as determined by CSYTRF.
        /// If IPIV(k) &gt; 0, then rows and columns k and IPIV(k) were
        /// interchanged and D(k,k) is a 1-by-1 diagonal block.
        /// If UPLO = &#39;U&#39; and IPIV(k) = IPIV(k-1) &lt; 0, then rows and
        /// columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k)
        /// is a 2-by-2 diagonal block.  If UPLO = &#39;L&#39; and IPIV(k) =
        /// IPIV(k+1) &lt; 0, then rows and columns k+1 and -IPIV(k) were
        /// interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
        /// 
        /// If FACT = &#39;N&#39;, then IPIV is an output argument and on exit
        /// contains details of the interchanges and the block structure
        /// of D, as determined by CSYTRF.
        /// </param>
        /// <param name="b">
        /// [in] B is COMPLEX array, dimension (LDB,NRHS).
        /// The N-by-NRHS right hand side matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [out] X is COMPLEX array, dimension (LDX,NRHS).
        /// If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is REAL.
        /// The estimate of the reciprocal condition number of the matrix
        /// A.  If RCOND is less than the machine precision (in
        /// particular, if RCOND = 0), the matrix is singular to working
        /// precision.  This condition is indicated by a return code of
        /// INFO &gt; 0.
        /// </param>
        /// <param name="ferr">
        /// [out] FERR is REAL array, dimension (NRHS).
        /// The estimated forward error bound for each solution vector
        /// X(j) (the j-th column of the solution matrix X).
        /// If XTRUE is the true solution corresponding to X(j), FERR(j)
        /// is an estimated upper bound for the magnitude of the largest
        /// element in (X(j) - XTRUE) divided by the magnitude of the
        /// largest element in X(j).  The estimate is as reliable as
        /// the estimate for RCOND, and is almost always a slight
        /// overestimate of the true error.
        /// </param>
        /// <param name="berr">
        /// [out] BERR is REAL array, dimension (NRHS).
        /// The componentwise relative backward error of each solution
        /// vector X(j) (i.e., the smallest relative change in
        /// any element of A or B that makes X(j) an exact solution).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, and i is
        /// &lt;= N:  D(i,i) is exactly zero.  The factorization
        /// has been completed but the factor D is exactly
        /// singular, so the solution and error bounds could
        /// not be computed. RCOND = 0 is returned.
        /// = N+1: D is nonsingular, but RCOND is less than machine
        /// precision, meaning that the matrix is singular
        /// to working precision.  Nevertheless, the
        /// solution and error bounds are computed because
        /// there are a number of situations where the
        /// computed solution can be more accurate than the
        /// value of RCOND would suggest.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_csysvx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Csysvx(
            MatrixLayout matrixLayout,
            char fact,
            char uplo,
            int n,
            int nrhs,
            Complex32* a,
            int lda,
            Complex32* af,
            int ldaf,
            int* ipiv,
            Complex32* b,
            int ldb,
            Complex32* x,
            int ldx,
            float* rcond,
            float* ferr,
            float* berr);
    }
}
