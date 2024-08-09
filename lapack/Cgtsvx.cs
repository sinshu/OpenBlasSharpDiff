using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGTSVX uses the LU factorization to compute the solution to a complex
        /// system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
        /// where A is a tridiagonal matrix of order N and X and B are N-by-NRHS
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
        /// = &#39;F&#39;:  DLF, DF, DUF, DU2, and IPIV contain the factored form
        /// of A; DL, D, DU, DLF, DF, DUF, DU2 and IPIV will not
        /// be modified.
        /// = &#39;N&#39;:  The matrix will be copied to DLF, DF, and DUF
        /// and factored.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// Specifies the form of the system of equations:
        /// = &#39;N&#39;:  A * X = B     (No transpose)
        /// = &#39;T&#39;:  A**T * X = B  (Transpose)
        /// = &#39;C&#39;:  A**H * X = B  (Conjugate transpose)
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="dl">
        /// [in] DL is COMPLEX array, dimension (N-1).
        /// The (n-1) subdiagonal elements of A.
        /// </param>
        /// <param name="d">
        /// [in] D is COMPLEX array, dimension (N).
        /// The n diagonal elements of A.
        /// </param>
        /// <param name="du">
        /// [in] DU is COMPLEX array, dimension (N-1).
        /// The (n-1) superdiagonal elements of A.
        /// </param>
        /// <param name="dlf">
        /// [in,out] DLF is COMPLEX array, dimension (N-1).
        /// If FACT = &#39;F&#39;, then DLF is an input argument and on entry
        /// contains the (n-1) multipliers that define the matrix L from
        /// the LU factorization of A as computed by CGTTRF.
        /// 
        /// If FACT = &#39;N&#39;, then DLF is an output argument and on exit
        /// contains the (n-1) multipliers that define the matrix L from
        /// the LU factorization of A.
        /// </param>
        /// <param name="df">
        /// [in,out] DF is COMPLEX array, dimension (N).
        /// If FACT = &#39;F&#39;, then DF is an input argument and on entry
        /// contains the n diagonal elements of the upper triangular
        /// matrix U from the LU factorization of A.
        /// 
        /// If FACT = &#39;N&#39;, then DF is an output argument and on exit
        /// contains the n diagonal elements of the upper triangular
        /// matrix U from the LU factorization of A.
        /// </param>
        /// <param name="duf">
        /// [in,out] DUF is COMPLEX array, dimension (N-1).
        /// If FACT = &#39;F&#39;, then DUF is an input argument and on entry
        /// contains the (n-1) elements of the first superdiagonal of U.
        /// 
        /// If FACT = &#39;N&#39;, then DUF is an output argument and on exit
        /// contains the (n-1) elements of the first superdiagonal of U.
        /// </param>
        /// <param name="du2">
        /// [in,out] DU2 is COMPLEX array, dimension (N-2).
        /// If FACT = &#39;F&#39;, then DU2 is an input argument and on entry
        /// contains the (n-2) elements of the second superdiagonal of
        /// U.
        /// 
        /// If FACT = &#39;N&#39;, then DU2 is an output argument and on exit
        /// contains the (n-2) elements of the second superdiagonal of
        /// U.
        /// </param>
        /// <param name="ipiv">
        /// [in,out] IPIV is INTEGER array, dimension (N).
        /// If FACT = &#39;F&#39;, then IPIV is an input argument and on entry
        /// contains the pivot indices from the LU factorization of A as
        /// computed by CGTTRF.
        /// 
        /// If FACT = &#39;N&#39;, then IPIV is an output argument and on exit
        /// contains the pivot indices from the LU factorization of A;
        /// row i of the matrix was interchanged with row IPIV(i).
        /// IPIV(i) will always be either i or i+1; IPIV(i) = i indicates
        /// a row interchange was not required.
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
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, and i is
        /// &lt;= N:  U(i,i) is exactly zero.  The factorization
        /// has not been completed unless i = N, but the
        /// factor U is exactly singular, so the solution
        /// and error bounds could not be computed.
        /// RCOND = 0 is returned.
        /// = N+1: U is nonsingular, but RCOND is less than machine
        /// precision, meaning that the matrix is singular
        /// to working precision.  Nevertheless, the
        /// solution and error bounds are computed because
        /// there are a number of situations where the
        /// computed solution can be more accurate than the
        /// value of RCOND would suggest.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgtsvx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgtsvx(
            MatrixLayout matrixLayout,
            char fact,
            char trans,
            int n,
            int nrhs,
            Complex32* dl,
            Complex32* d,
            Complex32* du,
            Complex32* dlf,
            Complex32* df,
            Complex32* duf,
            Complex32* du2,
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
