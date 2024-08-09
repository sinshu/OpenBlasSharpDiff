using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DTBTRS solves a triangular system of the form
        /// </para>
        /// <para>
        ///    A * X = B  or  A**T * X = B,
        /// </para>
        /// <para>
        /// where A is a triangular band matrix of order N, and B is an
        /// N-by NRHS matrix.  A check is made to verify that A is nonsingular.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  A is upper triangular;
        /// = &#39;L&#39;:  A is lower triangular.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// Specifies the form the system of equations:
        /// = &#39;N&#39;:  A * X = B  (No transpose)
        /// = &#39;T&#39;:  A**T * X = B  (Transpose)
        /// = &#39;C&#39;:  A**H * X = B  (Conjugate transpose = Transpose)
        /// </param>
        /// <param name="diag">
        /// [in] DIAG is CHARACTER*1.
        /// = &#39;N&#39;:  A is non-unit triangular;
        /// = &#39;U&#39;:  A is unit triangular.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="kd">
        /// [in] KD is INTEGER.
        /// The number of superdiagonals or subdiagonals of the
        /// triangular band matrix A.  KD &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in] AB is DOUBLE PRECISION array, dimension (LDAB,N).
        /// The upper or lower triangular band matrix A, stored in the
        /// first kd+1 rows of AB.  The j-th column of A is stored
        /// in the j-th column of the array AB as follows:
        /// if UPLO = &#39;U&#39;, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AB(1+i-j,j)    = A(i,j) for j&lt;=i&lt;=min(n,j+kd).
        /// If DIAG = &#39;U&#39;, the diagonal elements of A are not referenced
        /// and are assumed to be 1.
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KD+1.
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
        /// On entry, the right hand side matrix B.
        /// On exit, if INFO = 0, the solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the i-th diagonal element of A is zero,
        /// indicating that the matrix is singular and the
        /// solutions X have not been computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dtbtrs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dtbtrs(
            MatrixLayout matrixLayout,
            char uplo,
            char trans,
            char diag,
            int n,
            int kd,
            int nrhs,
            double* ab,
            int ldab,
            double* b,
            int ldb);
    }
}
