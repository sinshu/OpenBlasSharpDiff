using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SGETRS solves a system of linear equations
        ///    A * X = B  or  A**T * X = B
        /// with a general N-by-N matrix A using the LU factorization computed
        /// by SGETRF.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// Specifies the form of the system of equations:
        /// = &#39;N&#39;:  A * X = B  (No transpose)
        /// = &#39;T&#39;:  A**T* X = B  (Transpose)
        /// = &#39;C&#39;:  A**T* X = B  (Conjugate transpose = Transpose)
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
        /// <param name="a">
        /// [in] A is REAL array, dimension (LDA,N).
        /// The factors L and U from the factorization A = P*L*U
        /// as computed by SGETRF.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// The pivot indices from SGETRF; for 1&lt;=i&lt;=N, row i of the
        /// matrix was interchanged with row IPIV(i).
        /// </param>
        /// <param name="b">
        /// [in,out] B is REAL array, dimension (LDB,NRHS).
        /// On entry, the right hand side matrix B.
        /// On exit, the solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sgetrs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sgetrs(
            MatrixLayout matrixLayout,
            char trans,
            int n,
            int nrhs,
            float* a,
            int lda,
            int* ipiv,
            float* b,
            int ldb);
    }
}
