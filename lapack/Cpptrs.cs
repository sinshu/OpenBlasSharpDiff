using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CPPTRS solves a system of linear equations A*X = B with a Hermitian
        /// positive definite matrix A in packed storage using the Cholesky
        /// factorization A = U**H*U or A = L*L**H computed by CPPTRF.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
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
        /// <param name="ap">
        /// [in] AP is COMPLEX array, dimension (N*(N+1)/2).
        /// The triangular factor U or L from the Cholesky factorization
        /// A = U**H*U or A = L*L**H, packed columnwise in a linear
        /// array.  The j-th column of U or L is stored in the array AP
        /// as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = U(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = L(i,j) for j&lt;=i&lt;=n.
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX array, dimension (LDB,NRHS).
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cpptrs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cpptrs(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            Complex32* ap,
            Complex32* b,
            int ldb);
    }
}
