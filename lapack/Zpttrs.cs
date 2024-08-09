using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZPTTRS solves a tridiagonal system of the form
        ///    A * X = B
        /// using the factorization A = U**H *D* U or A = L*D*L**H computed by ZPTTRF.
        /// D is a diagonal matrix specified in the vector D, U (or L) is a unit
        /// bidiagonal matrix whose superdiagonal (subdiagonal) is specified in
        /// the vector E, and X and B are N by NRHS matrices.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies the form of the factorization and whether the
        /// vector E is the superdiagonal of the upper bidiagonal factor
        /// U or the subdiagonal of the lower bidiagonal factor L.
        /// = &#39;U&#39;:  A = U**H *D*U, E is the superdiagonal of U
        /// = &#39;L&#39;:  A = L*D*L**H, E is the subdiagonal of L
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the tridiagonal matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in] D is DOUBLE PRECISION array, dimension (N).
        /// The n diagonal elements of the diagonal matrix D from the
        /// factorization A = U**H *D*U or A = L*D*L**H.
        /// </param>
        /// <param name="e">
        /// [in] E is COMPLEX*16 array, dimension (N-1).
        /// If UPLO = &#39;U&#39;, the (n-1) superdiagonal elements of the unit
        /// bidiagonal factor U from the factorization A = U**H*D*U.
        /// If UPLO = &#39;L&#39;, the (n-1) subdiagonal elements of the unit
        /// bidiagonal factor L from the factorization A = L*D*L**H.
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,NRHS).
        /// On entry, the right hand side vectors B for the system of
        /// linear equations.
        /// On exit, the solution vectors, X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -k, the k-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zpttrs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zpttrs(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            double* d,
            Complex* e,
            Complex* b,
            int ldb);
    }
}
