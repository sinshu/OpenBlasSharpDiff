using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZPPTRI computes the inverse of a complex Hermitian positive definite
        /// matrix A using the Cholesky factorization A = U**H*U or A = L*L**H
        /// computed by ZPPTRF.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangular factor is stored in AP;
        /// = &#39;L&#39;:  Lower triangular factor is stored in AP.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="ap">
        /// [in,out] AP is COMPLEX*16 array, dimension (N*(N+1)/2).
        /// On entry, the triangular factor U or L from the Cholesky
        /// factorization A = U**H*U or A = L*L**H, packed columnwise as
        /// a linear array.  The j-th column of U or L is stored in the
        /// array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = U(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = L(i,j) for j&lt;=i&lt;=n.
        /// 
        /// On exit, the upper or lower triangle of the (Hermitian)
        /// inverse of A, overwriting the input factor U or L.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the (i,i) element of the factor U or L is
        /// zero, and the inverse could not be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zpptri", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zpptri(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex* ap);
    }
}
