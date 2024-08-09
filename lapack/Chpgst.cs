using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CHPGST reduces a complex Hermitian-definite generalized
        /// eigenproblem to standard form, using packed storage.
        /// </para>
        /// <para>
        /// If ITYPE = 1, the problem is A*x = lambda*B*x,
        /// and A is overwritten by inv(U**H)*A*inv(U) or inv(L)*A*inv(L**H)
        /// </para>
        /// <para>
        /// If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
        /// B*A*x = lambda*x, and A is overwritten by U*A*U**H or L**H*A*L.
        /// </para>
        /// <para>
        /// B must have been previously factorized as U**H*U or L*L**H by CPPTRF.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="itype">
        /// [in] ITYPE is INTEGER.
        /// = 1: compute inv(U**H)*A*inv(U) or inv(L)*A*inv(L**H);
        /// = 2 or 3: compute U*A*U**H or L**H*A*L.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored and B is factored as
        /// U**H*U;
        /// = &#39;L&#39;:  Lower triangle of A is stored and B is factored as
        /// L*L**H.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices A and B.  N &gt;= 0.
        /// </param>
        /// <param name="ap">
        /// [in,out] AP is COMPLEX array, dimension (N*(N+1)/2).
        /// On entry, the upper or lower triangle of the Hermitian matrix
        /// A, packed columnwise in a linear array.  The j-th column of A
        /// is stored in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// 
        /// On exit, if INFO = 0, the transformed matrix, stored in the
        /// same format as A.
        /// </param>
        /// <param name="bp">
        /// [in] BP is COMPLEX array, dimension (N*(N+1)/2).
        /// The triangular factor from the Cholesky factorization of B,
        /// stored in the same format as A, as returned by CPPTRF.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_chpgst", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Chpgst(
            MatrixLayout matrixLayout,
            int itype,
            char uplo,
            int n,
            Complex32* ap,
            Complex32* bp);
    }
}
