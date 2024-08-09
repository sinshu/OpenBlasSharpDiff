using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CPPTRF computes the Cholesky factorization of a complex Hermitian
        /// positive definite matrix A stored in packed format.
        /// </para>
        /// <para>
        /// The factorization has the form
        ///    A = U**H * U,  if UPLO = &#39;U&#39;, or
        ///    A = L  * L**H,  if UPLO = &#39;L&#39;,
        /// where U is an upper triangular matrix and L is lower triangular.
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
        /// <param name="ap">
        /// [in,out] AP is COMPLEX array, dimension (N*(N+1)/2).
        /// On entry, the upper or lower triangle of the Hermitian matrix
        /// A, packed columnwise in a linear array.  The j-th column of A
        /// is stored in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// See below for further details.
        /// 
        /// On exit, if INFO = 0, the triangular factor U or L from the
        /// Cholesky factorization A = U**H*U or A = L*L**H, in the same
        /// storage format as A.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the leading principal minor of order i
        /// is not positive definite, and the factorization could
        /// not be completed.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The packed storage scheme is illustrated by the following example
        ///  when N = 4, UPLO = &#39;U&#39;:
        /// </para>
        /// <para>
        ///  Two-dimensional storage of the Hermitian matrix A:
        /// </para>
        /// <para>
        ///     a11 a12 a13 a14
        ///         a22 a23 a24
        ///             a33 a34     (aij = conjg(aji))
        ///                 a44
        /// </para>
        /// <para>
        ///  Packed storage of the upper triangle of A:
        /// </para>
        /// <para>
        ///  AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cpptrf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cpptrf(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex32* ap);
    }
}
