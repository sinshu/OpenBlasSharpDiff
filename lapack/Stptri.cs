using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// STPTRI computes the inverse of a real upper or lower triangular
        /// matrix A stored in packed format.
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
        /// <param name="diag">
        /// [in] DIAG is CHARACTER*1.
        /// = &#39;N&#39;:  A is non-unit triangular;
        /// = &#39;U&#39;:  A is unit triangular.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="ap">
        /// [in,out] AP is REAL array, dimension (N*(N+1)/2).
        /// On entry, the upper or lower triangular matrix A, stored
        /// columnwise in a linear array.  The j-th column of A is stored
        /// in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*((2*n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// See below for further details.
        /// On exit, the (triangular) inverse of the original matrix, in
        /// the same packed storage format.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, A(i,i) is exactly zero.  The triangular
        /// matrix is singular and its inverse can not be computed.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  A triangular matrix A can be transferred to packed storage using one
        ///  of the following program segments:
        /// </para>
        /// <para>
        ///  UPLO = &#39;U&#39;:                      UPLO = &#39;L&#39;:
        /// </para>
        /// <para>
        ///        JC = 1                           JC = 1
        ///        DO 2 J = 1, N                    DO 2 J = 1, N
        ///           DO 1 I = 1, J                    DO 1 I = J, N
        ///              AP(JC+I-1) = A(I,J)              AP(JC+I-J) = A(I,J)
        ///      1    CONTINUE                    1    CONTINUE
        ///           JC = JC + J                      JC = JC + N - J + 1
        ///      2 CONTINUE                       2 CONTINUE
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_stptri", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Stptri(
            MatrixLayout matrixLayout,
            char uplo,
            char diag,
            int n,
            float* ap);
    }
}
