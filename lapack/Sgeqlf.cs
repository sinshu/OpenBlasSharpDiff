using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SGEQLF computes a QL factorization of a real M-by-N matrix A:
        /// A = Q * L.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is REAL array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit,
        /// if m &gt;= n, the lower triangle of the subarray
        /// A(m-n+1:m,1:n) contains the N-by-N lower triangular matrix L;
        /// if m &lt;= n, the elements on and below the (n-m)-th
        /// superdiagonal contain the M-by-N lower trapezoidal matrix L;
        /// the remaining elements, with the array TAU, represent the
        /// orthogonal matrix Q as a product of elementary reflectors
        /// (see Further Details).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="tau">
        /// [out] TAU is REAL array, dimension (min(M,N)).
        /// The scalar factors of the elementary reflectors (see Further
        /// Details).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The matrix Q is represented as a product of elementary reflectors
        /// </para>
        /// <para>
        ///     Q = H(k) . . . H(2) H(1), where k = min(m,n).
        /// </para>
        /// <para>
        ///  Each H(i) has the form
        /// </para>
        /// <para>
        ///     H(i) = I - tau * v * v**T
        /// </para>
        /// <para>
        ///  where tau is a real scalar, and v is a real vector with
        ///  v(m-k+i+1:m) = 0 and v(m-k+i) = 1; v(1:m-k+i-1) is stored on exit in
        ///  A(1:m-k+i-1,n-k+i), and tau in TAU(i).
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sgeqlf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sgeqlf(
            MatrixLayout matrixLayout,
            int m,
            int n,
            float* a,
            int lda,
            float* tau);
    }
}
