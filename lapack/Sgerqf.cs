using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SGERQF computes an RQ factorization of a real M-by-N matrix A:
        /// A = R * Q.
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
        /// if m &lt;= n, the upper triangle of the subarray
        /// A(1:m,n-m+1:n) contains the M-by-M upper triangular matrix R;
        /// if m &gt;= n, the elements on and above the (m-n)-th subdiagonal
        /// contain the M-by-N upper trapezoidal matrix R;
        /// the remaining elements, with the array TAU, represent the
        /// orthogonal matrix Q as a product of min(m,n) elementary
        /// reflectors (see Further Details).
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
        ///     Q = H(1) H(2) . . . H(k), where k = min(m,n).
        /// </para>
        /// <para>
        ///  Each H(i) has the form
        /// </para>
        /// <para>
        ///     H(i) = I - tau * v * v**T
        /// </para>
        /// <para>
        ///  where tau is a real scalar, and v is a real vector with
        ///  v(n-k+i+1:n) = 0 and v(n-k+i) = 1; v(1:n-k+i-1) is stored on exit in
        ///  A(m-k+i,1:n-k+i-1), and tau in TAU(i).
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sgerqf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sgerqf(
            MatrixLayout matrixLayout,
            int m,
            int n,
            float* a,
            int lda,
            float* tau);
    }
}
