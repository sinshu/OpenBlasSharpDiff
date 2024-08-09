using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGEQP3 computes a QR factorization with column pivoting of a
        /// matrix A:  A*P = Q*R  using Level 3 BLAS.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A. M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit, the upper triangle of the array contains the
        /// min(M,N)-by-N upper trapezoidal matrix R; the elements below
        /// the diagonal, together with the array TAU, represent the
        /// orthogonal matrix Q as a product of min(M,N) elementary
        /// reflectors.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,M).
        /// </param>
        /// <param name="jpvt">
        /// [in,out] JPVT is INTEGER array, dimension (N).
        /// On entry, if JPVT(J).ne.0, the J-th column of A is permuted
        /// to the front of A*P (a leading column); if JPVT(J)=0,
        /// the J-th column of A is a free column.
        /// On exit, if JPVT(J)=K, then the J-th column of A*P was the
        /// the K-th column of A.
        /// </param>
        /// <param name="tau">
        /// [out] TAU is DOUBLE PRECISION array, dimension (min(M,N)).
        /// The scalar factors of the elementary reflectors.
        /// </param>
        /// <returns>
        /// = 0: successful exit.
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value.
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
        ///  where tau is a real scalar, and v is a real/complex vector
        ///  with v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in
        ///  A(i+1:m,i), and tau in TAU(i).
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgeqp3", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgeqp3(
            MatrixLayout matrixLayout,
            int m,
            int n,
            double* a,
            int lda,
            int* jpvt,
            double* tau);
    }
}
