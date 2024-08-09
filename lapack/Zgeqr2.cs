using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGEQR2 computes a QR factorization of a complex m-by-n matrix A:
        /// </para>
        /// <para>
        ///    A = Q * ( R ),
        ///            ( 0 )
        /// </para>
        /// <para>
        /// where:
        /// </para>
        /// <para>
        ///    Q is a m-by-m orthogonal matrix;
        ///    R is an upper-triangular n-by-n matrix;
        ///    0 is a (m-n)-by-n zero matrix, if m &gt; n.
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
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the m by n matrix A.
        /// On exit, the elements on and above the diagonal of the array
        /// contain the min(m,n) by n upper trapezoidal matrix R (R is
        /// upper triangular if m &gt;= n); the elements below the diagonal,
        /// with the array TAU, represent the unitary matrix Q as a
        /// product of elementary reflectors (see Further Details).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="tau">
        /// [out] TAU is COMPLEX*16 array, dimension (min(M,N)).
        /// The scalar factors of the elementary reflectors (see Further
        /// Details).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
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
        ///     H(i) = I - tau * v * v**H
        /// </para>
        /// <para>
        ///  where tau is a complex scalar, and v is a complex vector with
        ///  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
        ///  and tau in TAU(i).
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgeqr2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgeqr2(
            MatrixLayout matrixLayout,
            int m,
            int n,
            Complex* a,
            int lda,
            Complex* tau);
    }
}
