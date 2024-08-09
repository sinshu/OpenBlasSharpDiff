using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CTPQRT2 computes a QR factorization of a complex &quot;triangular-pentagonal&quot;
        /// matrix C, which is composed of a triangular block A and pentagonal block B,
        /// using the compact WY representation for Q.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The total number of rows of the matrix B.
        /// M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix B, and the order of
        /// the triangular matrix A.
        /// N &gt;= 0.
        /// </param>
        /// <param name="l">
        /// [in] L is INTEGER.
        /// The number of rows of the upper trapezoidal part of B.
        /// MIN(M,N) &gt;= L &gt;= 0.  See Further Details.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the upper triangular N-by-N matrix A.
        /// On exit, the elements on and above the diagonal of the array
        /// contain the upper triangular matrix R.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX array, dimension (LDB,N).
        /// On entry, the pentagonal M-by-N matrix B.  The first M-L rows
        /// are rectangular, and the last L rows are upper trapezoidal.
        /// On exit, B contains the pentagonal matrix V.  See Further Details.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,M).
        /// </param>
        /// <param name="t">
        /// [out] T is COMPLEX array, dimension (LDT,N).
        /// The N-by-N upper triangular factor T of the block reflector.
        /// See Further Details.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T.  LDT &gt;= max(1,N)
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The input matrix C is a (N+M)-by-N matrix
        /// </para>
        /// <para>
        ///               C = [ A ]
        ///                   [ B ]
        /// </para>
        /// <para>
        ///  where A is an upper triangular N-by-N matrix, and B is M-by-N pentagonal
        ///  matrix consisting of a (M-L)-by-N rectangular matrix B1 on top of a L-by-N
        ///  upper trapezoidal matrix B2:
        /// </para>
        /// <para>
        ///               B = [ B1 ]  &lt;- (M-L)-by-N rectangular
        ///                   [ B2 ]  &lt;-     L-by-N upper trapezoidal.
        /// </para>
        /// <para>
        ///  The upper trapezoidal matrix B2 consists of the first L rows of a
        ///  N-by-N upper triangular matrix, where 0 &lt;= L &lt;= MIN(M,N).  If L=0,
        ///  B is rectangular M-by-N; if M=L=N, B is upper triangular.
        /// </para>
        /// <para>
        ///  The matrix W stores the elementary reflectors H(i) in the i-th column
        ///  below the diagonal (of A) in the (N+M)-by-N input matrix C
        /// </para>
        /// <para>
        ///               C = [ A ]  &lt;- upper triangular N-by-N
        ///                   [ B ]  &lt;- M-by-N pentagonal
        /// </para>
        /// <para>
        ///  so that W can be represented as
        /// </para>
        /// <para>
        ///               W = [ I ]  &lt;- identity, N-by-N
        ///                   [ V ]  &lt;- M-by-N, same form as B.
        /// </para>
        /// <para>
        ///  Thus, all of information needed for W is contained on exit in B, which
        ///  we call V above.  Note that V has the same form as B; that is,
        /// </para>
        /// <para>
        ///               V = [ V1 ] &lt;- (M-L)-by-N rectangular
        ///                   [ V2 ] &lt;-     L-by-N upper trapezoidal.
        /// </para>
        /// <para>
        ///  The columns of V represent the vectors which define the H(i)&#39;s.
        ///  The (M+N)-by-(M+N) block reflector H is then given by
        /// </para>
        /// <para>
        ///               H = I - W * T * W**H
        /// </para>
        /// <para>
        ///  where W**H is the conjugate transpose of W and T is the upper triangular
        ///  factor of the block reflector.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ctpqrt2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ctpqrt2(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int l,
            Complex32* a,
            int lda,
            Complex32* b,
            int ldb,
            Complex32* t,
            int ldt);
    }
}
