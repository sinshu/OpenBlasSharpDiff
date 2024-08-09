using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// SGBMV  performs one of the matrix-vector operations
        /// </para>
        /// <para>
        ///    y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
        /// </para>
        /// <para>
        /// where alpha and beta are scalars, x and y are vectors and A is an
        /// m by n band matrix, with kl sub-diagonals and ku super-diagonals.
        /// </para>
        /// </summary>
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="transa">
        /// No description available.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// On entry, M specifies the number of rows of the matrix A.
        /// M must be at least zero.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry, N specifies the number of columns of the matrix A.
        /// N must be at least zero.
        /// </param>
        /// <param name="kl">
        /// [in] KL is INTEGER.
        /// On entry, KL specifies the number of sub-diagonals of the
        /// matrix A. KL must satisfy  0 .le. KL.
        /// </param>
        /// <param name="ku">
        /// [in] KU is INTEGER.
        /// On entry, KU specifies the number of super-diagonals of the
        /// matrix A. KU must satisfy  0 .le. KU.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is REAL.
        /// On entry, ALPHA specifies the scalar alpha.
        /// </param>
        /// <param name="a">
        /// [in] A is REAL array, dimension ( LDA, N ).
        /// Before entry, the leading ( kl + ku + 1 ) by n part of the
        /// array A must contain the matrix of coefficients, supplied
        /// column by column, with the leading diagonal of the matrix in
        /// row ( ku + 1 ) of the array, the first super-diagonal
        /// starting at position 2 in row ku, the first sub-diagonal
        /// starting at position 1 in row ( ku + 2 ), and so on.
        /// Elements in the array A that do not correspond to elements
        /// in the band matrix (such as the top left ku by ku triangle)
        /// are not referenced.
        /// The following program segment will transfer a band matrix
        /// from conventional full matrix storage to band storage:
        /// 
        /// DO 20, J = 1, N
        /// K = KU + 1 - J
        /// DO 10, I = MAX( 1, J - KU ), MIN( M, J + KL )
        /// A( K + I, J ) = matrix( I, J )
        /// 10    CONTINUE
        /// 20 CONTINUE
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// ( kl + ku + 1 ).
        /// </param>
        /// <param name="x">
        /// [in] X is REAL array, dimension at least.
        /// ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = &#39;N&#39; or &#39;n&#39;
        /// and at least
        /// ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
        /// Before entry, the incremented array X must contain the
        /// vector x.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// </param>
        /// <param name="beta">
        /// [in] BETA is REAL.
        /// On entry, BETA specifies the scalar beta. When BETA is
        /// supplied as zero then Y need not be set on input.
        /// </param>
        /// <param name="y">
        /// [in,out] Y is REAL array, dimension at least.
        /// ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = &#39;N&#39; or &#39;n&#39;
        /// and at least
        /// ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
        /// Before entry, the incremented array Y must contain the
        /// vector y. On exit, Y is overwritten by the updated vector y.
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// Y. INCY must not be zero.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_sgbmv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Sgbmv(
            Order order,
            Transpose transa,
            int m,
            int n,
            int kl,
            int ku,
            float alpha,
            float* a,
            int lda,
            float* x,
            int incx,
            float beta,
            float* y,
            int incy);
    }
}
