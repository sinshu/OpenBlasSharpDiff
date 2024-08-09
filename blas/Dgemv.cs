using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// DGEMV  performs one of the matrix-vector operations
        /// </para>
        /// <para>
        ///    y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
        /// </para>
        /// <para>
        /// where alpha and beta are scalars, x and y are vectors and A is an
        /// m by n matrix.
        /// </para>
        /// </summary>
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// On entry, TRANS specifies the operation to be performed as
        /// follows:
        /// 
        /// TRANS = &#39;N&#39; or &#39;n&#39;   y := alpha*A*x + beta*y.
        /// 
        /// TRANS = &#39;T&#39; or &#39;t&#39;   y := alpha*A**T*x + beta*y.
        /// 
        /// TRANS = &#39;C&#39; or &#39;c&#39;   y := alpha*A**T*x + beta*y.
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
        /// <param name="alpha">
        /// [in] ALPHA is DOUBLE PRECISION.
        /// On entry, ALPHA specifies the scalar alpha.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension ( LDA, N ).
        /// Before entry, the leading m by n part of the array A must
        /// contain the matrix of coefficients.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// max( 1, m ).
        /// </param>
        /// <param name="x">
        /// [in] X is DOUBLE PRECISION array, dimension at least.
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
        /// [in] BETA is DOUBLE PRECISION.
        /// On entry, BETA specifies the scalar beta. When BETA is
        /// supplied as zero then Y need not be set on input.
        /// </param>
        /// <param name="y">
        /// [in,out] Y is DOUBLE PRECISION array, dimension at least.
        /// ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = &#39;N&#39; or &#39;n&#39;
        /// and at least
        /// ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
        /// Before entry with BETA non-zero, the incremented array Y
        /// must contain the vector y. On exit, Y is overwritten by the
        /// updated vector y.
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// Y. INCY must not be zero.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_dgemv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Dgemv(
            Order order,
            Transpose trans,
            int m,
            int n,
            double alpha,
            double* a,
            int lda,
            double* x,
            int incx,
            double beta,
            double* y,
            int incy);
    }
}
