using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// DSPMV  performs the matrix-vector operation
        /// </para>
        /// <para>
        ///    y := alpha*A*x + beta*y,
        /// </para>
        /// <para>
        /// where alpha and beta are scalars, x and y are n element vectors and
        /// A is an n by n symmetric matrix, supplied in packed form.
        /// </para>
        /// </summary>
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// On entry, UPLO specifies whether the upper or lower
        /// triangular part of the matrix A is supplied in the packed
        /// array AP as follows:
        /// 
        /// UPLO = &#39;U&#39; or &#39;u&#39;   The upper triangular part of A is
        /// supplied in AP.
        /// 
        /// UPLO = &#39;L&#39; or &#39;l&#39;   The lower triangular part of A is
        /// supplied in AP.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry, N specifies the order of the matrix A.
        /// N must be at least zero.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is DOUBLE PRECISION.
        /// On entry, ALPHA specifies the scalar alpha.
        /// </param>
        /// <param name="ap">
        /// [in] AP is DOUBLE PRECISION array, dimension at least.
        /// ( ( n*( n + 1 ) )/2 ).
        /// Before entry with UPLO = &#39;U&#39; or &#39;u&#39;, the array AP must
        /// contain the upper triangular part of the symmetric matrix
        /// packed sequentially, column by column, so that AP( 1 )
        /// contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
        /// and a( 2, 2 ) respectively, and so on.
        /// Before entry with UPLO = &#39;L&#39; or &#39;l&#39;, the array AP must
        /// contain the lower triangular part of the symmetric matrix
        /// packed sequentially, column by column, so that AP( 1 )
        /// contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
        /// and a( 3, 1 ) respectively, and so on.
        /// </param>
        /// <param name="x">
        /// [in] X is DOUBLE PRECISION array, dimension at least.
        /// ( 1 + ( n - 1 )*abs( INCX ) ).
        /// Before entry, the incremented array X must contain the n
        /// element vector x.
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
        /// ( 1 + ( n - 1 )*abs( INCY ) ).
        /// Before entry, the incremented array Y must contain the n
        /// element vector y. On exit, Y is overwritten by the updated
        /// vector y.
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// Y. INCY must not be zero.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_dspmv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Dspmv(
            Order order,
            Uplo uplo,
            int n,
            double alpha,
            double* ap,
            double* x,
            int incx,
            double beta,
            double* y,
            int incy);
    }
}
