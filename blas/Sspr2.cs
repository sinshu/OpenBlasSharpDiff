using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// SSPR2  performs the symmetric rank 2 operation
        /// </para>
        /// <para>
        ///    A := alpha*x*y**T + alpha*y*x**T + A,
        /// </para>
        /// <para>
        /// where alpha is a scalar, x and y are n element vectors and A is an
        /// n by n symmetric matrix, supplied in packed form.
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
        /// [in] ALPHA is REAL.
        /// On entry, ALPHA specifies the scalar alpha.
        /// </param>
        /// <param name="x">
        /// [in] X is REAL array, dimension at least.
        /// ( 1 + ( n - 1 )*abs( INCX ) ).
        /// Before entry, the incremented array X must contain the n
        /// element vector x.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// </param>
        /// <param name="y">
        /// [in] Y is REAL array, dimension at least.
        /// ( 1 + ( n - 1 )*abs( INCY ) ).
        /// Before entry, the incremented array Y must contain the n
        /// element vector y.
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// Y. INCY must not be zero.
        /// </param>
        /// <param name="a">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_sspr2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Sspr2(
            Order order,
            Uplo uplo,
            int n,
            float alpha,
            float* x,
            int incx,
            float* y,
            int incy,
            float* a);
    }
}
