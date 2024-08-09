using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// CHPR    performs the hermitian rank 1 operation
        /// </para>
        /// <para>
        ///    A := alpha*x*x**H + A,
        /// </para>
        /// <para>
        /// where alpha is a real scalar, x is an n element vector and A is an
        /// n by n hermitian matrix, supplied in packed form.
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
        /// [in] X is COMPLEX array, dimension at least.
        /// ( 1 + ( n - 1 )*abs( INCX ) ).
        /// Before entry, the incremented array X must contain the n
        /// element vector x.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// </param>
        /// <param name="a">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_chpr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Chpr(
            Order order,
            Uplo uplo,
            int n,
            float alpha,
            void* x,
            int incx,
            void* a);
    }
}
