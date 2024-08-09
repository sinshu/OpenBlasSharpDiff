using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// ZHER   performs the hermitian rank 1 operation
        /// </para>
        /// <para>
        ///    A := alpha*x*x**H + A,
        /// </para>
        /// <para>
        /// where alpha is a real scalar, x is an n element vector and A is an
        /// n by n hermitian matrix.
        /// </para>
        /// </summary>
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// On entry, UPLO specifies whether the upper or lower
        /// triangular part of the array A is to be referenced as
        /// follows:
        /// 
        /// UPLO = &#39;U&#39; or &#39;u&#39;   Only the upper triangular part of A
        /// is to be referenced.
        /// 
        /// UPLO = &#39;L&#39; or &#39;l&#39;   Only the lower triangular part of A
        /// is to be referenced.
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
        /// <param name="x">
        /// [in] X is COMPLEX*16 array, dimension at least.
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
        /// [in,out] A is COMPLEX*16 array, dimension ( LDA, N ).
        /// Before entry with  UPLO = &#39;U&#39; or &#39;u&#39;, the leading n by n
        /// upper triangular part of the array A must contain the upper
        /// triangular part of the hermitian matrix and the strictly
        /// lower triangular part of A is not referenced. On exit, the
        /// upper triangular part of the array A is overwritten by the
        /// upper triangular part of the updated matrix.
        /// Before entry with UPLO = &#39;L&#39; or &#39;l&#39;, the leading n by n
        /// lower triangular part of the array A must contain the lower
        /// triangular part of the hermitian matrix and the strictly
        /// upper triangular part of A is not referenced. On exit, the
        /// lower triangular part of the array A is overwritten by the
        /// lower triangular part of the updated matrix.
        /// Note that the imaginary parts of the diagonal elements need
        /// not be set, they are assumed to be zero, and on exit they
        /// are set to zero.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// max( 1, n ).
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_zher", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Zher(
            Order order,
            Uplo uplo,
            int n,
            double alpha,
            void* x,
            int incx,
            void* a,
            int lda);
    }
}
