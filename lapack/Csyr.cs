using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CSYR   performs the symmetric rank 1 operation
        /// </para>
        /// <para>
        ///    A := alpha*x*x**H + A,
        /// </para>
        /// <para>
        /// where alpha is a complex scalar, x is an n element vector and A is an
        /// n by n symmetric matrix.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
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
        /// 
        /// Unchanged on exit.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry, N specifies the order of the matrix A.
        /// N must be at least zero.
        /// Unchanged on exit.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is COMPLEX.
        /// On entry, ALPHA specifies the scalar alpha.
        /// Unchanged on exit.
        /// </param>
        /// <param name="x">
        /// [in] X is COMPLEX array, dimension at least.
        /// ( 1 + ( N - 1 )*abs( INCX ) ).
        /// Before entry, the incremented array X must contain the N-
        /// element vector x.
        /// Unchanged on exit.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// Unchanged on exit.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension ( LDA, N ).
        /// Before entry, with  UPLO = &#39;U&#39; or &#39;u&#39;, the leading n by n
        /// upper triangular part of the array A must contain the upper
        /// triangular part of the symmetric matrix and the strictly
        /// lower triangular part of A is not referenced. On exit, the
        /// upper triangular part of the array A is overwritten by the
        /// upper triangular part of the updated matrix.
        /// Before entry, with UPLO = &#39;L&#39; or &#39;l&#39;, the leading n by n
        /// lower triangular part of the array A must contain the lower
        /// triangular part of the symmetric matrix and the strictly
        /// upper triangular part of A is not referenced. On exit, the
        /// lower triangular part of the array A is overwritten by the
        /// lower triangular part of the updated matrix.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// max( 1, N ).
        /// Unchanged on exit.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_csyr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Csyr(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex32 alpha,
            Complex32* x,
            int incx,
            Complex32* a,
            int lda);
    }
}
