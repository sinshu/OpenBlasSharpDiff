using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// STPMV  performs one of the matrix-vector operations
        /// </para>
        /// <para>
        ///    x := A*x,   or   x := A**T*x,
        /// </para>
        /// <para>
        /// where x is an n element vector and  A is an n by n unit, or non-unit,
        /// upper or lower triangular matrix, supplied in packed form.
        /// </para>
        /// </summary>
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// On entry, UPLO specifies whether the matrix is an upper or
        /// lower triangular matrix as follows:
        /// 
        /// UPLO = &#39;U&#39; or &#39;u&#39;   A is an upper triangular matrix.
        /// 
        /// UPLO = &#39;L&#39; or &#39;l&#39;   A is a lower triangular matrix.
        /// </param>
        /// <param name="transa">
        /// No description available.
        /// </param>
        /// <param name="diag">
        /// [in] DIAG is CHARACTER*1.
        /// On entry, DIAG specifies whether or not A is unit
        /// triangular as follows:
        /// 
        /// DIAG = &#39;U&#39; or &#39;u&#39;   A is assumed to be unit triangular.
        /// 
        /// DIAG = &#39;N&#39; or &#39;n&#39;   A is not assumed to be unit
        /// triangular.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry, N specifies the order of the matrix A.
        /// N must be at least zero.
        /// </param>
        /// <param name="ap">
        /// [in] AP is REAL array, dimension at least.
        /// ( ( n*( n + 1 ) )/2 ).
        /// Before entry with  UPLO = &#39;U&#39; or &#39;u&#39;, the array AP must
        /// contain the upper triangular matrix packed sequentially,
        /// column by column, so that AP( 1 ) contains a( 1, 1 ),
        /// AP( 2 ) and AP( 3 ) contain a( 1, 2 ) and a( 2, 2 )
        /// respectively, and so on.
        /// Before entry with UPLO = &#39;L&#39; or &#39;l&#39;, the array AP must
        /// contain the lower triangular matrix packed sequentially,
        /// column by column, so that AP( 1 ) contains a( 1, 1 ),
        /// AP( 2 ) and AP( 3 ) contain a( 2, 1 ) and a( 3, 1 )
        /// respectively, and so on.
        /// Note that when  DIAG = &#39;U&#39; or &#39;u&#39;, the diagonal elements of
        /// A are not referenced, but are assumed to be unity.
        /// </param>
        /// <param name="x">
        /// [in,out] X is REAL array, dimension at least.
        /// ( 1 + ( n - 1 )*abs( INCX ) ).
        /// Before entry, the incremented array X must contain the n
        /// element vector x. On exit, X is overwritten with the
        /// transformed vector x.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_stpmv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Stpmv(
            Order order,
            Uplo uplo,
            Transpose transa,
            Diag diag,
            int n,
            float* ap,
            float* x,
            int incx);
    }
}
