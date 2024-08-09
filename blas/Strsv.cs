using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// STRSV  solves one of the systems of equations
        /// </para>
        /// <para>
        ///    A*x = b,   or   A**T*x = b,
        /// </para>
        /// <para>
        /// where b and x are n element vectors and A is an n by n unit, or
        /// non-unit, upper or lower triangular matrix.
        /// </para>
        /// <para>
        /// No test for singularity or near-singularity is included in this
        /// routine. Such tests must be performed before calling this routine.
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
        /// <param name="a">
        /// [in] A is REAL array, dimension ( LDA, N ).
        /// Before entry with  UPLO = &#39;U&#39; or &#39;u&#39;, the leading n by n
        /// upper triangular part of the array A must contain the upper
        /// triangular matrix and the strictly lower triangular part of
        /// A is not referenced.
        /// Before entry with UPLO = &#39;L&#39; or &#39;l&#39;, the leading n by n
        /// lower triangular part of the array A must contain the lower
        /// triangular matrix and the strictly upper triangular part of
        /// A is not referenced.
        /// Note that when  DIAG = &#39;U&#39; or &#39;u&#39;, the diagonal elements of
        /// A are not referenced either, but are assumed to be unity.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// max( 1, n ).
        /// </param>
        /// <param name="x">
        /// [in,out] X is REAL array, dimension at least.
        /// ( 1 + ( n - 1 )*abs( INCX ) ).
        /// Before entry, the incremented array X must contain the n
        /// element right-hand side vector b. On exit, X is overwritten
        /// with the solution vector x.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_strsv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Strsv(
            Order order,
            Uplo uplo,
            Transpose transa,
            Diag diag,
            int n,
            float* a,
            int lda,
            float* x,
            int incx);
    }
}
