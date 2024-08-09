using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// ZTBMV  performs one of the matrix-vector operations
        /// </para>
        /// <para>
        ///    x := A*x,   or   x := A**T*x,   or   x := A**H*x,
        /// </para>
        /// <para>
        /// where x is an n element vector and  A is an n by n unit, or non-unit,
        /// upper or lower triangular band matrix, with ( k + 1 ) diagonals.
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
        /// <param name="k">
        /// [in] K is INTEGER.
        /// On entry with UPLO = &#39;U&#39; or &#39;u&#39;, K specifies the number of
        /// super-diagonals of the matrix A.
        /// On entry with UPLO = &#39;L&#39; or &#39;l&#39;, K specifies the number of
        /// sub-diagonals of the matrix A.
        /// K must satisfy  0 .le. K.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX*16 array, dimension ( LDA, N ).
        /// Before entry with UPLO = &#39;U&#39; or &#39;u&#39;, the leading ( k + 1 )
        /// by n part of the array A must contain the upper triangular
        /// band part of the matrix of coefficients, supplied column by
        /// column, with the leading diagonal of the matrix in row
        /// ( k + 1 ) of the array, the first super-diagonal starting at
        /// position 2 in row k, and so on. The top left k by k triangle
        /// of the array A is not referenced.
        /// The following program segment will transfer an upper
        /// triangular band matrix from conventional full matrix storage
        /// to band storage:
        /// 
        /// DO 20, J = 1, N
        /// M = K + 1 - J
        /// DO 10, I = MAX( 1, J - K ), J
        /// A( M + I, J ) = matrix( I, J )
        /// 10    CONTINUE
        /// 20 CONTINUE
        /// 
        /// Before entry with UPLO = &#39;L&#39; or &#39;l&#39;, the leading ( k + 1 )
        /// by n part of the array A must contain the lower triangular
        /// band part of the matrix of coefficients, supplied column by
        /// column, with the leading diagonal of the matrix in row 1 of
        /// the array, the first sub-diagonal starting at position 1 in
        /// row 2, and so on. The bottom right k by k triangle of the
        /// array A is not referenced.
        /// The following program segment will transfer a lower
        /// triangular band matrix from conventional full matrix storage
        /// to band storage:
        /// 
        /// DO 20, J = 1, N
        /// M = 1 - J
        /// DO 10, I = J, MIN( N, J + K )
        /// A( M + I, J ) = matrix( I, J )
        /// 10    CONTINUE
        /// 20 CONTINUE
        /// 
        /// Note that when DIAG = &#39;U&#39; or &#39;u&#39; the elements of the array A
        /// corresponding to the diagonal elements of the matrix are not
        /// referenced, but are assumed to be unity.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// ( k + 1 ).
        /// </param>
        /// <param name="x">
        /// [in,out] X is COMPLEX*16 array, dimension at least.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_ztbmv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Ztbmv(
            Order order,
            Uplo uplo,
            Transpose transa,
            Diag diag,
            int n,
            int k,
            void* a,
            int lda,
            void* x,
            int incx);
    }
}
