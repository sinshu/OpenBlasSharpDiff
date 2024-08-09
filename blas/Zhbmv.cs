using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// ZHBMV  performs the matrix-vector  operation
        /// </para>
        /// <para>
        ///    y := alpha*A*x + beta*y,
        /// </para>
        /// <para>
        /// where alpha and beta are scalars, x and y are n element vectors and
        /// A is an n by n hermitian band matrix, with k super-diagonals.
        /// </para>
        /// </summary>
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// On entry, UPLO specifies whether the upper or lower
        /// triangular part of the band matrix A is being supplied as
        /// follows:
        /// 
        /// UPLO = &#39;U&#39; or &#39;u&#39;   The upper triangular part of A is
        /// being supplied.
        /// 
        /// UPLO = &#39;L&#39; or &#39;l&#39;   The lower triangular part of A is
        /// being supplied.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry, N specifies the order of the matrix A.
        /// N must be at least zero.
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// On entry, K specifies the number of super-diagonals of the
        /// matrix A. K must satisfy  0 .le. K.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is COMPLEX*16.
        /// On entry, ALPHA specifies the scalar alpha.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX*16 array, dimension ( LDA, N ).
        /// Before entry with UPLO = &#39;U&#39; or &#39;u&#39;, the leading ( k + 1 )
        /// by n part of the array A must contain the upper triangular
        /// band part of the hermitian matrix, supplied column by
        /// column, with the leading diagonal of the matrix in row
        /// ( k + 1 ) of the array, the first super-diagonal starting at
        /// position 2 in row k, and so on. The top left k by k triangle
        /// of the array A is not referenced.
        /// The following program segment will transfer the upper
        /// triangular part of a hermitian band matrix from conventional
        /// full matrix storage to band storage:
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
        /// band part of the hermitian matrix, supplied column by
        /// column, with the leading diagonal of the matrix in row 1 of
        /// the array, the first sub-diagonal starting at position 1 in
        /// row 2, and so on. The bottom right k by k triangle of the
        /// array A is not referenced.
        /// The following program segment will transfer the lower
        /// triangular part of a hermitian band matrix from conventional
        /// full matrix storage to band storage:
        /// 
        /// DO 20, J = 1, N
        /// M = 1 - J
        /// DO 10, I = J, MIN( N, J + K )
        /// A( M + I, J ) = matrix( I, J )
        /// 10    CONTINUE
        /// 20 CONTINUE
        /// 
        /// Note that the imaginary parts of the diagonal elements need
        /// not be set and are assumed to be zero.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// ( k + 1 ).
        /// </param>
        /// <param name="x">
        /// [in] X is COMPLEX*16 array, dimension at least.
        /// ( 1 + ( n - 1 )*abs( INCX ) ).
        /// Before entry, the incremented array X must contain the
        /// vector x.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// </param>
        /// <param name="beta">
        /// [in] BETA is COMPLEX*16.
        /// On entry, BETA specifies the scalar beta.
        /// </param>
        /// <param name="y">
        /// [in,out] Y is COMPLEX*16 array, dimension at least.
        /// ( 1 + ( n - 1 )*abs( INCY ) ).
        /// Before entry, the incremented array Y must contain the
        /// vector y. On exit, Y is overwritten by the updated vector y.
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// Y. INCY must not be zero.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_zhbmv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Zhbmv(
            Order order,
            Uplo uplo,
            int n,
            int k,
            void* alpha,
            void* a,
            int lda,
            void* x,
            int incx,
            void* beta,
            void* y,
            int incy);
    }
}
