using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// CTRMM  performs one of the matrix-matrix operations
        /// </para>
        /// <para>
        ///    B := alpha*op( A )*B,   or   B := alpha*B*op( A )
        /// </para>
        /// <para>
        /// where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
        /// non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
        /// </para>
        /// <para>
        ///    op( A ) = A   or   op( A ) = A**T   or   op( A ) = A**H.
        /// </para>
        /// </summary>
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// On entry,  SIDE specifies whether  op( A ) multiplies B from
        /// the left or right as follows:
        /// 
        /// SIDE = &#39;L&#39; or &#39;l&#39;   B := alpha*op( A )*B.
        /// 
        /// SIDE = &#39;R&#39; or &#39;r&#39;   B := alpha*B*op( A ).
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// On entry, UPLO specifies whether the matrix A is an upper or
        /// lower triangular matrix as follows:
        /// 
        /// UPLO = &#39;U&#39; or &#39;u&#39;   A is an upper triangular matrix.
        /// 
        /// UPLO = &#39;L&#39; or &#39;l&#39;   A is a lower triangular matrix.
        /// </param>
        /// <param name="transa">
        /// [in] TRANSA is CHARACTER*1.
        /// On entry, TRANSA specifies the form of op( A ) to be used in
        /// the matrix multiplication as follows:
        /// 
        /// TRANSA = &#39;N&#39; or &#39;n&#39;   op( A ) = A.
        /// 
        /// TRANSA = &#39;T&#39; or &#39;t&#39;   op( A ) = A**T.
        /// 
        /// TRANSA = &#39;C&#39; or &#39;c&#39;   op( A ) = A**H.
        /// </param>
        /// <param name="diag">
        /// [in] DIAG is CHARACTER*1.
        /// On entry, DIAG specifies whether or not A is unit triangular
        /// as follows:
        /// 
        /// DIAG = &#39;U&#39; or &#39;u&#39;   A is assumed to be unit triangular.
        /// 
        /// DIAG = &#39;N&#39; or &#39;n&#39;   A is not assumed to be unit
        /// triangular.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// On entry, M specifies the number of rows of B. M must be at
        /// least zero.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry, N specifies the number of columns of B.  N must be
        /// at least zero.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is COMPLEX.
        /// On entry,  ALPHA specifies the scalar  alpha. When  alpha is
        /// zero then  A is not referenced and  B need not be set before
        /// entry.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX array, dimension ( LDA, k ), where k is m.
        /// when  SIDE = &#39;L&#39; or &#39;l&#39;  and is  n  when  SIDE = &#39;R&#39; or &#39;r&#39;.
        /// Before entry  with  UPLO = &#39;U&#39; or &#39;u&#39;,  the  leading  k by k
        /// upper triangular part of the array  A must contain the upper
        /// triangular matrix  and the strictly lower triangular part of
        /// A is not referenced.
        /// Before entry  with  UPLO = &#39;L&#39; or &#39;l&#39;,  the  leading  k by k
        /// lower triangular part of the array  A must contain the lower
        /// triangular matrix  and the strictly upper triangular part of
        /// A is not referenced.
        /// Note that when  DIAG = &#39;U&#39; or &#39;u&#39;,  the diagonal elements of
        /// A  are not referenced either,  but are assumed to be  unity.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program.  When  SIDE = &#39;L&#39; or &#39;l&#39;  then
        /// LDA  must be at least  max( 1, m ),  when  SIDE = &#39;R&#39; or &#39;r&#39;
        /// then LDA must be at least max( 1, n ).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX array, dimension ( LDB, N ).
        /// Before entry,  the leading  m by n part of the array  B must
        /// contain the matrix  B,  and  on exit  is overwritten  by the
        /// transformed matrix.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// On entry, LDB specifies the first dimension of B as declared
        /// in  the  calling  (sub)  program.   LDB  must  be  at  least
        /// max( 1, m ).
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_ctrmm", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Ctrmm(
            Order order,
            Side side,
            Uplo uplo,
            Transpose transa,
            Diag diag,
            int m,
            int n,
            void* alpha,
            void* a,
            int lda,
            void* b,
            int ldb);
    }
}
