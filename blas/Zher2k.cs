using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// ZHER2K  performs one of the hermitian rank 2k operations
        /// </para>
        /// <para>
        ///    C := alpha*A*B**H + conjg( alpha )*B*A**H + beta*C,
        /// </para>
        /// <para>
        /// or
        /// </para>
        /// <para>
        ///    C := alpha*A**H*B + conjg( alpha )*B**H*A + beta*C,
        /// </para>
        /// <para>
        /// where  alpha and beta  are scalars with  beta  real,  C is an  n by n
        /// hermitian matrix and  A and B  are  n by k matrices in the first case
        /// and  k by n  matrices in the second case.
        /// </para>
        /// </summary>
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// On  entry,   UPLO  specifies  whether  the  upper  or  lower
        /// triangular  part  of the  array  C  is to be  referenced  as
        /// follows:
        /// 
        /// UPLO = &#39;U&#39; or &#39;u&#39;   Only the  upper triangular part of  C
        /// is to be referenced.
        /// 
        /// UPLO = &#39;L&#39; or &#39;l&#39;   Only the  lower triangular part of  C
        /// is to be referenced.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// On entry,  TRANS  specifies the operation to be performed as
        /// follows:
        /// 
        /// TRANS = &#39;N&#39; or &#39;n&#39;    C := alpha*A*B**H          +
        /// conjg( alpha )*B*A**H +
        /// beta*C.
        /// 
        /// TRANS = &#39;C&#39; or &#39;c&#39;    C := alpha*A**H*B          +
        /// conjg( alpha )*B**H*A +
        /// beta*C.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry,  N specifies the order of the matrix C.  N must be
        /// at least zero.
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// On entry with  TRANS = &#39;N&#39; or &#39;n&#39;,  K  specifies  the number
        /// of  columns  of the  matrices  A and B,  and on  entry  with
        /// TRANS = &#39;C&#39; or &#39;c&#39;,  K  specifies  the number of rows of the
        /// matrices  A and B.  K must be at least zero.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is COMPLEX*16 .
        /// On entry, ALPHA specifies the scalar alpha.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX*16 array, dimension ( LDA, ka ), where ka is.
        /// k  when  TRANS = &#39;N&#39; or &#39;n&#39;,  and is  n  otherwise.
        /// Before entry with  TRANS = &#39;N&#39; or &#39;n&#39;,  the  leading  n by k
        /// part of the array  A  must contain the matrix  A,  otherwise
        /// the leading  k by n  part of the array  A  must contain  the
        /// matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in  the  calling  (sub)  program.   When  TRANS = &#39;N&#39; or &#39;n&#39;
        /// then  LDA must be at least  max( 1, n ), otherwise  LDA must
        /// be at least  max( 1, k ).
        /// </param>
        /// <param name="b">
        /// [in] B is COMPLEX*16 array, dimension ( LDB, kb ), where kb is.
        /// k  when  TRANS = &#39;N&#39; or &#39;n&#39;,  and is  n  otherwise.
        /// Before entry with  TRANS = &#39;N&#39; or &#39;n&#39;,  the  leading  n by k
        /// part of the array  B  must contain the matrix  B,  otherwise
        /// the leading  k by n  part of the array  B  must contain  the
        /// matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// On entry, LDB specifies the first dimension of B as declared
        /// in  the  calling  (sub)  program.   When  TRANS = &#39;N&#39; or &#39;n&#39;
        /// then  LDB must be at least  max( 1, n ), otherwise  LDB must
        /// be at least  max( 1, k ).
        /// Unchanged on exit.
        /// </param>
        /// <param name="beta">
        /// [in] BETA is DOUBLE PRECISION .
        /// On entry, BETA specifies the scalar beta.
        /// </param>
        /// <param name="c">
        /// [in,out] C is COMPLEX*16 array, dimension ( LDC, N ).
        /// Before entry  with  UPLO = &#39;U&#39; or &#39;u&#39;,  the leading  n by n
        /// upper triangular part of the array C must contain the upper
        /// triangular part  of the  hermitian matrix  and the strictly
        /// lower triangular part of C is not referenced.  On exit, the
        /// upper triangular part of the array  C is overwritten by the
        /// upper triangular part of the updated matrix.
        /// Before entry  with  UPLO = &#39;L&#39; or &#39;l&#39;,  the leading  n by n
        /// lower triangular part of the array C must contain the lower
        /// triangular part  of the  hermitian matrix  and the strictly
        /// upper triangular part of C is not referenced.  On exit, the
        /// lower triangular part of the array  C is overwritten by the
        /// lower triangular part of the updated matrix.
        /// Note that the imaginary parts of the diagonal elements need
        /// not be set,  they are assumed to be zero,  and on exit they
        /// are set to zero.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// On entry, LDC specifies the first dimension of C as declared
        /// in  the  calling  (sub)  program.   LDC  must  be  at  least
        /// max( 1, n ).
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_zher2k", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Zher2k(
            Order order,
            Uplo uplo,
            Transpose trans,
            int n,
            int k,
            void* alpha,
            void* a,
            int lda,
            void* b,
            int ldb,
            double beta,
            void* c,
            int ldc);
    }
}
