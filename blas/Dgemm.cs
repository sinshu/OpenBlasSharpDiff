using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// DGEMM  performs one of the matrix-matrix operations
        /// </para>
        /// <para>
        ///    C := alpha*op( A )*op( B ) + beta*C,
        /// </para>
        /// <para>
        /// where  op( X ) is one of
        /// </para>
        /// <para>
        ///    op( X ) = X   or   op( X ) = X**T,
        /// </para>
        /// <para>
        /// alpha and beta are scalars, and A, B and C are matrices, with op( A )
        /// an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
        /// </para>
        /// </summary>
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="transa">
        /// [in] TRANSA is CHARACTER*1.
        /// On entry, TRANSA specifies the form of op( A ) to be used in
        /// the matrix multiplication as follows:
        /// 
        /// TRANSA = &#39;N&#39; or &#39;n&#39;,  op( A ) = A.
        /// 
        /// TRANSA = &#39;T&#39; or &#39;t&#39;,  op( A ) = A**T.
        /// 
        /// TRANSA = &#39;C&#39; or &#39;c&#39;,  op( A ) = A**T.
        /// </param>
        /// <param name="transb">
        /// [in] TRANSB is CHARACTER*1.
        /// On entry, TRANSB specifies the form of op( B ) to be used in
        /// the matrix multiplication as follows:
        /// 
        /// TRANSB = &#39;N&#39; or &#39;n&#39;,  op( B ) = B.
        /// 
        /// TRANSB = &#39;T&#39; or &#39;t&#39;,  op( B ) = B**T.
        /// 
        /// TRANSB = &#39;C&#39; or &#39;c&#39;,  op( B ) = B**T.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// On entry,  M  specifies  the number  of rows  of the  matrix
        /// op( A )  and of the  matrix  C.  M  must  be at least  zero.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry,  N  specifies the number  of columns of the matrix
        /// op( B ) and the number of columns of the matrix C. N must be
        /// at least zero.
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// On entry,  K  specifies  the number of columns of the matrix
        /// op( A ) and the number of rows of the matrix op( B ). K must
        /// be at least  zero.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is DOUBLE PRECISION.
        /// On entry, ALPHA specifies the scalar alpha.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension ( LDA, ka ), where ka is.
        /// k  when  TRANSA = &#39;N&#39; or &#39;n&#39;,  and is  m  otherwise.
        /// Before entry with  TRANSA = &#39;N&#39; or &#39;n&#39;,  the leading  m by k
        /// part of the array  A  must contain the matrix  A,  otherwise
        /// the leading  k by m  part of the array  A  must contain  the
        /// matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. When  TRANSA = &#39;N&#39; or &#39;n&#39; then
        /// LDA must be at least  max( 1, m ), otherwise  LDA must be at
        /// least  max( 1, k ).
        /// </param>
        /// <param name="b">
        /// [in] B is DOUBLE PRECISION array, dimension ( LDB, kb ), where kb is.
        /// n  when  TRANSB = &#39;N&#39; or &#39;n&#39;,  and is  k  otherwise.
        /// Before entry with  TRANSB = &#39;N&#39; or &#39;n&#39;,  the leading  k by n
        /// part of the array  B  must contain the matrix  B,  otherwise
        /// the leading  n by k  part of the array  B  must contain  the
        /// matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// On entry, LDB specifies the first dimension of B as declared
        /// in the calling (sub) program. When  TRANSB = &#39;N&#39; or &#39;n&#39; then
        /// LDB must be at least  max( 1, k ), otherwise  LDB must be at
        /// least  max( 1, n ).
        /// </param>
        /// <param name="beta">
        /// [in] BETA is DOUBLE PRECISION.
        /// On entry,  BETA  specifies the scalar  beta.  When  BETA  is
        /// supplied as zero then C need not be set on input.
        /// </param>
        /// <param name="c">
        /// [in,out] C is DOUBLE PRECISION array, dimension ( LDC, N ).
        /// Before entry, the leading  m by n  part of the array  C must
        /// contain the matrix  C,  except when  beta  is zero, in which
        /// case C need not be set on entry.
        /// On exit, the array  C  is overwritten by the  m by n  matrix
        /// ( alpha*op( A )*op( B ) + beta*C ).
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// On entry, LDC specifies the first dimension of C as declared
        /// in  the  calling  (sub)  program.   LDC  must  be  at  least
        /// max( 1, m ).
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_dgemm", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Dgemm(
            Order order,
            Transpose transa,
            Transpose transb,
            int m,
            int n,
            int k,
            double alpha,
            double* a,
            int lda,
            double* b,
            int ldb,
            double beta,
            double* c,
            int ldc);
    }
}
