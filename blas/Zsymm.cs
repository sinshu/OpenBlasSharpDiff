﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// ZSYMM  performs one of the matrix-matrix operations
        /// </para>
        /// <para>
        ///    C := alpha*A*B + beta*C,
        /// </para>
        /// <para>
        /// or
        /// </para>
        /// <para>
        ///    C := alpha*B*A + beta*C,
        /// </para>
        /// <para>
        /// where  alpha and beta are scalars, A is a symmetric matrix and  B and
        /// C are m by n matrices.
        /// </para>
        /// </summary>
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// On entry,  SIDE  specifies whether  the  symmetric matrix  A
        /// appears on the  left or right  in the  operation as follows:
        /// 
        /// SIDE = &#39;L&#39; or &#39;l&#39;   C := alpha*A*B + beta*C,
        /// 
        /// SIDE = &#39;R&#39; or &#39;r&#39;   C := alpha*B*A + beta*C,
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// On  entry,   UPLO  specifies  whether  the  upper  or  lower
        /// triangular  part  of  the  symmetric  matrix   A  is  to  be
        /// referenced as follows:
        /// 
        /// UPLO = &#39;U&#39; or &#39;u&#39;   Only the upper triangular part of the
        /// symmetric matrix is to be referenced.
        /// 
        /// UPLO = &#39;L&#39; or &#39;l&#39;   Only the lower triangular part of the
        /// symmetric matrix is to be referenced.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// On entry,  M  specifies the number of rows of the matrix  C.
        /// M  must be at least zero.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry, N specifies the number of columns of the matrix C.
        /// N  must be at least zero.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is COMPLEX*16.
        /// On entry, ALPHA specifies the scalar alpha.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX*16 array, dimension ( LDA, ka ), where ka is.
        /// m  when  SIDE = &#39;L&#39; or &#39;l&#39;  and is n  otherwise.
        /// Before entry  with  SIDE = &#39;L&#39; or &#39;l&#39;,  the  m by m  part of
        /// the array  A  must contain the  symmetric matrix,  such that
        /// when  UPLO = &#39;U&#39; or &#39;u&#39;, the leading m by m upper triangular
        /// part of the array  A  must contain the upper triangular part
        /// of the  symmetric matrix and the  strictly  lower triangular
        /// part of  A  is not referenced,  and when  UPLO = &#39;L&#39; or &#39;l&#39;,
        /// the leading  m by m  lower triangular part  of the  array  A
        /// must  contain  the  lower triangular part  of the  symmetric
        /// matrix and the  strictly upper triangular part of  A  is not
        /// referenced.
        /// Before entry  with  SIDE = &#39;R&#39; or &#39;r&#39;,  the  n by n  part of
        /// the array  A  must contain the  symmetric matrix,  such that
        /// when  UPLO = &#39;U&#39; or &#39;u&#39;, the leading n by n upper triangular
        /// part of the array  A  must contain the upper triangular part
        /// of the  symmetric matrix and the  strictly  lower triangular
        /// part of  A  is not referenced,  and when  UPLO = &#39;L&#39; or &#39;l&#39;,
        /// the leading  n by n  lower triangular part  of the  array  A
        /// must  contain  the  lower triangular part  of the  symmetric
        /// matrix and the  strictly upper triangular part of  A  is not
        /// referenced.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the  calling (sub) program. When  SIDE = &#39;L&#39; or &#39;l&#39;  then
        /// LDA must be at least  max( 1, m ), otherwise  LDA must be at
        /// least max( 1, n ).
        /// </param>
        /// <param name="b">
        /// [in] B is COMPLEX*16 array, dimension ( LDB, N ).
        /// Before entry, the leading  m by n part of the array  B  must
        /// contain the matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// On entry, LDB specifies the first dimension of B as declared
        /// in  the  calling  (sub)  program.   LDB  must  be  at  least
        /// max( 1, m ).
        /// </param>
        /// <param name="beta">
        /// [in] BETA is COMPLEX*16.
        /// On entry,  BETA  specifies the scalar  beta.  When  BETA  is
        /// supplied as zero then C need not be set on input.
        /// </param>
        /// <param name="c">
        /// [in,out] C is COMPLEX*16 array, dimension ( LDC, N ).
        /// Before entry, the leading  m by n  part of the array  C must
        /// contain the matrix  C,  except when  beta  is zero, in which
        /// case C need not be set on entry.
        /// On exit, the array  C  is overwritten by the  m by n updated
        /// matrix.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// On entry, LDC specifies the first dimension of C as declared
        /// in  the  calling  (sub)  program.   LDC  must  be  at  least
        /// max( 1, m ).
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_zsymm", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Zsymm(
            Order order,
            Side side,
            Uplo uplo,
            int m,
            int n,
            void* alpha,
            void* a,
            int lda,
            void* b,
            int ldb,
            void* beta,
            void* c,
            int ldc);
    }
}
