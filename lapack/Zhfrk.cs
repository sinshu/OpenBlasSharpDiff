﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// Level 3 BLAS like routine for C in RFP Format.
        /// </para>
        /// <para>
        /// ZHFRK performs one of the Hermitian rank--k operations
        /// </para>
        /// <para>
        ///    C := alpha*A*A**H + beta*C,
        /// </para>
        /// <para>
        /// or
        /// </para>
        /// <para>
        ///    C := alpha*A**H*A + beta*C,
        /// </para>
        /// <para>
        /// where alpha and beta are real scalars, C is an n--by--n Hermitian
        /// matrix and A is an n--by--k matrix in the first case and a k--by--n
        /// matrix in the second case.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="transr">
        /// [in] TRANSR is CHARACTER*1.
        /// = &#39;N&#39;:  The Normal Form of RFP A is stored;
        /// = &#39;C&#39;:  The Conjugate-transpose Form of RFP A is stored.
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
        /// 
        /// Unchanged on exit.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// On entry,  TRANS  specifies the operation to be performed as
        /// follows:
        /// 
        /// TRANS = &#39;N&#39; or &#39;n&#39;   C := alpha*A*A**H + beta*C.
        /// 
        /// TRANS = &#39;C&#39; or &#39;c&#39;   C := alpha*A**H*A + beta*C.
        /// 
        /// Unchanged on exit.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry,  N specifies the order of the matrix C.  N must be
        /// at least zero.
        /// Unchanged on exit.
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// On entry with  TRANS = &#39;N&#39; or &#39;n&#39;,  K  specifies  the number
        /// of  columns   of  the   matrix   A,   and  on   entry   with
        /// TRANS = &#39;C&#39; or &#39;c&#39;,  K  specifies  the number of rows of the
        /// matrix A.  K must be at least zero.
        /// Unchanged on exit.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is DOUBLE PRECISION.
        /// On entry, ALPHA specifies the scalar alpha.
        /// Unchanged on exit.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX*16 array, dimension (LDA,ka).
        /// where KA
        /// is K  when TRANS = &#39;N&#39; or &#39;n&#39;, and is N otherwise. Before
        /// entry with TRANS = &#39;N&#39; or &#39;n&#39;, the leading N--by--K part of
        /// the array A must contain the matrix A, otherwise the leading
        /// K--by--N part of the array A must contain the matrix A.
        /// Unchanged on exit.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in  the  calling  (sub)  program.   When  TRANS = &#39;N&#39; or &#39;n&#39;
        /// then  LDA must be at least  max( 1, n ), otherwise  LDA must
        /// be at least  max( 1, k ).
        /// Unchanged on exit.
        /// </param>
        /// <param name="beta">
        /// [in] BETA is DOUBLE PRECISION.
        /// On entry, BETA specifies the scalar beta.
        /// Unchanged on exit.
        /// </param>
        /// <param name="c">
        /// [in,out] C is COMPLEX*16 array, dimension (N*(N+1)/2).
        /// On entry, the matrix A in RFP Format. RFP Format is
        /// described by TRANSR, UPLO and N. Note that the imaginary
        /// parts of the diagonal elements need not be set, they are
        /// assumed to be zero, and on exit they are set to zero.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zhfrk", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zhfrk(
            MatrixLayout matrixLayout,
            char transr,
            char uplo,
            char trans,
            int n,
            int k,
            double alpha,
            Complex* a,
            int lda,
            double beta,
            Complex* c);
    }
}
