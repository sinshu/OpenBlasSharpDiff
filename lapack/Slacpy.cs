using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SLACPY copies all or part of a two-dimensional matrix A to another
        /// matrix B.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies the part of the matrix A to be copied to B.
        /// = &#39;U&#39;:      Upper triangular part
        /// = &#39;L&#39;:      Lower triangular part
        /// Otherwise:  All of the matrix A
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is REAL array, dimension (LDA,N).
        /// The m by n matrix A.  If UPLO = &#39;U&#39;, only the upper triangle
        /// or trapezoid is accessed; if UPLO = &#39;L&#39;, only the lower
        /// triangle or trapezoid is accessed.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="b">
        /// [out] B is REAL array, dimension (LDB,N).
        /// On exit, B = A in the locations specified by UPLO.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,M).
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_slacpy", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Slacpy(
            MatrixLayout matrixLayout,
            char uplo,
            int m,
            int n,
            float* a,
            int lda,
            float* b,
            int ldb);
    }
}
