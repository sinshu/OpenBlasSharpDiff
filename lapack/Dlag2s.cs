﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DLAG2S converts a DOUBLE PRECISION matrix, A, to a SINGLE
        /// PRECISION matrix, SA.
        /// </para>
        /// <para>
        /// RMAX is the overflow for the SINGLE PRECISION arithmetic
        /// DLAG2S checks that all the entries of A are between -RMAX and
        /// RMAX. If not the conversion is aborted and a flag is raised.
        /// </para>
        /// <para>
        /// This is an auxiliary routine so there is no argument checking.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of lines of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the M-by-N coefficient matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="sa">
        /// [out] SA is REAL array, dimension (LDSA,N).
        /// On exit, if INFO=0, the M-by-N coefficient matrix SA; if
        /// INFO&gt;0, the content of SA is unspecified.
        /// </param>
        /// <param name="ldsa">
        /// [in] LDSA is INTEGER.
        /// The leading dimension of the array SA.  LDSA &gt;= max(1,M).
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// = 1:  an entry of the matrix A is greater than the SINGLE
        /// PRECISION overflow threshold, in this case, the content
        /// of SA in exit is unspecified.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dlag2s", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dlag2s(
            MatrixLayout matrixLayout,
            int m,
            int n,
            double* a,
            int lda,
            float* sa,
            int ldsa);
    }
}
