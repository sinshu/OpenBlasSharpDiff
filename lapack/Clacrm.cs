using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CLACRM performs a very simple matrix-matrix multiplication:
        ///          C := A * B,
        /// where A is M by N and complex; B is N by N and real;
        /// C is M by N and complex.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A and of the matrix C.
        /// M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns and rows of the matrix B and
        /// the number of columns of the matrix C.
        /// N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX array, dimension (LDA, N).
        /// On entry, A contains the M by N matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;=max(1,M).
        /// </param>
        /// <param name="b">
        /// [in] B is REAL array, dimension (LDB, N).
        /// On entry, B contains the N by N matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;=max(1,N).
        /// </param>
        /// <param name="c">
        /// [out] C is COMPLEX array, dimension (LDC, N).
        /// On exit, C contains the M by N matrix C.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C. LDC &gt;=max(1,N).
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_clacrm", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Clacrm(
            MatrixLayout matrixLayout,
            int m,
            int n,
            Complex32* a,
            int lda,
            float* b,
            int ldb,
            Complex32* c,
            int ldc);
    }
}
