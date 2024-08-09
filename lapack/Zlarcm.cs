using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZLARCM performs a very simple matrix-matrix multiplication:
        ///          C := A * B,
        /// where A is M by M and real; B is M by N and complex;
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
        /// [in] A is DOUBLE PRECISION array, dimension (LDA, M).
        /// On entry, A contains the M by M matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;=max(1,M).
        /// </param>
        /// <param name="b">
        /// [in] B is COMPLEX*16 array, dimension (LDB, N).
        /// On entry, B contains the M by N matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;=max(1,M).
        /// </param>
        /// <param name="c">
        /// [out] C is COMPLEX*16 array, dimension (LDC, N).
        /// On exit, C contains the M by N matrix C.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C. LDC &gt;=max(1,M).
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zlarcm", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zlarcm(
            MatrixLayout matrixLayout,
            int m,
            int n,
            double* a,
            int lda,
            Complex* b,
            int ldb,
            Complex* c,
            int ldc);
    }
}
