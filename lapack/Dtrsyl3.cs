using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// No description available.
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="trana">
        /// No description available.
        /// </param>
        /// <param name="tranb">
        /// No description available.
        /// </param>
        /// <param name="isgn">
        /// No description available.
        /// </param>
        /// <param name="m">
        /// No description available.
        /// </param>
        /// <param name="n">
        /// No description available.
        /// </param>
        /// <param name="a">
        /// No description available.
        /// </param>
        /// <param name="lda">
        /// No description available.
        /// </param>
        /// <param name="b">
        /// No description available.
        /// </param>
        /// <param name="ldb">
        /// No description available.
        /// </param>
        /// <param name="c">
        /// No description available.
        /// </param>
        /// <param name="ldc">
        /// No description available.
        /// </param>
        /// <param name="scale">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dtrsyl3", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dtrsyl3(
            MatrixLayout matrixLayout,
            char trana,
            char tranb,
            int isgn,
            int m,
            int n,
            double* a,
            int lda,
            double* b,
            int ldb,
            double* c,
            int ldc,
            double* scale);
    }
}
