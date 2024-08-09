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
        /// <param name="m">
        /// No description available.
        /// </param>
        /// <param name="n">
        /// No description available.
        /// </param>
        /// <param name="kl">
        /// No description available.
        /// </param>
        /// <param name="ku">
        /// No description available.
        /// </param>
        /// <param name="d">
        /// No description available.
        /// </param>
        /// <param name="a">
        /// No description available.
        /// </param>
        /// <param name="lda">
        /// No description available.
        /// </param>
        /// <param name="iseed">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zlagge", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zlagge(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int kl,
            int ku,
            double* d,
            Complex* a,
            int lda,
            int* iseed);
    }
}
