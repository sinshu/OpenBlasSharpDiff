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
        /// <param name="dist">
        /// No description available.
        /// </param>
        /// <param name="iseed">
        /// No description available.
        /// </param>
        /// <param name="sym">
        /// No description available.
        /// </param>
        /// <param name="d">
        /// No description available.
        /// </param>
        /// <param name="mode">
        /// No description available.
        /// </param>
        /// <param name="cond">
        /// No description available.
        /// </param>
        /// <param name="dmax">
        /// No description available.
        /// </param>
        /// <param name="kl">
        /// No description available.
        /// </param>
        /// <param name="ku">
        /// No description available.
        /// </param>
        /// <param name="pack">
        /// No description available.
        /// </param>
        /// <param name="a">
        /// No description available.
        /// </param>
        /// <param name="lda">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zlatms", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zlatms(
            MatrixLayout matrixLayout,
            int m,
            int n,
            char dist,
            int* iseed,
            char sym,
            double* d,
            int mode,
            double cond,
            double dmax,
            int kl,
            int ku,
            char pack,
            Complex* a,
            int lda);
    }
}
