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
        /// <param name="a">
        /// No description available.
        /// </param>
        /// <param name="lda">
        /// No description available.
        /// </param>
        /// <param name="jpvt">
        /// No description available.
        /// </param>
        /// <param name="tau">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sgeqpf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sgeqpf(
            MatrixLayout matrixLayout,
            int m,
            int n,
            float* a,
            int lda,
            int* jpvt,
            float* tau);
    }
}
