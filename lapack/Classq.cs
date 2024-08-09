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
        /// <param name="n">
        /// No description available.
        /// </param>
        /// <param name="x">
        /// No description available.
        /// </param>
        /// <param name="incx">
        /// No description available.
        /// </param>
        /// <param name="scale">
        /// No description available.
        /// </param>
        /// <param name="sumsq">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_classq", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Classq(
            int n,
            Complex32* x,
            int incx,
            float* scale,
            float* sumsq);
    }
}
