using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    ZROTG determines a double complex Givens rotation.
        /// </para>
        /// </summary>
        /// <param name="a">
        /// No description available.
        /// </param>
        /// <param name="b">
        /// No description available.
        /// </param>
        /// <param name="c">
        /// [out] C is DOUBLE PRECISION.
        /// </param>
        /// <param name="s">
        /// [out] S is COMPLEX*16.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_zrotg", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Zrotg(
            void* a,
            void* b,
            double* c,
            void* s);
    }
}
