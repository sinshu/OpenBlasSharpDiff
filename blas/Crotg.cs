using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// CROTG determines a complex Givens rotation.
        /// </para>
        /// </summary>
        /// <param name="a">
        /// [in] CA is COMPLEX.
        /// </param>
        /// <param name="b">
        /// [in] CB is COMPLEX.
        /// </param>
        /// <param name="c">
        /// [out] C is REAL.
        /// </param>
        /// <param name="s">
        /// [out] S is COMPLEX.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_crotg", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Crotg(
            void* a,
            void* b,
            float* c,
            void* s);
    }
}
