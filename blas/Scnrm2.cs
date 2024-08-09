using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// SCNRM2 returns the euclidean norm of a vector via the function
        /// name, so that
        /// </para>
        /// <para>
        ///    SCNRM2 := sqrt( x**H*x )
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="x">
        /// [in] X is COMPLEX array, dimension (N).
        /// complex vector with N elements
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of X
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_scnrm2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe float Scnrm2(
            int n,
            void* x,
            int incx);
    }
}
