using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CLACGV conjugates a complex vector of length N.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The length of the vector X.  N &gt;= 0.
        /// </param>
        /// <param name="x">
        /// [in,out] X is COMPLEX array, dimension.
        /// (1+(N-1)*abs(INCX))
        /// On entry, the vector of length N to be conjugated.
        /// On exit, X is overwritten with conjg(X).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// The spacing between successive elements of X.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_clacgv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Clacgv(
            int n,
            Complex32* x,
            int incx);
    }
}
