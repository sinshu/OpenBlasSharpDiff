using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// DNRM2 returns the euclidean norm of a vector via the function
        /// name, so that
        /// </para>
        /// <para>
        ///    DNRM2 := sqrt( x&#39;*x )
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="x">
        /// [in] X is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of DX
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_dnrm2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe double Dnrm2(
            int n,
            double* x,
            int incx);
    }
}
