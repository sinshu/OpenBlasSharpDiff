using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DLARTGS generates a plane rotation designed to introduce a bulge in
        /// Golub-Reinsch-style implicit QR iteration for the bidiagonal SVD
        /// problem. X and Y are the top-row entries, and SIGMA is the shift.
        /// The computed CS and SN define a plane rotation satisfying
        /// </para>
        /// <para>
        ///    [  CS  SN  ]  .  [ X^2 - SIGMA ]  =  [ R ],
        ///    [ -SN  CS  ]     [    X * Y    ]     [ 0 ]
        /// </para>
        /// <para>
        /// with R nonnegative.  If X^2 - SIGMA and X * Y are 0, then the
        /// rotation is by PI/2.
        /// </para>
        /// </summary>
        /// <param name="x">
        /// [in] X is DOUBLE PRECISION.
        /// The (1,1) entry of an upper bidiagonal matrix.
        /// </param>
        /// <param name="y">
        /// [in] Y is DOUBLE PRECISION.
        /// The (1,2) entry of an upper bidiagonal matrix.
        /// </param>
        /// <param name="sigma">
        /// [in] SIGMA is DOUBLE PRECISION.
        /// The shift.
        /// </param>
        /// <param name="cs">
        /// [out] CS is DOUBLE PRECISION.
        /// The cosine of the rotation.
        /// </param>
        /// <param name="sn">
        /// [out] SN is DOUBLE PRECISION.
        /// The sine of the rotation.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dlartgs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dlartgs(
            double x,
            double y,
            double sigma,
            double* cs,
            double* sn);
    }
}
