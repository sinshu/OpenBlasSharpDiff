using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SLARTGS generates a plane rotation designed to introduce a bulge in
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
        /// [in] X is REAL.
        /// The (1,1) entry of an upper bidiagonal matrix.
        /// </param>
        /// <param name="y">
        /// [in] Y is REAL.
        /// The (1,2) entry of an upper bidiagonal matrix.
        /// </param>
        /// <param name="sigma">
        /// [in] SIGMA is REAL.
        /// The shift.
        /// </param>
        /// <param name="cs">
        /// [out] CS is REAL.
        /// The cosine of the rotation.
        /// </param>
        /// <param name="sn">
        /// [out] SN is REAL.
        /// The sine of the rotation.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_slartgs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Slartgs(
            float x,
            float y,
            float sigma,
            float* cs,
            float* sn);
    }
}
