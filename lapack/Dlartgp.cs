using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DLARTGP generates a plane rotation so that
        /// </para>
        /// <para>
        ///    [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
        ///    [ -SN  CS  ]     [ G ]     [ 0 ]
        /// </para>
        /// <para>
        /// This is a slower, more accurate version of the Level 1 BLAS routine DROTG,
        /// with the following other differences:
        ///    F and G are unchanged on return.
        ///    If G=0, then CS=(+/-)1 and SN=0.
        ///    If F=0 and (G .ne. 0), then CS=0 and SN=(+/-)1.
        /// </para>
        /// <para>
        /// The sign is chosen so that R &gt;= 0.
        /// </para>
        /// </summary>
        /// <param name="f">
        /// [in] F is DOUBLE PRECISION.
        /// The first component of vector to be rotated.
        /// </param>
        /// <param name="g">
        /// [in] G is DOUBLE PRECISION.
        /// The second component of vector to be rotated.
        /// </param>
        /// <param name="cs">
        /// [out] CS is DOUBLE PRECISION.
        /// The cosine of the rotation.
        /// </param>
        /// <param name="sn">
        /// [out] SN is DOUBLE PRECISION.
        /// The sine of the rotation.
        /// </param>
        /// <param name="r">
        /// [out] R is DOUBLE PRECISION.
        /// The nonzero component of the rotated vector.
        /// 
        /// This version has a few statements commented out for thread safety
        /// (machine parameters are computed on each entry). 10 feb 03, SJH.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dlartgp", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dlartgp(
            double f,
            double g,
            double* cs,
            double* sn,
            double* r);
    }
}
