using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    CONSTRUCT THE MODIFIED GIVENS TRANSFORMATION MATRIX H WHICH ZEROS
        ///    THE SECOND COMPONENT OF THE 2-VECTOR  (DSQRT(DD1)*DX1,DSQRT(DD2)*&gt;    DY2)**T.
        ///    WITH DPARAM(1)=DFLAG, H HAS ONE OF THE FOLLOWING FORMS..
        /// </para>
        /// <para>
        ///    DFLAG=-1.D0     DFLAG=0.D0        DFLAG=1.D0     DFLAG=-2.D0
        /// </para>
        /// <para>
        ///      (DH11  DH12)    (1.D0  DH12)    (DH11  1.D0)    (1.D0  0.D0)
        ///    H=(          )    (          )    (          )    (          )
        ///      (DH21  DH22),   (DH21  1.D0),   (-1.D0 DH22),   (0.D0  1.D0).
        ///    LOCATIONS 2-4 OF DPARAM CONTAIN DH11, DH21, DH12, AND DH22
        ///    RESPECTIVELY. (VALUES OF 1.D0, -1.D0, OR 0.D0 IMPLIED BY THE
        ///    VALUE OF DPARAM(1) ARE NOT STORED IN DPARAM.)
        /// </para>
        /// <para>
        ///    THE VALUES OF GAMSQ AND RGAMSQ SET IN THE DATA STATEMENT MAY BE
        ///    INEXACT.  THIS IS OK AS THEY ARE ONLY USED FOR TESTING THE SIZE
        ///    OF DD1 AND DD2.  ALL ACTUAL SCALING OF DATA IS DONE USING GAM.
        /// </para>
        /// </summary>
        /// <param name="d1">
        /// [in,out] DD1 is DOUBLE PRECISION.
        /// </param>
        /// <param name="d2">
        /// [in,out] DD2 is DOUBLE PRECISION.
        /// </param>
        /// <param name="b1">
        /// No description available.
        /// </param>
        /// <param name="b2">
        /// No description available.
        /// </param>
        /// <param name="p">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_drotmg", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Drotmg(
            double* d1,
            double* d2,
            double* b1,
            double b2,
            double* p);
    }
}
