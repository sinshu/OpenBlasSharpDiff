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
        ///    THE SECOND COMPONENT OF THE 2-VECTOR  (SQRT(SD1)*SX1,SQRT(SD2)*&gt;    SY2)**T.
        ///    WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS..
        /// </para>
        /// <para>
        ///    SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0
        /// </para>
        /// <para>
        ///      (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)
        ///    H=(          )    (          )    (          )    (          )
        ///      (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0).
        ///    LOCATIONS 2-4 OF SPARAM CONTAIN SH11,SH21,SH12, AND SH22
        ///    RESPECTIVELY. (VALUES OF 1.E0, -1.E0, OR 0.E0 IMPLIED BY THE
        ///    VALUE OF SPARAM(1) ARE NOT STORED IN SPARAM.)
        /// </para>
        /// <para>
        ///    THE VALUES OF GAMSQ AND RGAMSQ SET IN THE DATA STATEMENT MAY BE
        ///    INEXACT.  THIS IS OK AS THEY ARE ONLY USED FOR TESTING THE SIZE
        ///    OF SD1 AND SD2.  ALL ACTUAL SCALING OF DATA IS DONE USING GAM.
        /// </para>
        /// </summary>
        /// <param name="d1">
        /// [in,out] SD1 is REAL.
        /// </param>
        /// <param name="d2">
        /// [in,out] SD2 is REAL.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_srotmg", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Srotmg(
            float* d1,
            float* d2,
            float* b1,
            float b2,
            float* p);
    }
}
