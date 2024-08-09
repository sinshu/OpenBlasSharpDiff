using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
        /// </para>
        /// <para>
        ///    (SX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF SX ARE IN
        ///    (SX**T)
        /// </para>
        /// <para>
        ///    SX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
        ///    LX = (-INCX)*N, AND SIMILARLY FOR SY USING USING LY AND INCY.
        ///    WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS..
        /// </para>
        /// <para>
        ///    SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0
        /// </para>
        /// <para>
        ///      (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)
        ///    H=(          )    (          )    (          )    (          )
        ///      (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0).
        ///    SEE  SROTMG FOR A DESCRIPTION OF DATA STORAGE IN SPARAM.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="x">
        /// [in,out] SX is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of SX
        /// </param>
        /// <param name="y">
        /// [in,out] SY is REAL array, dimension ( 1 + ( N - 1 )*abs( INCY ) ).
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// storage spacing between elements of SY
        /// </param>
        /// <param name="p">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_srotm", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Srotm(
            int n,
            float* x,
            int incx,
            float* y,
            int incy,
            float* p);
    }
}
