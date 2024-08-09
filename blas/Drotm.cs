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
        ///    (DX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF DX ARE IN
        ///    (DY**T)
        /// </para>
        /// <para>
        ///    DX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
        ///    LX = (-INCX)*N, AND SIMILARLY FOR SY USING LY AND INCY.
        ///    WITH DPARAM(1)=DFLAG, H HAS ONE OF THE FOLLOWING FORMS..
        /// </para>
        /// <para>
        ///    DFLAG=-1.D0     DFLAG=0.D0        DFLAG=1.D0     DFLAG=-2.D0
        /// </para>
        /// <para>
        ///      (DH11  DH12)    (1.D0  DH12)    (DH11  1.D0)    (1.D0  0.D0)
        ///    H=(          )    (          )    (          )    (          )
        ///      (DH21  DH22),   (DH21  1.D0),   (-1.D0 DH22),   (0.D0  1.D0).
        ///    SEE DROTMG FOR A DESCRIPTION OF DATA STORAGE IN DPARAM.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="x">
        /// [in,out] DX is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of DX
        /// </param>
        /// <param name="y">
        /// [in,out] DY is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCY ) ).
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// storage spacing between elements of DY
        /// </param>
        /// <param name="p">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_drotm", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Drotm(
            int n,
            double* x,
            int incx,
            double* y,
            int incy,
            double* p);
    }
}
