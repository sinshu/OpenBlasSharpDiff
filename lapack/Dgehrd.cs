using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGEHRD reduces a real general matrix A to upper Hessenberg form H by
        /// an orthogonal similarity transformation:  Q**T * A * Q = H .
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="ilo">
        /// [in] ILO is INTEGER.
        /// </param>
        /// <param name="ihi">
        /// [in] IHI is INTEGER.
        /// 
        /// It is assumed that A is already upper triangular in rows
        /// and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
        /// set by a previous call to DGEBAL; otherwise they should be
        /// set to 1 and N respectively. See Further Details.
        /// 1 &lt;= ILO &lt;= IHI &lt;= N, if N &gt; 0; ILO=1 and IHI=0, if N=0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the N-by-N general matrix to be reduced.
        /// On exit, the upper triangle and the first subdiagonal of A
        /// are overwritten with the upper Hessenberg matrix H, and the
        /// elements below the first subdiagonal, with the array TAU,
        /// represent the orthogonal matrix Q as a product of elementary
        /// reflectors. See Further Details.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="tau">
        /// [out] TAU is DOUBLE PRECISION array, dimension (N-1).
        /// The scalar factors of the elementary reflectors (see Further
        /// Details). Elements 1:ILO-1 and IHI:N-1 of TAU are set to
        /// zero.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The matrix Q is represented as a product of (ihi-ilo) elementary
        ///  reflectors
        /// </para>
        /// <para>
        ///     Q = H(ilo) H(ilo+1) . . . H(ihi-1).
        /// </para>
        /// <para>
        ///  Each H(i) has the form
        /// </para>
        /// <para>
        ///     H(i) = I - tau * v * v**T
        /// </para>
        /// <para>
        ///  where tau is a real scalar, and v is a real vector with
        ///  v(1:i) = 0, v(i+1) = 1 and v(ihi+1:n) = 0; v(i+2:ihi) is stored on
        ///  exit in A(i+2:ihi,i), and tau in TAU(i).
        /// </para>
        /// <para>
        ///  The contents of A are illustrated by the following example, with
        ///  n = 7, ilo = 2 and ihi = 6:
        /// </para>
        /// <para>
        ///  on entry,                        on exit,
        /// </para>
        /// <para>
        ///  ( a   a   a   a   a   a   a )    (  a   a   h   h   h   h   a )
        ///  (     a   a   a   a   a   a )    (      a   h   h   h   h   a )
        ///  (     a   a   a   a   a   a )    (      h   h   h   h   h   h )
        ///  (     a   a   a   a   a   a )    (      v2  h   h   h   h   h )
        ///  (     a   a   a   a   a   a )    (      v2  v3  h   h   h   h )
        ///  (     a   a   a   a   a   a )    (      v2  v3  v4  h   h   h )
        ///  (                         a )    (                          a )
        /// </para>
        /// <para>
        ///  where a denotes an element of the original matrix A, h denotes a
        ///  modified element of the upper Hessenberg matrix H, and vi denotes an
        ///  element of the vector defining H(i).
        /// </para>
        /// <para>
        ///  This file is a slight modification of LAPACK-3.0&#39;s DGEHRD
        ///  subroutine incorporating improvements proposed by Quintana-Orti and
        ///  Van de Geijn (2006). (See DLAHR2.)
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgehrd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgehrd(
            MatrixLayout matrixLayout,
            int n,
            int ilo,
            int ihi,
            double* a,
            int lda,
            double* tau);
    }
}
