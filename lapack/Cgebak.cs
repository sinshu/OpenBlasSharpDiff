using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGEBAK forms the right or left eigenvectors of a complex general
        /// matrix by backward transformation on the computed eigenvectors of the
        /// balanced matrix output by CGEBAL.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="job">
        /// [in] JOB is CHARACTER*1.
        /// Specifies the type of backward transformation required:
        /// = &#39;N&#39;: do nothing, return immediately;
        /// = &#39;P&#39;: do backward transformation for permutation only;
        /// = &#39;S&#39;: do backward transformation for scaling only;
        /// = &#39;B&#39;: do backward transformations for both permutation and
        /// scaling.
        /// JOB must be the same as the argument JOB supplied to CGEBAL.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// = &#39;R&#39;:  V contains right eigenvectors;
        /// = &#39;L&#39;:  V contains left eigenvectors.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of rows of the matrix V.  N &gt;= 0.
        /// </param>
        /// <param name="ilo">
        /// [in] ILO is INTEGER.
        /// </param>
        /// <param name="ihi">
        /// [in] IHI is INTEGER.
        /// The integers ILO and IHI determined by CGEBAL.
        /// 1 &lt;= ILO &lt;= IHI &lt;= N, if N &gt; 0; ILO=1 and IHI=0, if N=0.
        /// </param>
        /// <param name="scale">
        /// [in] SCALE is REAL array, dimension (N).
        /// Details of the permutation and scaling factors, as returned
        /// by CGEBAL.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of columns of the matrix V.  M &gt;= 0.
        /// </param>
        /// <param name="v">
        /// [in,out] V is COMPLEX array, dimension (LDV,M).
        /// On entry, the matrix of right or left eigenvectors to be
        /// transformed, as returned by CHSEIN or CTREVC.
        /// On exit, V is overwritten by the transformed eigenvectors.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V. LDV &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgebak", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgebak(
            MatrixLayout matrixLayout,
            char job,
            char side,
            int n,
            int ilo,
            int ihi,
            float* scale,
            int m,
            Complex32* v,
            int ldv);
    }
}
